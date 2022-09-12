use clippy_utils::diagnostics::{span_lint_and_sugg, span_lint_and_then};
use clippy_utils::is_diag_trait_item;
use clippy_utils::macros::{is_format_macro, FormatArgsExpn, FormatArg, FormatParamKind};
use clippy_utils::source::{expand_past_previous_comma, snippet, snippet_opt};
use clippy_utils::ty::implements_trait;
use if_chain::if_chain;
use itertools::Itertools;
use rustc_errors::Applicability;
use rustc_hir::{Expr, ExprKind, HirId, Path, QPath};
use rustc_lint::{LateContext, LateLintPass};
use rustc_middle::ty::adjustment::{Adjust, Adjustment};
use rustc_middle::ty::Ty;
use rustc_session::{declare_lint_pass, declare_tool_lint};
use rustc_span::{sym, ExpnData, ExpnKind, Span, Symbol};

declare_clippy_lint! {
    /// ### What it does
    /// Detects `format!` within the arguments of another macro that does
    /// formatting such as `format!` itself, `write!` or `println!`. Suggests
    /// inlining the `format!` call.
    ///
    /// ### Why is this bad?
    /// The recommended code is both shorter and avoids a temporary allocation.
    ///
    /// ### Example
    /// ```rust
    /// # use std::panic::Location;
    /// println!("error: {}", format!("something failed at {}", Location::caller()));
    /// ```
    /// Use instead:
    /// ```rust
    /// # use std::panic::Location;
    /// println!("error: something failed at {}", Location::caller());
    /// ```
    #[clippy::version = "1.58.0"]
    pub FORMAT_IN_FORMAT_ARGS,
    perf,
    "`format!` used in a macro that does formatting"
}

declare_clippy_lint! {
    /// ### What it does
    /// Checks for [`ToString::to_string`](https://doc.rust-lang.org/std/string/trait.ToString.html#tymethod.to_string)
    /// applied to a type that implements [`Display`](https://doc.rust-lang.org/std/fmt/trait.Display.html)
    /// in a macro that does formatting.
    ///
    /// ### Why is this bad?
    /// Since the type implements `Display`, the use of `to_string` is
    /// unnecessary.
    ///
    /// ### Example
    /// ```rust
    /// # use std::panic::Location;
    /// println!("error: something failed at {}", Location::caller().to_string());
    /// ```
    /// Use instead:
    /// ```rust
    /// # use std::panic::Location;
    /// println!("error: something failed at {}", Location::caller());
    /// ```
    #[clippy::version = "1.58.0"]
    pub TO_STRING_IN_FORMAT_ARGS,
    perf,
    "`to_string` applied to a type that implements `Display` in format args"
}

declare_clippy_lint! {
    /// ### What it does
    /// Detect when a variable is not inlined in a format string,
    /// and suggests to inline it.
    ///
    /// ### Why is this bad?
    /// Non-inlined code is slightly more difficult to read and understand,
    /// as it requires arguments to be matched against the format string.
    /// The inlined syntax, where allowed, is simpler.
    ///
    /// ### Example
    /// ```rust
    /// format!("{}", foo);
    /// ```
    /// Use instead:
    /// ```rust
    /// format!("{foo}");
    /// ```
    #[clippy::version = "1.64.0"]
    pub INLINE_FORMAT_ARGS,
    nursery,
    "using non-inlined variables in `format!` calls"
}

declare_lint_pass!(FormatArgs => [FORMAT_IN_FORMAT_ARGS, TO_STRING_IN_FORMAT_ARGS, INLINE_FORMAT_ARGS]);

impl<'tcx> LateLintPass<'tcx> for FormatArgs {
    fn check_expr(&mut self, cx: &LateContext<'tcx>, expr: &'tcx Expr<'tcx>) {
        if_chain! {
            if let Some(format_args) = FormatArgsExpn::parse(cx, expr);
            let expr_expn_data = expr.span.ctxt().outer_expn_data();
            let outermost_expn_data = outermost_expn_data(expr_expn_data);
            if let Some(macro_def_id) = outermost_expn_data.macro_def_id;
            if is_format_macro(cx, macro_def_id);
            if let ExpnKind::Macro(_, name) = outermost_expn_data.kind;
            then {
                // if at least some of the arguments/format/precision are referenced by an index,
                // e.g.  format!("{} {1}", foo, bar)  or  format!("{:1$}", foo, 2)
                // we cannot remove an argument from a list until we support renumbering
                let mut do_inline = !has_numbered(&format_args);
                let mut inline_spans = None;
                for arg in &format_args.args {

                    /////// DEBUG, REMOVE BEFORE MERGE ///////
                    // println!("ARG: {}\n{} -- span={}, value={}, precision={}, width={}, ty={}, trait={}\n{:#?}\n",
                    // println!("ARG: {}\n{} -- span={}, value={}, trait={}\n{:#?}\n",
                    //     snippet(cx, format_args.format_string.span, "(empty)"),
                    //     snippet(cx, arg.span, "(empty)"),
                    //     snippet(cx, arg.param.span, "(empty)"),
                    //     snippet(cx, arg.param.value.span, "(empty)"),
                    //     // snippet(cx, arg.param.precision_span.unwrap_or(DUMMY_SP), "(empty)"),
                    //     // snippet(cx, arg.param.width_span.unwrap_or(DUMMY_SP), "(empty)"),
                    //     // snippet(cx, arg.param.ty_span.unwrap_or(DUMMY_SP), "(empty)"),
                    //     snippet(cx, arg.format.trait_span.unwrap_or(rustc_span::DUMMY_SP), "(empty)"),
                    //     arg,
                    // );

                    if is_aliased(&format_args, arg.param.value.hir_id) {
                        do_inline = false;  // inlining doesn't support aliases (yet?)
                        continue;
                    }
                    if do_inline {
                        check_inline(cx, &arg, &mut inline_spans);
                    }
                    if !arg.format.is_default() {
                        continue;
                    }
                    check_format_in_format_args(cx, outermost_expn_data.call_site, name, arg.param.value);
                    check_to_string_in_format_args(cx, name, arg.param.value);
                }
                if do_inline && let Some(inline_spans) = inline_spans  {
                    suggest_inline(cx, outermost_expn_data.call_site, inline_spans);
                }
            }
        }
    }
}

fn suggest_inline(cx: &LateContext<'_>, expr_span: Span, changes: Vec<(Span, String)>) {
    span_lint_and_then(
        cx,
        INLINE_FORMAT_ARGS,
        expr_span,
        "variables can be used directly in the `format!` string",
        |diag| {
            diag.multipart_suggestion("change this to", changes, Applicability::MachineApplicable);
        },
    );
}

fn check_inline(
    cx: &LateContext<'_>,
    arg: &FormatArg<'_>,
    changes: &mut Option<Vec<(Span, String)>>,
) {
    if (arg.param.span.is_empty() || snippet(cx, arg.param.span, "").trim_end().is_empty())
        && let ExprKind::Path(QPath::Resolved(None, path)) = arg.param.value.kind
        && let Path { span, segments, .. } = path
        && let [segment] = segments
    {
            let c = changes.get_or_insert_with(Vec::new);
            c.push((arg.param.span, segment.ident.name.to_string()));
            let arg_span = expand_past_previous_comma(cx, *span);
            c.push((arg_span, "".to_string()));
    }
}

fn outermost_expn_data(expn_data: ExpnData) -> ExpnData {
    if expn_data.call_site.from_expansion() {
        outermost_expn_data(expn_data.call_site.ctxt().outer_expn_data())
    } else {
        expn_data
    }
}

fn check_format_in_format_args(cx: &LateContext<'_>, call_site: Span, name: Symbol, arg: &Expr<'_>) {
    let expn_data = arg.span.ctxt().outer_expn_data();
    if expn_data.call_site.from_expansion() {
        return;
    }
    let Some(mac_id) = expn_data.macro_def_id else { return };
    if !cx.tcx.is_diagnostic_item(sym::format_macro, mac_id) {
        return;
    }
    span_lint_and_then(
        cx,
        FORMAT_IN_FORMAT_ARGS,
        call_site,
        &format!("`format!` in `{}!` args", name),
        |diag| {
            diag.help(&format!(
                "combine the `format!(..)` arguments with the outer `{}!(..)` call",
                name
            ));
            diag.help("or consider changing `format!` to `format_args!`");
        },
    );
}

fn check_to_string_in_format_args(cx: &LateContext<'_>, name: Symbol, value: &Expr<'_>) {
    if_chain! {
        if !value.span.from_expansion();
        if let ExprKind::MethodCall(_, [receiver], _) = value.kind;
        if let Some(method_def_id) = cx.typeck_results().type_dependent_def_id(value.hir_id);
        if is_diag_trait_item(cx, method_def_id, sym::ToString);
        let receiver_ty = cx.typeck_results().expr_ty(receiver);
        if let Some(display_trait_id) = cx.tcx.get_diagnostic_item(sym::Display);
        let (n_needed_derefs, target) =
            count_needed_derefs(receiver_ty, cx.typeck_results().expr_adjustments(receiver).iter());
        if implements_trait(cx, target, display_trait_id, &[]);
        if let Some(sized_trait_id) = cx.tcx.lang_items().sized_trait();
        if let Some(receiver_snippet) = snippet_opt(cx, receiver.span);
        then {
            let needs_ref = !implements_trait(cx, receiver_ty, sized_trait_id, &[]);
            if n_needed_derefs == 0 && !needs_ref {
                span_lint_and_sugg(
                    cx,
                    TO_STRING_IN_FORMAT_ARGS,
                    value.span.with_lo(receiver.span.hi()),
                    &format!(
                        "`to_string` applied to a type that implements `Display` in `{}!` args",
                        name
                    ),
                    "remove this",
                    String::new(),
                    Applicability::MachineApplicable,
                );
            } else {
                span_lint_and_sugg(
                    cx,
                    TO_STRING_IN_FORMAT_ARGS,
                    value.span,
                    &format!(
                        "`to_string` applied to a type that implements `Display` in `{}!` args",
                        name
                    ),
                    "use this",
                    format!(
                        "{}{:*>width$}{}",
                        if needs_ref { "&" } else { "" },
                        "",
                        receiver_snippet,
                        width = n_needed_derefs
                    ),
                    Applicability::MachineApplicable,
                );
            }
        }
    }
}

/// Returns true if `hir_id` is referred to by multiple format params
fn is_aliased(args: &FormatArgsExpn<'_>, hir_id: HirId) -> bool {
    args.params()
        .filter(|param| param.value.hir_id == hir_id)
        .at_most_one()
        .is_err()
}

/// Returns true if the format string references any argument by its index (e.g. `{0}`, `{:1$}`, or `{:.2$}`)
fn has_numbered(args: &FormatArgsExpn<'_>) -> bool {
    // Note that CountIsStar is NOT a numbered case
    args.params().any(|param| param.kind == FormatParamKind::Numbered)
}

fn count_needed_derefs<'tcx, I>(mut ty: Ty<'tcx>, mut iter: I) -> (usize, Ty<'tcx>)
where
    I: Iterator<Item = &'tcx Adjustment<'tcx>>,
{
    let mut n_total = 0;
    let mut n_needed = 0;
    loop {
        if let Some(Adjustment {
            kind: Adjust::Deref(overloaded_deref),
            target,
        }) = iter.next()
        {
            n_total += 1;
            if overloaded_deref.is_some() {
                n_needed = n_total;
            }
            ty = *target;
        } else {
            return (n_needed, ty);
        }
    }
}
