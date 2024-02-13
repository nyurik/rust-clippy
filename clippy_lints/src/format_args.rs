use arrayvec::ArrayVec;
use clippy_config::msrvs::{self, Msrv};
use clippy_utils::diagnostics::{span_lint_and_sugg, span_lint_and_then};
use clippy_utils::is_diag_trait_item;
use clippy_utils::macros::{
    find_format_arg_expr, format_arg_removal_span, format_placeholder_format_span, is_assert_macro, is_format_macro,
    is_panic, matching_root_macro_call, root_macro_call_first_node, FormatArgsStorage, FormatParamUsage, MacroCall,
};
use clippy_utils::source::snippet_opt;
use clippy_utils::ty::{implements_trait, is_type_lang_item};
use itertools::Itertools;
use rustc_ast::token::LitKind;
use rustc_ast::{
    FormatArgPosition, FormatArgPositionKind, FormatArgsPiece, FormatArgumentKind, FormatCount, FormatOptions,
    FormatPlaceholder, FormatTrait,
};
use rustc_errors::Applicability;
use rustc_errors::SuggestionStyle::{CompletelyHidden, ShowCode};
use rustc_hir::{Expr, ExprKind, LangItem};
use rustc_lint::{LateContext, LateLintPass, LintContext};
use rustc_middle::ty::adjustment::{Adjust, Adjustment};
use rustc_middle::ty::Ty;
use rustc_session::impl_lint_pass;
use rustc_span::edition::Edition::Edition2021;
use rustc_span::{sym, Span, Symbol};

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
    /// ```no_run
    /// # use std::panic::Location;
    /// println!("error: {}", format!("something failed at {}", Location::caller()));
    /// ```
    /// Use instead:
    /// ```no_run
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
    /// ```no_run
    /// # use std::panic::Location;
    /// println!("error: something failed at {}", Location::caller().to_string());
    /// ```
    /// Use instead:
    /// ```no_run
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
    /// ```no_run
    /// # let var = 42;
    /// # let width = 1;
    /// # let prec = 2;
    /// format!("{}", var);
    /// format!("{v:?}", v = var);
    /// format!("{0} {0}", var);
    /// format!("{0:1$}", var, width);
    /// format!("{:.*}", prec, var);
    /// ```
    /// Use instead:
    /// ```no_run
    /// # let var = 42;
    /// # let width = 1;
    /// # let prec = 2;
    /// format!("{var}");
    /// format!("{var:?}");
    /// format!("{var} {var}");
    /// format!("{var:width$}");
    /// format!("{var:.prec$}");
    /// ```
    ///
    /// If allow-mixed-uninlined-format-args is set to false in clippy.toml,
    /// the following code will also trigger the lint:
    /// ```no_run
    /// # let var = 42;
    /// format!("{} {}", var, 1+2);
    /// ```
    /// Use instead:
    /// ```no_run
    /// # let var = 42;
    /// format!("{var} {}", 1+2);
    /// ```
    ///
    /// ### Known Problems
    ///
    /// If a format string contains a numbered argument that cannot be inlined
    /// nothing will be suggested, e.g. `println!("{0}={1}", var, 1+2)`.
    #[clippy::version = "1.66.0"]
    pub UNINLINED_FORMAT_ARGS,
    pedantic,
    "using non-inlined variables in `format!` calls"
}

declare_clippy_lint! {
    /// ### What it does
    /// Detects [formatting parameters] that have no effect on the output of
    /// `format!()`, `println!()` or similar macros.
    ///
    /// ### Why is this bad?
    /// Shorter format specifiers are easier to read, it may also indicate that
    /// an expected formatting operation such as adding padding isn't happening.
    ///
    /// ### Example
    /// ```no_run
    /// println!("{:.}", 1.0);
    ///
    /// println!("not padded: {:5}", format_args!("..."));
    /// ```
    /// Use instead:
    /// ```no_run
    /// println!("{}", 1.0);
    ///
    /// println!("not padded: {}", format_args!("..."));
    /// // OR
    /// println!("padded: {:5}", format!("..."));
    /// ```
    ///
    /// [formatting parameters]: https://doc.rust-lang.org/std/fmt/index.html#formatting-parameters
    #[clippy::version = "1.66.0"]
    pub UNUSED_FORMAT_SPECS,
    complexity,
    "use of a format specifier that has no effect"
}

declare_clippy_lint! {
    /// ### What it does
    /// This lint warns about the use of literals as `print!`/`println!` args.
    ///
    /// ### Why is this bad?
    /// Using literals as `println!` args is inefficient
    /// (c.f., https://github.com/matthiaskrgr/rust-str-bench) and unnecessary
    /// (i.e., just put the literal in the format string)
    ///
    /// ### Example
    /// ```no_run
    /// println!("{}", "foo");
    /// ```
    /// use the literal without formatting:
    /// ```no_run
    /// println!("foo");
    /// ```
    #[clippy::version = "pre 1.29.0"]
    pub PRINT_LITERAL,
    style,
    "printing a literal with a format string"
}

declare_clippy_lint! {
    /// ### What it does
    /// This lint warns about the use of literals as `write!`/`writeln!` args.
    ///
    /// ### Why is this bad?
    /// Using literals as `writeln!` args is inefficient
    /// (c.f., https://github.com/matthiaskrgr/rust-str-bench) and unnecessary
    /// (i.e., just put the literal in the format string)
    ///
    /// ### Example
    /// ```no_run
    /// # use std::fmt::Write;
    /// # let mut buf = String::new();
    /// writeln!(buf, "{}", "foo");
    /// ```
    ///
    /// Use instead:
    /// ```no_run
    /// # use std::fmt::Write;
    /// # let mut buf = String::new();
    /// writeln!(buf, "foo");
    /// ```
    #[clippy::version = "pre 1.29.0"]
    pub WRITE_LITERAL,
    style,
    "writing a literal with a format string"
}

declare_clippy_lint! {
    /// ### What it does
    /// This lint warns about the use of literals as `format!` args.
    ///
    /// ### Why is this bad?
    /// Using literals as `format!` args is inefficient
    /// (c.f., https://github.com/matthiaskrgr/rust-str-bench) and unnecessary
    /// (i.e., just put the literal in the format string)
    ///
    /// ### Example
    /// ```no_run
    /// # use std::fmt::Write;
    /// # let mut buf = String::new();
    /// # let var = 42;
    /// let fmt = format!("{} is {}", "var", var + 1);
    /// # println!("{fmt}");
    /// ```
    ///
    /// Use instead:
    /// ```no_run
    /// # use std::fmt::Write;
    /// # let mut buf = String::new();
    /// # let var = 42;
    /// let fmt = format!("var is {}", var + 1);
    /// # println!("{fmt}");
    /// ```
    #[clippy::version = "pre 1.29.0"]
    pub FORMAT_LITERAL,
    style,
    "format a literal with a format string"
}

impl_lint_pass!(FormatArgs => [
    FORMAT_IN_FORMAT_ARGS,
    FORMAT_LITERAL,
    PRINT_LITERAL,
    TO_STRING_IN_FORMAT_ARGS,
    UNINLINED_FORMAT_ARGS,
    UNUSED_FORMAT_SPECS,
    WRITE_LITERAL,
]);

#[allow(clippy::struct_field_names)]
pub struct FormatArgs {
    format_args: FormatArgsStorage,
    msrv: Msrv,
    ignore_mixed: bool,
    include_custom: bool,
}

impl FormatArgs {
    #[must_use]
    pub fn new(
        format_args: FormatArgsStorage,
        msrv: Msrv,
        allow_mixed_uninlined_format_args: bool,
        include_custom_format_macros: bool,
    ) -> Self {
        Self {
            format_args,
            msrv,
            ignore_mixed: allow_mixed_uninlined_format_args,
            include_custom: include_custom_format_macros,
        }
    }
}

impl<'tcx> LateLintPass<'tcx> for FormatArgs {
    fn check_expr(&mut self, cx: &LateContext<'tcx>, expr: &'tcx Expr<'tcx>) {
        if let Some(macro_call) = root_macro_call_first_node(cx, expr)
            && (self.include_custom || is_format_macro(cx, macro_call.def_id))
            && let Some(format_args) = self.format_args.get(cx, expr, macro_call.expn)
        {
            let linter = FormatArgsExpr {
                cx,
                expr,
                macro_call: &macro_call,
                format_args,
                ignore_mixed: self.ignore_mixed,
                item_name: cx.tcx.item_name(macro_call.def_id),
            };

            linter.check_each_arg();
            linter.check_literals();

            if self.msrv.meets(msrvs::FORMAT_ARGS_CAPTURE) {
                linter.check_uninlined_args();
            }
        }
    }

    extract_msrv_attr!(LateContext);
}

struct FormatArgsExpr<'a, 'tcx> {
    cx: &'a LateContext<'tcx>,
    expr: &'tcx Expr<'tcx>,
    macro_call: &'a MacroCall,
    format_args: &'a rustc_ast::FormatArgs,
    ignore_mixed: bool,
    item_name: Symbol,
}

impl<'a, 'tcx> FormatArgsExpr<'a, 'tcx> {
    fn check_each_arg(&self) {
        for piece in &self.format_args.template {
            if let FormatArgsPiece::Placeholder(placeholder) = piece
                && let Ok(index) = placeholder.argument.index
                && let Some(arg) = self.format_args.arguments.all_args().get(index)
            {
                let arg_expr = find_format_arg_expr(self.expr, arg);

                self.check_unused_format_specifier(placeholder, arg_expr);

                if let Ok(arg_expr) = arg_expr
                    && placeholder.format_trait == FormatTrait::Display
                    && placeholder.format_options == FormatOptions::default()
                    && !self.is_aliased(index)
                {
                    self.check_format_in_format_args(arg_expr);
                    self.check_to_string_in_format_args(arg_expr);
                }
            }
        }
    }

    fn check_unused_format_specifier(
        &self,
        placeholder: &FormatPlaceholder,
        arg_expr: Result<&Expr<'_>, &rustc_ast::Expr>,
    ) {
        let ty_or_ast_expr = arg_expr.map(|expr| self.cx.typeck_results().expr_ty(expr).peel_refs());

        let is_format_args = match ty_or_ast_expr {
            Ok(ty) => is_type_lang_item(self.cx, ty, LangItem::FormatArguments),
            Err(expr) => matches!(expr.peel_parens_and_refs().kind, rustc_ast::ExprKind::FormatArgs(_)),
        };

        let options = &placeholder.format_options;

        let arg_span = match arg_expr {
            Ok(expr) => expr.span,
            Err(expr) => expr.span,
        };

        if let Some(placeholder_span) = placeholder.span
            && is_format_args
            && *options != FormatOptions::default()
        {
            span_lint_and_then(
                self.cx,
                UNUSED_FORMAT_SPECS,
                placeholder_span,
                "format specifiers have no effect on `format_args!()`",
                |diag| {
                    let mut suggest_format = |spec| {
                        let message = format!("for the {spec} to apply consider using `format!()`");

                        if let Some(mac_call) = matching_root_macro_call(self.cx, arg_span, sym::format_args_macro) {
                            diag.span_suggestion(
                                self.cx.sess().source_map().span_until_char(mac_call.span, '!'),
                                message,
                                "format",
                                Applicability::MaybeIncorrect,
                            );
                        } else {
                            diag.help(message);
                        }
                    };

                    if options.width.is_some() {
                        suggest_format("width");
                    }

                    if options.precision.is_some() {
                        suggest_format("precision");
                    }

                    if let Some(format_span) = format_placeholder_format_span(placeholder) {
                        diag.span_suggestion_verbose(
                            format_span,
                            "if the current behavior is intentional, remove the format specifiers",
                            "",
                            Applicability::MaybeIncorrect,
                        );
                    }
                },
            );
        }
    }

    fn check_uninlined_args(&self) {
        if self.format_args.span.from_expansion() {
            return;
        }
        if self.macro_call.span.edition() < Edition2021
            && (is_panic(self.cx, self.macro_call.def_id) || is_assert_macro(self.cx, self.macro_call.def_id))
        {
            // panic!, assert!, and debug_assert! before 2021 edition considers a single string argument as
            // non-format
            return;
        }

        let mut fixes = Vec::new();
        // If any of the arguments are referenced by an index number,
        // and that argument is not a simple variable and cannot be inlined,
        // we cannot remove any other arguments in the format string,
        // because the index numbers might be wrong after inlining.
        // Example of an un-inlinable format:  print!("{}{1}", foo, 2)
        for (pos, usage) in self.format_arg_positions() {
            if !self.check_one_arg(pos, usage, &mut fixes) {
                return;
            }
        }

        if fixes.is_empty() {
            return;
        }

        // multiline span display suggestion is sometimes broken: https://github.com/rust-lang/rust/pull/102729#discussion_r988704308
        // in those cases, make the code suggestion hidden
        let multiline_fix = fixes
            .iter()
            .any(|(span, _)| self.cx.sess().source_map().is_multiline(*span));

        // Suggest removing each argument only once, for example in `format!("{0} {0}", arg)`.
        fixes.sort_unstable_by_key(|(span, _)| *span);
        fixes.dedup_by_key(|(span, _)| *span);

        span_lint_and_then(
            self.cx,
            UNINLINED_FORMAT_ARGS,
            self.macro_call.span,
            "variables can be used directly in the `format!` string",
            |diag| {
                diag.multipart_suggestion_with_style(
                    "change this to",
                    fixes,
                    Applicability::MachineApplicable,
                    if multiline_fix { CompletelyHidden } else { ShowCode },
                );
            },
        );
    }

    fn check_one_arg(&self, pos: &FormatArgPosition, usage: FormatParamUsage, fixes: &mut Vec<(Span, String)>) -> bool {
        let index = pos.index.unwrap();
        let arg = &self.format_args.arguments.all_args()[index];

        if !matches!(arg.kind, FormatArgumentKind::Captured(_))
            && let rustc_ast::ExprKind::Path(None, path) = &arg.expr.kind
            && let [segment] = path.segments.as_slice()
            && segment.args.is_none()
            && let Some(arg_span) = format_arg_removal_span(self.format_args, index)
            && let Some(pos_span) = pos.span
        {
            let replacement = match usage {
                FormatParamUsage::Argument => segment.ident.name.to_string(),
                FormatParamUsage::Width => format!("{}$", segment.ident.name),
                FormatParamUsage::Precision => format!(".{}$", segment.ident.name),
            };
            fixes.push((pos_span, replacement));
            fixes.push((arg_span, String::new()));
            true // successful inlining, continue checking
        } else {
            // Do not continue inlining (return false) in case
            // * if we can't inline a numbered argument, e.g. `print!("{0} ...", foo.bar, ...)`
            // * if allow_mixed_uninlined_format_args is false and this arg hasn't been inlined already
            pos.kind != FormatArgPositionKind::Number
                && (!self.ignore_mixed || matches!(arg.kind, FormatArgumentKind::Captured(_)))
        }
    }

    fn check_format_in_format_args(&self, arg: &Expr<'_>) {
        let expn_data = arg.span.ctxt().outer_expn_data();
        if expn_data.call_site.from_expansion() {
            return;
        }
        let Some(mac_id) = expn_data.macro_def_id else { return };
        if !self.cx.tcx.is_diagnostic_item(sym::format_macro, mac_id) {
            return;
        }
        span_lint_and_then(
            self.cx,
            FORMAT_IN_FORMAT_ARGS,
            self.macro_call.span,
            format!("`format!` in `{}!` args", self.item_name),
            |diag| {
                diag.help(format!(
                    "combine the `format!(..)` arguments with the outer `{}!(..)` call",
                    self.item_name
                ));
                diag.help("or consider changing `format!` to `format_args!`");
            },
        );
    }

    fn check_to_string_in_format_args(&self, value: &Expr<'_>) {
        let cx = self.cx;
        if !value.span.from_expansion()
            && let ExprKind::MethodCall(_, receiver, [], to_string_span) = value.kind
            && let Some(method_def_id) = cx.typeck_results().type_dependent_def_id(value.hir_id)
            && is_diag_trait_item(cx, method_def_id, sym::ToString)
            && let receiver_ty = cx.typeck_results().expr_ty(receiver)
            && let Some(display_trait_id) = cx.tcx.get_diagnostic_item(sym::Display)
            && let (n_needed_derefs, target) =
                count_needed_derefs(receiver_ty, cx.typeck_results().expr_adjustments(receiver).iter())
            && implements_trait(cx, target, display_trait_id, &[])
            && let Some(sized_trait_id) = cx.tcx.lang_items().sized_trait()
            && let Some(receiver_snippet) = snippet_opt(cx, receiver.span)
        {
            let needs_ref = !implements_trait(cx, receiver_ty, sized_trait_id, &[]);
            if n_needed_derefs == 0 && !needs_ref {
                span_lint_and_sugg(
                    cx,
                    TO_STRING_IN_FORMAT_ARGS,
                    to_string_span.with_lo(receiver.span.hi()),
                    format!(
                        "`to_string` applied to a type that implements `Display` in `{}!` args",
                        self.item_name
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
                    format!(
                        "`to_string` applied to a type that implements `Display` in `{}!` args",
                        self.item_name
                    ),
                    "use this",
                    format!(
                        "{}{:*>n_needed_derefs$}{receiver_snippet}",
                        if needs_ref { "&" } else { "" },
                        ""
                    ),
                    Applicability::MachineApplicable,
                );
            }
        }
    }

    fn format_arg_positions(&self) -> impl Iterator<Item = (&FormatArgPosition, FormatParamUsage)> {
        self.format_args.template.iter().flat_map(|piece| match piece {
            FormatArgsPiece::Placeholder(placeholder) => {
                let mut positions = ArrayVec::<_, 3>::new();

                positions.push((&placeholder.argument, FormatParamUsage::Argument));
                if let Some(FormatCount::Argument(position)) = &placeholder.format_options.width {
                    positions.push((position, FormatParamUsage::Width));
                }
                if let Some(FormatCount::Argument(position)) = &placeholder.format_options.precision {
                    positions.push((position, FormatParamUsage::Precision));
                }

                positions
            },
            FormatArgsPiece::Literal(_) => ArrayVec::new(),
        })
    }

    /// Returns true if the format argument at `index` is referred to by multiple format params
    fn is_aliased(&self, index: usize) -> bool {
        self.format_arg_positions()
            .filter(|(position, _)| position.index == Ok(index))
            .at_most_one()
            .is_err()
    }

    /// Detect `PRINT_LITERAL`, `WRITE_LITERAL`, `FORMAT_LITERAL` -- literals in format strings
    ///   - `format!("{}", "a")` -> `format!("a")`
    fn check_literals(&self) {
        let arg_index = |argument: &FormatArgPosition| argument.index.unwrap_or_else(|pos| pos);

        let mut counts = vec![0u32; self.format_args.arguments.all_args().len()];
        for piece in &self.format_args.template {
            if let FormatArgsPiece::Placeholder(placeholder) = piece {
                counts[arg_index(&placeholder.argument)] += 1;
            }
        }

        let mut suggestion: Vec<(Span, String)> = vec![];
        // holds index of replaced positional arguments; used to decrement the index of the remaining
        // positional arguments.
        let mut replaced_position: Vec<usize> = vec![];
        let mut sug_span: Option<Span> = None;

        for piece in &self.format_args.template {
            if let FormatArgsPiece::Placeholder(FormatPlaceholder {
                argument,
                span: Some(placeholder_span),
                format_trait: FormatTrait::Display,
                format_options,
            }) = piece
                && *format_options == FormatOptions::default()
                && let index = arg_index(argument)
                && counts[index] == 1
                && let Some(arg) = self.format_args.arguments.by_index(index)
                && let rustc_ast::ExprKind::Lit(lit) = &arg.expr.kind
                && !arg.expr.span.from_expansion()
                && let Some(value_string) = snippet_opt(self.cx, arg.expr.span)
            {
                let (replacement, replace_raw) = match lit.kind {
                    LitKind::Str | LitKind::StrRaw(_) => match extract_str_literal(&value_string) {
                        Some(extracted) => extracted,
                        None => return,
                    },
                    LitKind::Char => (
                        match lit.symbol.as_str() {
                            "\"" => "\\\"",
                            "\\'" => "'",
                            _ => match value_string.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')) {
                                Some(stripped) => stripped,
                                None => return,
                            },
                        }
                        .to_string(),
                        false,
                    ),
                    LitKind::Bool => (lit.symbol.to_string(), false),
                    _ => continue,
                };

                let Some(format_string_snippet) = snippet_opt(self.cx, self.format_args.span) else {
                    continue;
                };
                let format_string_is_raw = format_string_snippet.starts_with('r');

                let replacement = match (format_string_is_raw, replace_raw) {
                    (false, false) => Some(replacement),
                    (false, true) => Some(replacement.replace('"', "\\\"").replace('\\', "\\\\")),
                    (true, false) => match conservative_unescape(&replacement) {
                        Ok(unescaped) => Some(unescaped),
                        Err(UnescapeErr::Lint) => None,
                        Err(UnescapeErr::Ignore) => continue,
                    },
                    (true, true) => {
                        if replacement.contains(['#', '"']) {
                            None
                        } else {
                            Some(replacement)
                        }
                    },
                };

                sug_span = Some(sug_span.unwrap_or(arg.expr.span).to(arg.expr.span));

                if let Some((_, index)) = positional_arg_piece_span(piece) {
                    replaced_position.push(index);
                }

                if let Some(replacement) = replacement
                    // `format!("{}", "a")`, `format!("{named}", named = "b")
                    //              ~~~~~                      ~~~~~~~~~~~~~
                    && let Some(removal_span) = format_arg_removal_span(self.format_args, index)
                {
                    let replacement = escape_braces(&replacement, !format_string_is_raw && !replace_raw);
                    suggestion.push((*placeholder_span, replacement));
                    suggestion.push((removal_span, String::new()));
                }
            }
        }

        // Decrement the index of the remaining by the number of replaced positional arguments
        if !suggestion.is_empty() {
            for piece in &self.format_args.template {
                if let Some((span, index)) = positional_arg_piece_span(piece)
                    && suggestion.iter().all(|(s, _)| *s != span)
                {
                    let decrement = replaced_position.iter().filter(|i| **i < index).count();
                    suggestion.push((span, format!("{{{}}}", index.saturating_sub(decrement))));
                }
            }
        }

        if let Some(span) = sug_span {
            let lint_name = match self.item_name {
                sym::print_macro | sym::eprint_macro | sym::println_macro | sym::eprintln_macro => PRINT_LITERAL,
                sym::write_macro | sym::writeln_macro => WRITE_LITERAL,
                _ => FORMAT_LITERAL,
            };

            span_lint_and_then(
                self.cx,
                lint_name,
                span,
                "literal with an empty format string",
                |diag| {
                    if !suggestion.is_empty() {
                        diag.multipart_suggestion("try", suggestion, Applicability::MachineApplicable);
                    }
                },
            );
        }
    }
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

/// Extract Span and its index from the given `piece`, iff it's positional argument.
fn positional_arg_piece_span(piece: &FormatArgsPiece) -> Option<(Span, usize)> {
    match piece {
        FormatArgsPiece::Placeholder(FormatPlaceholder {
            argument:
                FormatArgPosition {
                    index: Ok(index),
                    kind: FormatArgPositionKind::Number,
                    ..
                },
            span: Some(span),
            ..
        }) => Some((*span, *index)),
        _ => None,
    }
}
/// Replaces `{` with `{{` and `}` with `}}`. If `preserve_unicode_escapes` is `true` the braces in
/// `\u{xxxx}` are left unmodified
#[expect(clippy::match_same_arms)]
fn escape_braces(literal: &str, preserve_unicode_escapes: bool) -> String {
    #[derive(Clone, Copy)]
    enum State {
        Normal,
        Backslash,
        UnicodeEscape,
    }

    let mut escaped = String::with_capacity(literal.len());
    let mut state = State::Normal;

    for ch in literal.chars() {
        state = match (ch, state) {
            // Escape braces outside of unicode escapes by doubling them up
            ('{' | '}', State::Normal) => {
                escaped.push(ch);
                State::Normal
            },
            // If `preserve_unicode_escapes` isn't enabled stay in `State::Normal`, otherwise:
            //
            // \u{aaaa} \\ \x01
            // ^        ^  ^
            ('\\', State::Normal) if preserve_unicode_escapes => State::Backslash,
            // \u{aaaa}
            //  ^
            ('u', State::Backslash) => State::UnicodeEscape,
            // \xAA \\
            //  ^    ^
            (_, State::Backslash) => State::Normal,
            // \u{aaaa}
            //        ^
            ('}', State::UnicodeEscape) => State::Normal,
            _ => state,
        };

        escaped.push(ch);
    }

    escaped
}

/// Removes the raw marker, `#`s and quotes from a str, and returns if the literal is raw
///
/// `r#"a"#` -> (`a`, true)
///
/// `"b"` -> (`b`, false)
fn extract_str_literal(literal: &str) -> Option<(String, bool)> {
    let (literal, raw) = match literal.strip_prefix('r') {
        Some(stripped) => (stripped.trim_matches('#'), true),
        None => (literal, false),
    };

    Some((literal.strip_prefix('"')?.strip_suffix('"')?.to_string(), raw))
}

enum UnescapeErr {
    /// Should still be linted, can be manually resolved by author, e.g.
    ///
    /// ```ignore
    /// print!(r"{}", '"');
    /// ```
    Lint,
    /// Should not be linted, e.g.
    ///
    /// ```ignore
    /// print!(r"{}", '\r');
    /// ```
    Ignore,
}

/// Unescape a normal string into a raw string
fn conservative_unescape(literal: &str) -> Result<String, UnescapeErr> {
    let mut unescaped = String::with_capacity(literal.len());
    let mut chars = literal.chars();
    let mut err = false;

    while let Some(ch) = chars.next() {
        match ch {
            '#' => err = true,
            '\\' => match chars.next() {
                Some('\\') => unescaped.push('\\'),
                Some('"') => err = true,
                _ => return Err(UnescapeErr::Ignore),
            },
            _ => unescaped.push(ch),
        }
    }

    if err { Err(UnescapeErr::Lint) } else { Ok(unescaped) }
}
