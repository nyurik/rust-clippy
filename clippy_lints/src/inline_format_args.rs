// #![allow(unused_imports)]
// #![allow(unused_variables)]

// use std::fs::{create_dir_all, OpenOptions};
// use std::io::Write;

use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::macros::{FormatArgsArg, FormatArgsExpn, is_format_macro, root_macro_call_first_node};
use clippy_utils::source::{expand_past_previous_comma, snippet};
use if_chain::if_chain;

use rustc_errors::Applicability;
use rustc_hir::{Expr, ExprKind, Path, QPath};
use rustc_hir::def::Res;
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::{declare_lint_pass, declare_tool_lint};
use rustc_span::ExpnData;

declare_clippy_lint! {
    /// ### What it does
    ///
    /// ### Why is this bad?
    ///
    /// ### Example
    /// ```rust
    /// // example code where clippy issues a warning
    /// ```
    /// Use instead:
    /// ```rust
    /// // example code which does not raise clippy warning
    /// ```
    #[clippy::version = "1.64.0"]
    pub INLINE_FORMAT_ARGS,
    nursery,
    "default lint description"
}
declare_lint_pass!(InlineFormatArgs => [INLINE_FORMAT_ARGS]);

impl<'tcx> LateLintPass<'tcx> for InlineFormatArgs {
    fn check_expr(&mut self, cx: &LateContext<'tcx>, expr: &'tcx Expr<'tcx>) {
        if_chain! {
            // TODO: are all these needed?  They wre mostly copy/pasted from other places.
            if let Some(outer_macro) = root_macro_call_first_node(cx, expr);
            if let Some(format_args) = FormatArgsExpn::find_nested(cx, expr, outer_macro.expn);
            if !format_args.format_string_span.from_expansion();
            let expr_expn_data = expr.span.ctxt().outer_expn_data();
            let outermost_expn_data = outermost_expn_data(expr_expn_data);
            if let Some(macro_def_id) = outermost_expn_data.macro_def_id;
            if is_format_macro(cx, macro_def_id);
            // TODO: do we need this?
            // if let rustc_span::ExpnKind::Macro(_, name) = outermost_expn_data.kind;
            if let Some(args) = format_args.args(cx);
            if !args.is_empty();
            then {
                let mut changes = None;
                for (i, arg) in args.iter().enumerate() {
                    if_chain! {
                        // If this condition is expensive, may want to move it to the end of this if chain?
                        if arg.argument_span.is_empty() || snippet(cx, arg.argument_span, "").trim_end().is_empty();
                        if let ExprKind::Path(QPath::Resolved(None, Path { span, res, segments })) = arg.value.kind;
                        if segments.len() == 1;
                        // TODO: do we need this?
                        if let Res::Local(_local_id) = res;
                        if !is_aliased(&args, i);
                        then {
                            let c = changes.get_or_insert_with(|| vec![]);
                            // TODO: is it ok to assume segments.len() == 1?
                            // if not, could use this instead:
                            //     let var_name = snippet(cx, *span, "").trim_end();
                            //     if var_name.is_empty() { continue; }
                            let var_name = segments[0].ident.name;
                            c.push((arg.argument_span, var_name.to_string()));
                            let arg_span = expand_past_previous_comma(cx, *span);
                            c.push((arg_span, "".to_string()));
                        }
                    }
                }

                if let Some(changes) = changes {
                    span_lint_and_then(
                        cx,
                        INLINE_FORMAT_ARGS,
                        outer_macro.span,
                        "REPLACE ME",
                        |diag| {
                            diag.multipart_suggestion(
                                &format!("some interesting message"),
                                changes,
                                Applicability::MachineApplicable,
                            );
                        },
                    );
                }

// let dumps_dir = "expected";
// create_dir_all(dumps_dir).unwrap();
// let full_str = snippet(cx, outer_macro.span, "panic").to_string()
//             .replace("/","--")
//             .replace("\t","[TAB]")
//             .replace("\n","[NL]")
//             .replace("\\","--");
// let filename = format!("{dumps_dir}/{full_str}.txt");
// let mut file = OpenOptions::new()
//         .write(true)
//         .create(true)
//             .truncate(true)
//     .open(&filename)
//     .unwrap();
// writeln!(file, "NAME {name:?}").unwrap();
// writeln!(file, "FMT {macro_def_id:?}").unwrap();
// for (i, part) in format_args.format_string_parts.iter().enumerate() {
//     writeln!(file, "FMT_PART {i} = {:?}", part).unwrap();
// }
// for (i, part) in format_args.formatters.iter().enumerate() {
//     writeln!(file, "FORMATTERS {i} = {:?}", part).unwrap();
// }
// for (i, part) in format_args.specs.iter().enumerate() {
//     writeln!(file, "SPECS {i} = {:#?}", part).unwrap();
// }
// for (i, arg) in args.iter().enumerate() {
//     writeln!(file, "#{i} = TRAIT={:?}, HAS_STRFMT={:?}, ALIAS={:?}, HAS_PRIM_FMT={:?}, SPAN={:?}, ARG_SPAN={:?}, SPEC={:#?}\nVALUE={:#?}",
//         arg.format_trait, arg.has_string_formatting(), is_aliased(&args, i), arg.has_primitive_formatting(),
//         snippet(cx, arg.span, "<<<..>>>"),
//         snippet(cx, arg.argument_span, "<<<..>>>"),
//         arg.spec, arg.value).unwrap();
// }
            }
        }
    }
}

// TODO: if this is a common pattern, should this be in utils?
fn outermost_expn_data(expn_data: ExpnData) -> ExpnData {
    if expn_data.call_site.from_expansion() {
        outermost_expn_data(expn_data.call_site.ctxt().outer_expn_data())
    } else {
        expn_data
    }
}

// Returns true if `args[i]` "refers to" or "is referred to by" another argument.
// TODO: this is not catching cases when the value is (also) used as precision or width
fn is_aliased(args: &[FormatArgsArg<'_>], i: usize) -> bool {
    let value = args[i].value;
    args.iter()
        .enumerate()
        .any(|(j, arg)| i != j && std::ptr::eq(value, arg.value))
}
