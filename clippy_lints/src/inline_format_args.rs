// #![allow(unused_imports)]
// #![allow(unused_variables)]

// use std::fs::{create_dir_all, OpenOptions};
// use std::io::Write;

use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::macros::{is_aliased_format_arg, is_format_macro, root_macro_call_first_node, FormatArgsExpn};
use clippy_utils::source::{expand_past_previous_comma, snippet};
use if_chain::if_chain;

use rustc_errors::Applicability;
use rustc_hir::{Expr, ExprKind, Path, QPath};
use rustc_lint::{LateContext, LateLintPass};
use rustc_session::{declare_lint_pass, declare_tool_lint};

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
                    if let Some(outer_macro) = root_macro_call_first_node(cx, expr);
                    if let Some(format_args) = FormatArgsExpn::find_nested(cx, expr, outer_macro.expn);
                    if !format_args.format_string_span.from_expansion();
                    if is_format_macro(cx, outer_macro.def_id);
                    // TODO: do we need this?
                    // if let rustc_span::ExpnKind::Macro(_, name) = outermost_expn_data.kind;
                    if let Some(args) = format_args.args(cx);
                    if !args.is_empty();
                    then {
                        let mut changes = None;
                        for (i, arg) in args.iter().enumerate() {
                            if_chain! {
                                // TODO: If this condition is expensive, may want to move it to the end of this if chain?
                                if arg.argument_span.is_empty() || snippet(cx, arg.argument_span, "").trim_end().is_empty();
                                if let ExprKind::Path(QPath::Resolved(None, path)) = arg.value.kind;
                                if let Path { span, segments, .. } = path;
                                if let [segment] = segments;
                                if !is_aliased_format_arg(&args, i);
                                then {
                                    let c = changes.get_or_insert_with(|| vec![]);
                                    let var_name = segment.ident.name;
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
        //         arg.format_trait, arg.has_string_formatting(), is_aliased_format_arg(&args, i), arg.has_primitive_formatting(),
        //         snippet(cx, arg.span, "<<<..>>>"),
        //         snippet(cx, arg.argument_span, "<<<..>>>"),
        //         arg.spec, arg.value).unwrap();
        // }
                    }
                }
    }
}
