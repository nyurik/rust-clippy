use clippy_utils::diagnostics::span_lint_and_then;
use clippy_utils::macros::{is_aliased_format_arg, is_format_macro, root_macro_call_first_node, FormatArgsExpn};
use clippy_utils::source::{expand_past_previous_comma, snippet};

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
    "inline variables in `format!` calls"
}
declare_lint_pass!(InlineFormatArgs => [INLINE_FORMAT_ARGS]);

impl<'tcx> LateLintPass<'tcx> for InlineFormatArgs {
    fn check_expr(&mut self, cx: &LateContext<'tcx>, expr: &'tcx Expr<'tcx>) {
        if let Some(outer_macro) = root_macro_call_first_node(cx, expr)
            && let Some(format_args) = FormatArgsExpn::find_nested(cx, expr, outer_macro.expn)
            && !format_args.format_string_span.from_expansion()
            && is_format_macro(cx, outer_macro.def_id)
            && let Some(args) = format_args.args(cx)
            && !args.is_empty()
        {
            let mut changes = None;
            for (i, arg) in args.iter().enumerate() {
                // TODO: If this condition is expensive, may want to move it to the end of this if chain?
                if (arg.argument_span.is_empty() || snippet(cx, arg.argument_span, "").trim_end().is_empty())
                    && let ExprKind::Path(QPath::Resolved(None, path)) = arg.value.kind
                    && let Path { span, segments, .. } = path
                    && let [segment] = segments
                    && !is_aliased_format_arg(&args, i)
                {
                    // TODO: in the future, is_aliased_format_arg should take care of this.
                    // TODO: Better yet, FormatArgsExpn should parse all components, and we expand them,
                    // but that may require code reuse from rustc format handling.
                    // This check cancels the entire format, not just the current argument
                    if snippet(cx, arg.span, "").contains('$') {
                        return;
                    }

                    let c = changes.get_or_insert_with(Vec::new);
                    c.push((arg.argument_span, segment.ident.name.to_string()));
                    let arg_span = expand_past_previous_comma(cx, *span);
                    c.push((arg_span, "".to_string()));
                }
            }

            if let Some(changes) = changes {
                span_lint_and_then(
                    cx,
                    INLINE_FORMAT_ARGS,
                    outer_macro.span,
                    "variables can be used directly in the `format!` string",
                    |diag| {
                        diag.multipart_suggestion(
                            "change this to",
                            changes,
                            Applicability::MachineApplicable,
                        );
                    },
                );
            }
        }
    }
}
