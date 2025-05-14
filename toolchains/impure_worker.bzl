def _impure_worker_impl(ctx: AnalysisContext) -> list[Provider]:
    worker = cmd_args(ctx.attrs.binary_path)
    run_info = RunInfo(args = worker)
    return [DefaultInfo(), run_info]

impure_worker = rule(
    impl = _impure_worker_impl,
    attrs = {
        "binary_path": attrs.string(default = "should_not_be_used"),
    },
)
