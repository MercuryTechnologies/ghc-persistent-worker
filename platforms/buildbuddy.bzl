load("@prelude//decls:common.bzl", "buck")

def _exec_platform_impl(ctx: AnalysisContext) -> list[Provider]:
    constraints = {}
    constraints.update(ctx.attrs.cpu_configuration[ConfigurationInfo].constraints)
    constraints.update(ctx.attrs.os_configuration[ConfigurationInfo].constraints)

    configuration = ConfigurationInfo(constraints = constraints, values = {})

    name = ctx.label.raw_target()
    exec_platform = ExecutionPlatformInfo(
        label = name,
        configuration = configuration,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = False,
            use_limited_hybrid = True,
            remote_execution_properties = {
                "OSFamily": host_info().os,
            },
            remote_execution_use_case = "buck2-default",
            remote_output_paths = "output_paths",
            allow_cache_uploads = True,
            remote_cache_enabled = True,
        ),
    )
    return [
        exec_platform,
        DefaultInfo(),
        PlatformInfo(label = str(name), configuration = configuration),
    ]

exec_platform = rule(
    impl = _exec_platform_impl,
    attrs = {
        "allow_cache_uploads": attrs.bool(default = True),
        "local_enabled": attrs.bool(),
        "remote_enabled": attrs.bool(default = True),
        "remote_cache_enabled": attrs.bool(default = True),
        "os_configuration": attrs.dep(
            providers = [ConfigurationInfo],
        ),
        "cpu_configuration": attrs.dep(
            providers = [ConfigurationInfo],
        ),
        "remote_execution_properties": attrs.dict(
            attrs.string(),
            attrs.one_of(attrs.string(), attrs.bool()),
        ),
    },
)

def _exec_platforms_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(platforms = [p[ExecutionPlatformInfo] for p in ctx.attrs.platforms]),
    ]

exec_platforms = rule(
    impl = _exec_platforms_impl,
    attrs = {
        "platforms": attrs.list(attrs.dep(providers = [ExecutionPlatformInfo])),
    },
)
