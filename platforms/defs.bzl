load("@prelude//:build_mode.bzl", "BuildModeInfo")

def _local_execution_platform(ctx: AnalysisContext) -> list[Provider]:
    constraints = dict()
    constraints.update(ctx.attrs.cpu_configuration[ConfigurationInfo].constraints)
    constraints.update(ctx.attrs.os_configuration[ConfigurationInfo].constraints)
    cfg = ConfigurationInfo(constraints = constraints, values = {})

    name = ctx.label.raw_target()
    platform = ExecutionPlatformInfo(
        label = name,
        configuration = cfg,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            use_persistent_workers = True,
            remote_enabled = False,
            remote_cache_enabled = False,
            allow_cache_uploads = False,
            use_limited_hybrid = False,
        ),
    )

    return [
        DefaultInfo(),
        platform,
        PlatformInfo(label = str(name), configuration = cfg),
        ExecutionPlatformRegistrationInfo(platforms = [platform]),
    ]

local_execution_platform = rule(
    impl = _local_execution_platform,
    attrs = {
        "cpu_configuration": attrs.dep(providers = [ConfigurationInfo]),
        "os_configuration": attrs.dep(providers = [ConfigurationInfo]),
    },
)

def _remote_execution_platform(ctx: AnalysisContext) -> list[Provider]:
    constraints = dict()
    constraints.update(ctx.attrs.cpu_configuration[ConfigurationInfo].constraints)
    constraints.update(ctx.attrs.os_configuration[ConfigurationInfo].constraints)
    cfg = ConfigurationInfo(constraints = constraints, values = {})

    name = ctx.label.raw_target()
    platform = ExecutionPlatformInfo(
        label = name,
        configuration = cfg,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            #use_persistent_workers = True,
            remote_enabled = False,
            use_limited_hybrid = True,
            remote_execution_properties = {
                "OSFamily": host_info().os,
            },
            remote_execution_use_case = "buck2-default",
            remote_output_paths = "strict",
            allow_cache_uploads = True,
            remote_cache_enabled = True,
        ),
    )

    return [
        DefaultInfo(),
        platform,
        PlatformInfo(label = str(name), configuration = cfg),
        ExecutionPlatformRegistrationInfo(platforms = [platform]),
    ]

remote_execution_platform = rule(
    impl = _remote_execution_platform,
    attrs = {
        "cpu_configuration": attrs.dep(providers = [ConfigurationInfo]),
        "os_configuration": attrs.dep(providers = [ConfigurationInfo]),
    },
)

def _build_mode_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        BuildModeInfo(),
        DefaultInfo(),
    ]

build_mode = rule(
    impl = _build_mode_impl,
    attrs = {},
)
