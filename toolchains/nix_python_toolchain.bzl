load("@prelude//python_bootstrap:python_bootstrap.bzl", "PythonBootstrapToolchainInfo")

def _nix_python_bootstrap_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        PythonBootstrapToolchainInfo(
            interpreter = ctx.attrs.python[RunInfo],
        ),
    ]

nix_python_bootstrap_toolchain = rule(
    impl = _nix_python_bootstrap_toolchain_impl,
    attrs = {
        "python": attrs.dep(
            providers = [RunInfo],
            default = "//:python",
        ),
    },
    is_toolchain_rule = True,
)
