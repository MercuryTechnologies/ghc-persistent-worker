load("@prelude//genrule_toolchain.bzl", "GenruleToolchainInfo")

def nix_bash_genrule_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        GenruleToolchainInfo(bash = ctx.attrs.bash[RunInfo]),
    ]

nix_bash_genrule_toolchain = rule(
    impl = nix_bash_genrule_toolchain_impl,
    attrs = {
        "bash": attrs.dep(
            providers = [RunInfo],
            default = "//:bash",
        )
    },
    is_toolchain_rule = True,
)
