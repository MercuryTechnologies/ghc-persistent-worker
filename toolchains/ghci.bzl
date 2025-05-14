load(
    "@prelude//haskell:toolchain.bzl",
    "HaskellToolchainInfo",
)
load("@prelude//decls/toolchains_common.bzl", "toolchains_common")

def _ghci_impl(ctx: AnalysisContext) -> list[Provider]:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    out = ctx.actions.write(
        "ghci",
        [
            "#!/usr/bin/env bash",
            cmd_args(haskell_toolchain.compiler, format = """exec {} --interactive "$@" """),
        ],
        is_executable = True,
    )
    return [
        DefaultInfo(out),
        RunInfo(cmd_args(out, hidden=[haskell_toolchain.compiler])),
    ]

ghci = rule(
    impl = _ghci_impl,
    attrs = {
        "_haskell_toolchain": toolchains_common.haskell(),
    },
)
