load("//haskell:defs.bzl", "default_ghc_flags")

def mercury_haskell_library(
        name,
        srcs = [],
        extra_compiler_flags = [],
        **kwargs):
    """haskell_library wrapper with Mercury-specific defaults and checks

    Args:
      srcs: List of hs srcs to forward to haskell_library and to check with hlint etc
      extra_compiler_flags: Additional compiler flags to add to default_ghc_flags
    """

    _hlints(srcs)

    native.haskell_library(
        name = name,
        srcs = srcs,
        compiler_flags = default_ghc_flags + extra_compiler_flags,
        **kwargs
    )

def mercury_haskell_binary(
        name,
        srcs = [],
        extra_compiler_flags = [],
        **kwargs):
    """haskell_binary wrapper with Mercury-specific defaults and checks

    Args:
      srcs: List of hs srcs to forward to haskell_binary and to check with hlint etc
      extra_compiler_flags: Additional compiler flags to add to default_ghc_flags
    """

    _hlints(srcs)

    native.haskell_binary(
        name = name,
        srcs = srcs,
        compiler_flags = default_ghc_flags + extra_compiler_flags,
        **kwargs
    )

def _hlints(srcs):
    # Filter to avoid complexities from target srcs
    for src in filter(lambda x:x.endswith(".hs"), srcs):
        _hlint(src)

def _hlint(src):
    prefix = src.replace(":", "__")
    # This sh_binary target shouldn't be required. Working around a buck2 issue.
    # Should be:
    # native.sh_test(
    #     name = prefix + "_hlint",
    #     test = "//hlint-rules:hlint_configured",
    #     resources = ["//hlint-rules:hlint_yamls", src],
    # )
    # However that results in "Failed to spawn a process"
    # Possibly related: https://github.com/facebook/buck2/issues/670
    native.sh_binary(
        name = prefix + "_hlint_bin",
        main = "//hlint-rules:hlint_configured",
        resources = ["//hlint-rules:hlint_yamls", src],
    )
    native.sh_test(
        name = prefix + "_hlint",
        test = ":" + prefix + "_hlint_bin",
        labels = ["hlint"],
    )
