def haskell_lib(**kwargs):
    return native.haskell_library(
        _worker = "toolchains//:persistent_worker",
        **kwargs
    )

def haskell_bin(**kwargs):
    return native.haskell_binary(
        _worker = "toolchains//:persistent_worker",
        **kwargs
    )
