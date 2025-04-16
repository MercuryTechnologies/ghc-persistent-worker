# Cabal configuration for local packages
{

  packages = {

    ghc-worker = {
      src = ../ghc-worker;
      cabal = {
        meta.synopsis = "Buck2 GHC persistent worker";
        default-extensions = ["NoFieldSelectors"];
      };
      cabal.ghc-options-exe = [
        "-O2"
        "-threaded"
        "-rtsopts"
        ''"-with-rtsopts=-K512M -I5 -A128M -T -N"''
      ];

      library = {
        enable = true;
        dependencies = [
          "async"
          "binary"
          "bytestring"
          "buck-worker-grpc"
          "buck-worker-internal"
          "buck-worker-proto"
          "buck-worker-types"
          "containers"
          "deepseq"
          "directory"
          "filepath"
          "ghc"
          "ghc-debug-stub"
          "grapesy"
          "optparse-applicative"
          "process"
          "text"
        ];
      };

      executables.ghc-worker = {
        source-dirs = "app/ghc-worker";
      };

      tests.ghc-worker-test = {
        enable = true;
        dependencies = [
          "aeson"
          "buck-worker-internal"
          "buck-worker-types"
          "bytestring"
          "containers"
          "directory"
          "extra"
          "filepath"
          "ghc"
          "ghc-boot"
          "ghc-paths"
          "ghc-worker-test-common"
          "hedgehog"
          "lens"
          "mtl"
          "tasty"
          "tasty-hedgehog"
          "text"
        ];
        source-dirs = "test";
        component = {
          default-extensions = ["NoFieldSelectors"];
          ghc-options = [
            "-threaded"
            "-rtsopts"
            ''"-with-rtsopts=-K512M -I5 -A128M -T -N"''
          ];
        };
      };

    };

    ghc-proxy = {
      src = ../ghc-proxy;
      cabal = {
        meta.synopsis = "Buck2 GHC proxy";
        default-extensions = ["NoFieldSelectors"];
      };
      cabal.ghc-options-exe = [
        "-O2"
        "-threaded"
        "-rtsopts"
        ''"-with-rtsopts=-K512M -I5 -A128M -T -N"''
      ];
      library.enable = false;

      executables.ghc-proxy = {
        source-dirs = "app/ghc-proxy";
        dependencies = [
          "buck-worker-internal"
          "buck-worker-types"
        ];
      };
    };

    buck-proxy = {
      src = ../buck-proxy;
      cabal.meta.synopsis = "Buck2 GHC persistent worker";
      library = {
        enable = true;
        dependencies = [
          "buck-worker-grpc"
          "buck-worker-proto"
          "buck-worker-types"
          "containers"
          "directory"
          "grapesy"
          "optparse-applicative"
          "process"
          "text"
        ];
      };
      executables.buck-proxy = {
        dependencies = [
          "buck-worker-types"
          "optparse-applicative"
          "unix"
        ];
        ghc-options-exe = [
          "-O2"
          "-threaded"
          "-rtsopts"
          ''"-with-rtsopts=-K512M -H -I5 -T -N"''
        ];
        source-dirs = "app/buck-proxy";
      };
    };

    instrument = {
      src = ../instrument;
      cabal.meta.synopsis = "Buck2 GHC persistent worker instrumentation client";
      executable = {
        dependencies = [
          "binary"
          "brick"
          "buck-worker-internal"
          "buck-worker-proto"
          "buck-worker-types"
          "bytestring"
          "containers"
          "ghc-debug-brick"
          "directory"
          "filepath"
          "fsnotify"
          "grapesy"
          "microlens-platform"
          "text"
          "time"
          "vty"
        ];
        ghc-options-exe = [
          "-O2"
          "-threaded"
          "-rtsopts"
          ''"-with-rtsopts=-K512M -H -I5 -T -N"''
        ];
        source-dirs = ".";
      };
    };

    buck-worker-internal = {
      src = ../internal;
      library = {
        enable = true;
        dependencies = [
          "aeson"
          "async"
          "buck-worker-types"
          "bytestring"
          "containers"
          "directory"
          "exceptions"
          "flatparse"
          "filepath"
          "file-io"
          "ghc"
          "ghc-boot"
          "hashable"
          "text"
          "time"
          "transformers"
        ];
        source-dirs = "src";
        ghc-options = ["-O2"];
      };


    };

    buck-worker-proto = {
      src = ../proto;
      library = {
        enable = true;
        dependencies = [
          "bytestring"
          "containers"
          "deepseq"
          "grapesy"
          "lens-family"
          "proto-lens"
          "text"
          "vector"
        ];
        source-dirs = "src";
        ghc-options = ["-O2"];
      };
    };

    buck-worker-types = {
      src = ../types;
      library = {
        enable = true;
        dependencies = [
          "aeson"
          "binary"
          "containers"
          "filepath"
          "ghc"
          "ghc-paths"
          "optparse-applicative"
          "split"
          "text"
        ];
        source-dirs = "src";
        ghc-options = ["-O2"];
      };
    };

    buck-worker-grpc = {
      src = ../grpc;
      library = {
        enable = true;
        dependencies = [
          "buck-worker-types"
          "buck-worker-proto"
          "containers"
          "grapesy"
          "text"
        ];
        source-dirs = "src";
        ghc-options = ["-O2"];
      };
    };

    ghc-worker-test-common = {
      src = ../test-common;
      cabal.default-extensions = ["NoFieldSelectors"];
      library = {
        enable = true;
        dependencies = [
          "aeson"
          "async"
          "buck-worker-internal"
          "buck-worker-types"
          "bytestring"
          "containers"
          "directory"
          "extra"
          "file-io"
          "filepath"
          "generic-lens"
          "ghc"
          "ghc-paths"
          "hedgehog"
          "lens"
          "mtl"
          "tasty"
          "tasty-hedgehog"
          "temporary"
          "text"
          "time"
          "typed-process"
        ];
        source-dirs = "lib";
      };
    };

  };

  cabal = {
    author = "Ian-Woo Kim";
    license = "MIT";
    license-file = "LICENSE";
    meta.maintainer = "ianwookim@gmail.com";
    language = "GHC2021";

    default-extensions = [
      "BlockArguments"
      "DerivingStrategies"
      "DuplicateRecordFields"
      "LambdaCase"
      "OverloadedLabels"
      "OverloadedLists"
      "NamedFieldPuns"
      "OverloadedRecordDot"
      "OverloadedStrings"
      "QuasiQuotes"
      "RecordWildCards"
      "StrictData"
      "TypeFamilies"
      "DataKinds"
    ];

    ghc-options = [
      "-Wall"
      "-Widentities"
      "-Wincomplete-uni-patterns"
      "-Wmissing-deriving-strategies"
      "-Wredundant-constraints"
      "-Wunused-type-patterns"
      "-Wunused-packages"
    ];

    meta = {

      flags = {

        mwb = {
          description = "Use mwb-customized GHC from January 2026";
          manual = true;
          default = false;
        };

        downsweep-cache = {
          description = "GHC contains the patch for using an old module graph as a cache for downsweep";
          manual = true;
          default = false;
        };

        unit-index = {
          description = "GHC contains the patch for the abstraction of parts of the unit state";
          manual = true;
          default = false;
        };

        fixed-nodes = {
          description = "GHC contains the patch for fixed module graph nodes";
          manual = true;
          default = false;
        };

      };

      when = [
        {
          condition = "flag(mwb)";
          cpp-options = ["-DMWB" "-DUNIT_INDEX" "-DDOWNSWEEP_CACHE"];
        }
        {
          condition = "flag(downsweep-cache)";
          cpp-options = ["-DDOWNSWEEP_CACHE"];
        }
        {
          condition = "flag(unit-index)";
          cpp-options = ["-DUNIT_INDEX"];
        }
        {
          condition = "flag(fixed-nodes) || impl(ghc >= 9.14)";
          cpp-options = ["-DFIXED_NODES"];
        }
      ];

    };

  };

}
