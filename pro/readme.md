# Testing the Buck worker

The server executable in [../buck-worker](../buck-worker) can just be started with `cabal run buck-worker:exe:worker` in
the devShell.
For convenience, the flake provides an app that sets the socket env var:

```shell
nix run .#test-server
```

Another app runs a shell script that uses `grpcurl` to send messages to the server, compiling the two modules `Dep1.hs`
and `Main.hs` in this directory:

```shell
nix run .#test-client
```

This script can be adapted to process additional modules.
