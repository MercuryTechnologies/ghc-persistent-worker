#!/usr/bin/env zsh

setopt err_exit no_unset

ghc_bin=$(which ghc)
ghc_path=${ghc_bin:h:h}
root=$(pwd)
project=$root/pro
out=$project/out
tmp=$project/tmp
socket=${WORKER_SOCKET:-${project}/worker-socket}

rm -rf $out $tmp
mkdir -p $out $tmp

req()
{
  local file=$1 extra=(${*[2,$]}) args=''
  local mod=${${file:t}%.hs}
  local topdir=$(print -- $ghc_path/lib/ghc-*/lib)
  if (( $#extra > 0 ))
  then
    args=",\"${(j:",":)extra}\""
  fi
  grpcurl -import-path $root/buck-worker -proto worker.proto -unix -d @ -plaintext $socket worker.Worker.Execute <<EOF
{
  "argv": [
    "--ghc",
    "${ghc_bin}",
    "-B${topdir}",
    "-i",
    "-o",
    "${out}/${mod}.dyn_o",
    "-ohi",
    "${out}/${mod}.dyn_hi",
    "-dynohi",
    "${out}/${mod}.dyn_hi",
    "-odir",
    "$out",
    "-hiedir",
    "$out",
    "-dumpdir",
    "$out",
    "-stubdir",
    "$out",
    "-fwrite-if-simplified-core",
    "-package",
    "base",
    "-dynamic",
    "-fPIC",
    "-osuf",
    "dyn_o",
    "-hisuf",
    "dyn_hi",
    "-this-unit-id",
    "${project:t}",
    "-i$out",
    "$project/$file"
    $args
  ],
  "env": [
    {
      "key": "TMPDIR",
      "value": "$tmp"
    }
  ]
}
EOF
}

req 'Dep1.hs'
req 'Main.hs'
