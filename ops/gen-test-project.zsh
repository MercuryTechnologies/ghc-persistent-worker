#!/usr/bin/env zsh

setopt no_unset err_exit rematch_pcre

typeset -A conf
conf=(
  [l1]=1
  [l2]=1
  [l1-modules]=3
  [l2-modules]=3
  [bindings]=50
)

msg() {
  print "\e[35m|>\e[m $*"
}

usage()
{
  local opts=''
  for k in ${(k)conf}
  do
    opts="$opts [--$k NUM]"
  done
  msg "Usage: gen-test-project.zsh directory$opts"
  exit 1
}

die()
{
  msg $*
  usage
}

if (( $# == 0 ))
then
  usage
fi

dir=$1

i=2
while (( $i <= $# ))
do
  arg=$@[$i]
  key=${arg#--}
  cur=${conf[$key]-}
  if [[ -z $cur ]]
  then
    die "$arg is not a valid option."
  fi
  (( i++ ))
  new=$@[$i]
  if [[ ! $new =~ '^\d+$' ]]
  then
    die "$arg needs an integer argument, got: $new"
  fi
  conf[$key]=$new
  (( i++ ))
done

l1=$conf[l1]
l2=$conf[l2]
l1_modules=$conf[l1-modules]
l2_modules=$conf[l2-modules]
bindings=$conf[bindings]

marker='worker-test-project-marker'
typeset -a l1_imports
typeset -a targets
typeset -a l1_deps
typeset -a bin_deps
l1_imports=()
l2_imports=()
targets=()
l1_deps=()
l2_deps=()
l1_sum='0'
l2_sum='0'

if [[ -d $dir ]] && [[ -n $(ls $dir) ]] && [[ ! -f $dir/$marker ]]
then
  msg "\e[34m$dir\e[m is not empty but doesn't contain a previous marker file \e[34m$marker\e[m. Mistake?"
  usage
fi

rm -rf $dir
mkdir -p $dir
cd $dir
touch $marker

l1_module()
{
  local mod=$1 name_prefix=$2 i=$3 dep=$4
  local content=''
  content+="module ${mod} where

"
  for j in {1..$bindings}
  do
    local name="${name_prefix}_${i}_${j}"
    content+="
${name} :: Int
${name} = ${i} + ${j}
"
    l1_sum+=" + ${name}"
  done
  l1_imports+=("import ${mod}")
  print $content > "${mod}.hs"
}

l2_module()
{
  local mod=$1 name_prefix=$2 i=$3 dep=$4
  local content=''
  local l2_bind="use_${k}_$i"
  content+="module ${mod} where

${(F)l1_imports}

$l2_bind :: Int
$l2_bind = ${l1_sum}
"
  l2_sum+=" + ${l2_bind}"
  l2_imports+=("import ${mod}")
  print $content > "${mod}.hs"
}

l1_library()
{
  local k=$1
  target="worker-test-dep$k"
  targets+=('haskell_library(
    name = "'$target'",
    srcs = [
'${(F)mods}'
    ],
    deps = [
        "//haskell:base",
        "//haskell:template-haskell",
    ],
)
')
  l1_deps+=('        ":'$target'",')
}

l2_library()
{
  local k=$1
  target="worker-test-lib$k"
  targets+=('haskell_library(
    name = "'$target'",
    srcs = [
'${(F)mods}'
    ],
    deps = [
        "//haskell:base",
        "//haskell:template-haskell",
'${(F)l1_deps}'
    ],
)
')
  l2_deps+=('        ":'$target'",')
}

for k in {1..$l1}
do
  local -a mods
  mods=()
  for i in {1..$l1_modules}
  do
    local mod="L1_${k}_${i}"
    l1_module $mod "l1_$k" $i 'true'
    mods+=("        \"${mod}.hs\",")
  done
  l1_library $k
done

for k in {1..$l2}
do
  local -a mods lib_imports
  mods=()
  lib_imports=()
  for i in {1..$l2_modules}
  do
    local mod="L2_${k}_${i}"
    l2_module $mod "l2_$k" $i 'false'
    mods+=("        \"${mod}.hs\",")
  done
  l2_library $k
done

cat > Main.hs <<EOF
module Main where

${(F)l2_imports}

main :: IO ()
main = print ($l2_sum)
EOF

cat > BUCK <<EOF
${(F)targets}
haskell_binary(
    name = "worker-test",
    srcs = [
        "Main.hs",
    ],
    deps = [
        "//haskell:base",
        "//haskell:template-haskell",
${(F)l2_deps}
    ],
    linker_flags = [
        "-threaded",
        "-rtsopts",
        "-with-rtsopts=-N",
        "-O2",
        "-rdynamic",
    ],
    link_style = "shared",
)
EOF
