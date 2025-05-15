#!/usr/bin/env zsh

setopt no_unset err_exit rematch_pcre

typeset -A conf
conf=(
  [l1]=1
  [l2]=1
  [l3]=1
  [l1-modules]=3
  [l2-modules]=3
  [l3-modules]=3
  [bindings]=50
  [th]=0
  [bin]=0
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
  if [[ $key == 'th' ]] || [[ $key == 'bin' ]]
  then
    new=1
  else
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
  fi
  conf[$key]=$new
  (( i++ ))
done

l1=$conf[l1]
l2=$conf[l2]
l3=$conf[l3]
l1_modules=$conf[l1-modules]
l2_modules=$conf[l2-modules]
l3_modules=$conf[l3-modules]
bindings=$conf[bindings]
use_th=$conf[th]
add_bin=$conf[bin]

marker='worker-test-project-marker'
typeset -a l1_imports
typeset -a l2_imports
typeset -a l3_imports
typeset -a targets
typeset -a l1_deps
typeset -a l2_deps
typeset -a l3_deps
l1_imports=()
l2_imports=()
l3_imports=()
targets=()
l1_deps=()
l2_deps=()
l3_deps=()
l1_sum='0'
l2_sum='0'
l3_sum='0'

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
  content+="module ${mod} where

${(F)l1_imports}
"
  if [[ $use_th == 1 ]]
  then
    content+="
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (lift)

"
  fi
  for j in {1..$bindings}
  do
    local name="${name_prefix}_${i}_${j}"
    if [[ $use_th == 1 ]]
    then
      content+="
${name} :: ExpQ
${name} = lift @_ @Int (${i} + ${l1_sum})
"
      l2_sum+=" + \\\$(${name})"
    else
      content+="
${name} :: Int
${name} = ${i} + ${l1_sum}
"
      l2_sum+=" + ${name}"
    fi
  done
  l2_imports+=("import ${mod}")
  print $content > "${mod}.hs"
}

l3_module()
{
  local mod=$1 name_prefix=$2 i=$3 dep=$4
  local content=''
  local name="use_${k}_$i"
  content+="{-# language TemplateHaskell #-}
  module ${mod} where

${(F)l2_imports}

$name :: Int
$name = ${i} + ${l2_sum}
"
  l3_sum+=" + ${name}"
  l3_imports+=("import ${mod}")
  print $content > "${mod}.hs"
}

l1_library()
{
  local k=$1
  target="worker-test-l1-lib$k"
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
  target="worker-test-l2-lib$k"
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

l3_library()
{
  local k=$1
  target="worker-test-l3-lib$k"
  targets+=('haskell_library(
    name = "'$target'",
    srcs = [
'${(F)mods}'
    ],
    deps = [
        "//haskell:base",
        "//haskell:template-haskell",
'${(F)l2_deps}'
    ],
)
')
  l3_deps+=('        ":'$target'",')
}

if (( $l1 > 0 ))
then

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

fi

if (( $l2 > 0 ))
then

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

fi

for k in {1..$l3}
do
  local -a mods lib_imports
  mods=()
  lib_imports=()
  for i in {1..$l3_modules}
  do
    local mod="L3_${k}_${i}"
    l3_module $mod "l3_$k" $i 'false'
    mods+=("        \"${mod}.hs\",")
  done
  l3_library $k
done

cat > BUCK <<EOF
${(F)targets}
EOF

if (( $add_bin == 1 ))
then

  cat > Main.hs <<EOF
module Main where

${(F)l3_imports}

main :: IO ()
main = print ($l3_sum)
EOF

  cat > BUCK <<EOF
haskell_binary(
    name = "worker-test",
    srcs = [
        "Main.hs",
    ],
    deps = [
        "//haskell:base",
        "//haskell:template-haskell",
${(F)l3_deps}
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

fi
