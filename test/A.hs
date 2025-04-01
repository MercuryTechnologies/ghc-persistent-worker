{-# LANGUAGE TemplateHaskell #-}
module A where

test x = [| print $x |]