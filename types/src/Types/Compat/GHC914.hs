{-# LANGUAGE CPP #-}

module Types.Compat.GHC914 where

#if !defined(MWB)

sanitizeGhcArgs :: [String] -> [String]
sanitizeGhcArgs = \case
  [] -> []
  "-dep-json" : _ : rest -> sanitizeGhcArgs rest
  "-fpackage-db-byte-code" : rest -> sanitizeGhcArgs rest
  a : rest -> a : sanitizeGhcArgs rest

#else

sanitizeGhcArgs :: [String] -> [String]
sanitizeGhcArgs = id

#endif
