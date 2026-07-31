{-# LANGUAGE CPP #-}

module Internal.Compat.Linkables where

#if defined(LINKABLES)

support_Linkables :: Bool
support_Linkables = True

#else

support_Linkables :: Bool
support_Linkables = False

#endif
