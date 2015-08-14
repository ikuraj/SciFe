--{-# LANGUAGE PackageImports #-}
--import "base" Prelude
import SmallCheck
import RedBlack
import System.Environment

instance Serial Colour where
  series = cons0 R \/ cons0 B

instance Serial a => Serial (Tree a) where
  series = cons0 E \/ cons4 T

main = do [d] <- getArgs ; depthCheck (read d) prop_insertRB
