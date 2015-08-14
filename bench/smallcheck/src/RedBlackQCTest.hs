import Test.QuickCheck
import Control.Monad
--import RedBlack hiding (every)
import RedBlackBuggy hiding (every)
import System.Environment

instance Arbitrary Colour where
  arbitrary = oneof [return R, return B]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized t
    where
      t 0 = return E
      t n = oneof [ return E
                  , liftM4 T arbitrary
                             (t (n `div` 2))
                             arbitrary
                             (t (n `div` 2))
                  ]

main = do [n] <- getArgs
          mapM_ (\i -> quickCheckWith hard prop_insertRB) [1..read n]

hard = stdArgs {
  maxSuccess = 1000
  --, maxSize    = (+ 3) . (`div` 2)
  , maxDiscardRatio = 5
  --, show   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
  }
