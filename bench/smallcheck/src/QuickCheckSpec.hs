module QuickCheckSpec where

import           Test.Hspec
import           Countdown
import Debug.Trace
import Test.QuickCheck

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
          mapM_ (\i -> check hard prop_insertRB) [1..read n]

hard :: Config
hard = Config
  { maxTest = 1000
  , maxFail = 10000
  , size    = (+ 3) . (`div` 2)
  , every   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
  }


spec :: Spec
spec = do
  describe "Countdown problem" $ do
    let in1 = [1, 2, 3, 4]
        out1 = [2, 3, 4]
    it "subst" $ do
      let subsRes = subs(in1)
      --subsRes `shouldSatisfy` (\x -> [5] `elem` x)
      ([1, 3, 4] `elem` subsRes) `shouldBe` True
      --do print $ traceShow subsRes ()
      length subsRes `shouldBe` 16

    it "interleave" $ do
      let interleaveRes = interleave 9 in1
      do print $ traceShow interleaveRes ()
      length interleaveRes `shouldBe` 5

    it "choice" $ do
      let choiceRes = choices in1
      choiceRes `shouldSatisfy` (\x ->
        [1, 3, 4] `elem` x &&
        [4, 2] `elem` x)

    it "split" $ do
      let splitRes = split in1
      splitRes `shouldSatisfy` (\x ->
        ([1, 2], [3, 4]) `elem` x)

    it "splitQuickCheck" $ property $
      (\x -> \y -> not (null x) && not (null y) ==>
        (x ::[Int], y ::[Int]) `elem` (split (x++y)) )