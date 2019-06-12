module FourthTenSpec where

import           FourthTen
import           Test.Hspec

spec :: Spec
spec = do
  describe "Question 31" $ do
    it "Prime checker..." $ do
      (isPrime 7) == (True)

  describe "Question 32" $ do
    it "Greatest common divisor..." $ do
      (gcd' 252 105) == (21)

  describe "Question 33" $ do
    it "Coprime..." $ do
      (coprime 35 64) == (True)
    it "Not coprime..." $ do
      (coprime 35 20) == (False)

  describe "Question 34" $ do
    it "Totient..." $ do
      (totient 10) == (4)
    it "Totient..." $ do
      (totient 523) == (522)
    it "Totient..." $ do
      (totient 1) == (1)

  describe "Question 35" $ do
    it "Prime factors..." $ do
      (primeFactors 315) == ([3, 3, 5, 7])

  describe "Question 36" $ do
    it "Prime factors & multiplicity..." $ do
      (primeFactorsMult 315) == ([(3,2),(5,1),(7,1)])

  describe "Question 37" $ do
    it "Totient..." $ do
      (totientImproved 10) == (4)
    it "Totient..." $ do
      (totientImproved 523) == (522)
    it "Totient..." $ do
      (totientImproved 1) == (1)

    describe "Question 40" $ do
      it "Goldbach..." $ do
        (goldbach 28) == ((5,23))

      describe "Question 41" $ do
        it "Goldbach list..." $ do
          (goldbachList 9 20) == ([(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)])
