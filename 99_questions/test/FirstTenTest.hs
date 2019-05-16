module Main where

import           FirstTen
import           System.Exit      (exitFailure)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [questionOne, questionTwo, questionThree, questionFour, questionFive, questionSix, questionSeven, questionEight, questionNine, questionTen]

questionOne =
  testCase "Question 1..." $ assertEqual [] (myLast [1,2,3,4]) (4)

questionTwo =
  testCase "Question 2..." $ assertEqual [] (myButLast ['a'..'z']) ('y')

questionThree =
  testCase "Question 3..." $ assertEqual [] (elementAt "haskell" 5) ('e')

questionFour =
  testCase "Question 4..." $ assertEqual [] (myLength [123, 456, 789]) (3)

questionFive =
  testCase "Question 5..." $ assertEqual [] (myReverse [1,2,3,4]) ([4,3,2,1])

questionSix =
  testCase "Question 6..." $ assertEqual [] (isPalindrome [1,2,4,8,16,8,4,2,1]) (True)

questionSeven =
  testCase "Question 7..." $ assertEqual [] (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) ([1,2,3,4,5])

questionEight =
  testCase "Question 8..." $ assertEqual [] (compress "aaaabccaadeeee") ("abcade")

questionNine =
  testCase "Question 9..." $ assertEqual [] (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
             'a', 'd', 'e', 'e', 'e', 'e']) (["aaaa","b","cc","aa","d","eeee"])

questionTen =
  testCase "Question 10..." $ assertEqual [] (encode "aaaabccaadeeee") ([(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')])
