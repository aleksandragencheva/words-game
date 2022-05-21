import Lib
import Test.Hspec
import Data

gwc = gridWithCoords grid
testFindWord word = 
  let (Just result) = findWord gwc word
      string = map cell2char result
  in string `shouldBe`  word

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should concatenate each line with newline" $ do
      (formatGrid gridWithCoords(["abc", "def", "hij"])) `shouldBe` "abc\ndef\nhij\n"

  describe "findWord" $ do
    it "Should find word that exists on the Grid" $ do
      testFindWord "HASKELL"
      testFindWord "PERL"

    it "Should not find word that exists on the Grid" $ do
      findWord gwc "HAMSTER" `shouldBe` Nothing

  describe "findWords" $ do
    it "Should find all the words that exist on the Grid" $ do
      let found = findWords gwc languages 
          asString = map (map cell2char) found
      in asString  `shouldBe` languages

    it "Should not find all the words that exist on the Grid" $ do
      findWords gwc ["french", "english", "dutch"] `shouldBe` []