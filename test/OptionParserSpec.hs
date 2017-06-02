module OptionParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import qualified Text.Megaparsec as M
import Data.Set (fromList)
import Data.Bifunctor (first)

import OptionParser


equals a b = M.parse option "" a `shouldParse` b

failWith = (. err) . shouldBe . fail
    where err  = Left . fromList . fmap M.DecFail
          fail = first M.errorCustom . M.parse option ""


spec =
  describe "parsing" $ do
    it "valid options" $ do
      equals "-h --help     Show this screen.\n"
           $ Option [ShortName 'h', LongName "help"] Nothing "Show this screen."
      equals "--coefficient=K  The K coefficient [default: 2.95]\n"
           $ Option [LongName "coefficient"] (Just $ Parameter "K" $ Just "2.95") "The K coefficient "
      equals "--output=FILE  Output file [default: test.txt]\n"
           $ Option [LongName "output"] (Just $ Parameter "FILE" $ Just "test.txt") "Output file "
      equals "--directory=DIR  Some directory [default: ./]\n"
           $ Option [LongName "directory"] (Just $ Parameter "DIR" $ Just "./") "Some directory "
      equals "-o FILE --output=FILE  Description\n"
           $ Option [ShortName 'o', LongName "output"] (Just $ Parameter "FILE" Nothing) "Description"
      equals "-i <file>, --input <file>  Description\n"
           $ Option [ShortName 'i', LongName "input"] (Just $ Parameter "file" Nothing) "Description"
      equals "--stdout  Use stdout.\n"
           $ Option [LongName "stdout"] Nothing "Use stdout."

    it "invalid options" $ do
      failWith "-a A --bb B  Teste" ["Ta de brincation"]

