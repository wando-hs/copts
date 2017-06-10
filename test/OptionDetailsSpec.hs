module OptionDetailsSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (Dec (..), parse, errorCustom)

import Data.Set (fromList)
import Data.Bifunctor (first)

import Copts.Parser.Element
import Copts.Parser.OptionDetails


equals a b = parse details "" a `shouldParse` b

failWith = (. err) . shouldBe . fail
    where err  = Left . fromList . fmap DecFail
          fail = first errorCustom . parse details ""


spec =
  describe "parsing" $ do
    it "valid options" $ do
      equals "-h --help     Show this screen.\n"
           $ Details [Short 'h', Long "help"] Nothing "Show this screen."
      equals "--coefficient=K  The K coefficient [default: 2.95]\n"
           $ Details [Long "coefficient"] (Just $ Parameter "K" $ Just "2.95") "The K coefficient "
      equals "--output=FILE  Output file [default: test.txt]\n"
           $ Details [Long "output"] (Just $ Parameter "FILE" $ Just "test.txt") "Output file "
      equals "--directory=DIR  Some directory [default: ./]\n"
           $ Details [Long "directory"] (Just $ Parameter "DIR" $ Just "./") "Some directory "
      equals "-o FILE --output=FILE  Description\n"
           $ Details [Short 'o', Long "output"] (Just $ Parameter "FILE" Nothing) "Description"
      equals "-i <file>, --input <file>  Description\n"
           $ Details [Short 'i', Long "input"] (Just $ Parameter "file" Nothing) "Description"
      equals "--stdout  Use stdout.\n"
           $ Details [Long "stdout"] Nothing "Use stdout."

    it "invalid options" $ do
      failWith "-a A --bb B  Teste" ["Ta de brincation"]

