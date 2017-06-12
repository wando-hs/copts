module ParserSpec where


import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec as M (parse)

import Copts.Parser


parse :: (Show a, Eq a) => (Parser a, String) -> a -> Expectation
parse (parser, text) result = M.parse parser "" text `shouldParse` result

navalFate = "Naval Fate.\n\
\\n\
\Usage:\n\
\  naval_fate ship new <name>...\n\
\  naval_fate ship <name> move <x> <y> [--speed=<kn>]\n\
\  naval_fate ship shoot <x> <y>\n\
\  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]\n\
\  naval_fate -h | --help\n\
\  naval_fate --version\n\
\\n\
\Options:\n\
\  -h --help     Show this screen.\n\
\  --version     Show version.\n\
\  --speed=<kn>  Speed in knots [default: 10].\n\
\  --moored      Moored (anchored) mine.\n\
\  --drifting    Drifting mine."

spec =
  describe "parsing a help text" $ do
    it "when it is valid" $ do
      parse (help, navalFate) $ Complex "Naval Fate." [] []
