module ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec as M (parse)

import Copts.Parser
import Copts.Parser.Usage
import Copts.Parser.Element
import Copts.Parser.OptionDetails


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
\\n"

-- \Options:\n\
-- \  -h --help     Show this screen.\n\
-- \  --version     Show version.\n\
-- \  --speed=<kn>  Speed in knots [default: 10].\n\
-- \  --moored      Moored (anchored) mine.\n\
-- \  --drifting    Drifting mine."

spec =
  describe "parsing a help text" $ do
    it "when it is valid" $ do
      parse (help, navalFate) $
        Simple "Naval Fate.\n\n" [
          [A (Command "naval_fate"), A (Command "ship"), A (Command "new"), Ellipsis (A (Argument "name"))],
          [A (Command "naval_fate"), A (Command "ship"), A (Argument "name"), A (Command "move"), A (Argument "x"), A (Argument "y"), Optional [A (Option (Long "speed", Just "kn"))]],
          [A (Command "naval_fate"), A (Command "ship"), A (Command "shoot"), A (Argument "x"), A (Argument "y")],
          [A (Command "naval_fate"), A (Command "mine"), Required [XOR [A (Command "set"), A (Command "remove")]], A (Argument "x"), A (Argument "y"), Optional [XOR [A (Option (Long "moored", Nothing)), A (Option (Long "drifting", Nothing))]]],
          [A (Command "naval_fate"), XOR [A (Option (Short 'h', Nothing)), A (Option (Long "help", Nothing))]],
          [A (Command "naval_fate"), A (Option (Long "version", Nothing))]]
