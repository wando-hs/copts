module OptionDetailsSpec where


import Helpers
import Test.Hspec
import Copts.Parser.Element
import Copts.Parser.OptionDetails


text #> result = do
  parse (details, text ++ "  ") result
  parse (details, "  " ++ text) result
  parse (details, text) result

spec =
  describe "parsing option details" $ do
    it "when it is valid" $ do
      "-h --help     Show this screen."                    #> Details [Short 'h', Long "help"] Nothing "Show this screen."
      "--coefficient=K  The K coefficient [default: 2.95]" #> Details [Long "coefficient"] (Just $ Parameter "K" $ Just "2.95") "The K coefficient"
      "--output=FILE  Output file [default: test.txt]"     #> Details [Long "output"] (Just $ Parameter "FILE" $ Just "test.txt") "Output file"
      "--directory=DIR  Some directory [default: ./]"      #> Details [Long "directory"] (Just $ Parameter "DIR" $ Just "./") "Some directory"
      "-o FILE --output=FILE  Description"                 #> Details [Short 'o', Long "output"] (Just $ Parameter "FILE" Nothing) "Description"
      "-i <file>, --input <file>  Description"             #> Details [Short 'i', Long "input"] (Just $ Parameter "file" Nothing) "Description"
      "--stdout  Use stdout."                              #> Details [Long "stdout"] Nothing "Use stdout."

