module ParsedSamples where

import Copts.Parser

navalFate :: Help
navalFate =
  Complex "Naval Fate."
    [ [A $ Command "naval_fate", A $ Command "ship", A $ Command "new", Repeated $ A $ Argument "name"]
    , [A $ Command "naval_fate"
      , A $ Command "ship"
      , A $ Argument "name"
      , A $ Command "move"
      , A $ Argument "x"
      , A $ Argument "y"
      , Optional [A $ Option (Long "speed", Nothing), A $ Argument "kn"]]
    , [A $ Command "naval_fate", A $ Command "ship", A $ Command "shoot", A $ Argument "x", A $ Argument "y"]
    , [A $ Command "naval_fate", A $ Command "mine", Required [Exclusive [[A $ Command "set"], [A $ Command "remove"]]], A $ Argument "x", A $ Argument "y", Optional [Exclusive [[A $ Option (Long "moored", Nothing)], [A $ Option (Long "drifting", Nothing)]]]]
    , [A $ Command "naval_fate", Exclusive [[A $ Option (Short 'h', Nothing)], [A $ Option (Long "help", Nothing)]]]
    , [A $ Command "naval_fate", A $ Option (Long "version", Nothing)]
    ]
    [ Details [Short 'h', Long "help"] Nothing "Show this screen."
    , Details [Long "version"] Nothing "Show version."
    , Details [Long "speed"] (Just $ Parameter "kn" $ Just "10") "Speed in knots"
    , Details [Long "moored"] Nothing "Moored (anchored) mine."
    , Details [Long "drifting"] Nothing "Drifting mine."
    ]

myProgram :: Help
myProgram =
  Simple
    ""
    [ [ A (Command "my_program")
      , A (Command "command")
      , A (Option ( Long "option" , Nothing ))
      , A (Argument "argument")
      ]
    , [ A (Command "my_program")
      , Optional [ A (Argument "optional-argument") ]
      ]
    , [ A (Command "my_program")
      , A (Option ( Long "another-option" , Just "with-argument" ))
      ]
    , [ A (Command "my_program")
      , A $ Option (Long "another-option2", Nothing)
      , A $ Argument "with-argument2"
      ]
    , [ A (Command "my_program")
      , Required
          [ Exclusive
              [ [ A (Option ( Long "either-that-option" , Nothing )) ]
              , [ A (Argument "or-this-argument") ]
              ]
          ]
      ]
    , [ A (Command "my_program")
      , A (Argument "repeating-argument")
      , Repeated (A (Argument "repeating-argument"))
      ]
    , [ A (Command "my_program")
      , Optional
          [ A (Command "command")
          , A (Option ( Long "option" , Nothing ))
          , A (Argument "argument")
          ]
      , A (Argument "q")
      ]
    , [ A (Command "my_program")
      , Optional [ A (Command "command") ]
      , Optional [ A (Option ( Long "option" , Nothing )) ]
      , Optional [ A (Argument "argument") ]
      , A (Argument "q")
      ]
    ]
