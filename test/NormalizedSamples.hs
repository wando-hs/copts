module NormalizedSamples where

import Copts.AST

navalFate =
  [ [ Command "naval_fate"
    , Command "ship"
    , Command "new"
    , Repeated (Argument "name")
    ]
  , [ Command "naval_fate"
    , Command "ship"
    , Argument "name"
    , Command "move"
    , Argument "x"
    , Argument "y"
      , Optional [ Option [ Long "speed" ] (Just $ Parameter "kn" ( Just "10" )) ]
    ]
  , [ Command "naval_fate"
    , Command "ship"
    , Command "shoot"
    , Argument "x"
    , Argument "y"
    ]
  , [ Command "naval_fate"
    , Command "mine"
    , Required
        [ Exclusive [ [ Command "set" ] , [ Command "remove" ] ] ]
    , Argument "x"
    , Argument "y"
    , Optional
        [ Exclusive
            [ [ Option [ Long "moored" ] Nothing ]
            , [ Option [ Long "drifting" ] Nothing ]
            ]
        ]
    ]
  , [ Command "naval_fate"
    , Exclusive
        [ [ Option [ Short 'h' , Long "help" ] Nothing ]
        , [ Option [ Short 'h' , Long "help" ] Nothing ]
        ]
    ]
    , [ Command "naval_fate" , Option [ Long "version" ] Nothing ]
  ]

myProgram =
  [ [ Command "my_program"
    , Command "command"
    , Option [ Long "option" ] Nothing
    , Argument "argument"
    ]
  , [ Command "my_program"
    , Optional [ Argument "optional-argument" ]
    ]
  , [ Command "my_program"
    , Option
        [ Long "another-option" ] (Just $ Parameter "with-argument" Nothing )
    ]
  , [ Command "my_program"
    , Required
        [ Exclusive
            [ [ Option [ Long "either-that-option" ] Nothing ]
            , [ Argument "or-this-argument" ]
            ]
        ]
    ]
  , [ Command "my_program"
    , Argument "repeating-argument"
    , Repeated (Argument "repeating-argument")
    ]
  , [ Command "my_program"
    , Optional
        [ Command "command"
        , Option [ Long "option" ] Nothing
        , Argument "argument"
        ]
    , Argument "q"
    ]
  , [ Command "my_program"
    , Optional
        [ Command "command"
        , Option [ Long "option" ] Nothing
        , Argument "argument"
        ]
    , Argument "q"
    ]
  ]
