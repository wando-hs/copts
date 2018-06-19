module NormalizedSamples where

import Copts.AST

navalFate :: Usage
navalFate =
  [ Command "naval_fate"
  , Exclusive
    [ [ Command "ship"
      , Command "new"
      , Repeated (Argument "name")
      ]
    , [ Command "ship"
      , Argument "name"
      , Command "move"
      , Argument "x"
      , Argument "y"
        , Optional [ Option [ Long "speed" ] (Just $ Parameter "kn" ( Just "10" )) ]
      ]
    , [ Command "ship"
      , Command "shoot"
      , Argument "x"
      , Argument "y"
      ]
    , [ Command "mine"
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
    , [ Exclusive
          [ [ Option [ Short 'h' , Long "help" ] Nothing ]
          , [ Option [ Short 'h' , Long "help" ] Nothing ]
          ]
      ]
    , [ Option [ Long "version" ] Nothing ]
    ]
  ]

myProgram :: Usage
myProgram =
  [ Command "my_program"
  , Exclusive
    [ [ Command "command"
      , Option [ Long "option" ] Nothing
      , Argument "argument"
      ]
    , [ Optional [ Argument "optional-argument" ] ]
    , [ Option
        [ Long "another-option" ] (Just $ Parameter "with-argument" Nothing )
      ]
    , [ Required
        [ Exclusive
            [ [ Option [ Long "either-that-option" ] Nothing ]
            , [ Argument "or-this-argument" ]
            ]
        ]
      ]
    , [ Argument "repeating-argument"
      , Repeated (Argument "repeating-argument")
      ]
    , [ Optional
        [ Command "command"
        , Option [ Long "option" ] Nothing
        , Argument "argument"
        ]
      , Argument "q"
      ]
    , [ Optional
        [ Command "command"
        , Option [ Long "option" ] Nothing
        , Argument "argument"
        ]
      , Argument "q"
      ]
    ]
  ]
