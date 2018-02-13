module NormalizedSamples where

import Copts.Normalizer

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
    , Optional [ Option [ "--speed" ] (Just ( "kn" , Just "10" )) ]
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
            [ [ Option [ "--moored" ] Nothing ]
            , [ Option [ "--drifting" ] Nothing ]
            ]
        ]
    ]
  , [ Command "naval_fate"
    , Exclusive
        [ [ Option [ "-h" , "--help" ] Nothing ]
        , [ Option [ "-h" , "--help" ] Nothing ]
        ]
    ]
  , [ Command "naval_fate" , Option [ "--version" ] Nothing ]
  ]

