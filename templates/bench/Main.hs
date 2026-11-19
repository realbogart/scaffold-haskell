module Main (main) where

import Criterion.Main
import {{project_name_capitalized}} qualified as App

main :: IO ()
main =
  defaultMain
    [ bgroup
        "greeting"
        [ bench "short name" $ nf App.greeting "world",
          bench "long name" $ nf App.greeting (replicate 1000 'a')
        ]
    ]
