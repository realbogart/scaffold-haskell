module Main where

import Criterion.Main
import {{project_name_capitalized}} qualified

-- Example benchmarks for common operations
main :: IO ()
main = defaultMain
  [ bgroup "String operations"
    [ bench "reverse short string" $ whnf reverse "hello"
    , bench "reverse long string" $ whnf reverse (replicate 1000 'a')
    , bench "concatenation" $ whnf (++ "world") "hello "
    ]
  , bgroup "List operations"
    [ bench "length [1..100]" $ whnf length [1..100 :: Int]
    , bench "sum [1..100]" $ whnf sum [1..100 :: Int]
    , bench "reverse [1..100]" $ whnf reverse [1..100 :: Int]
    ]
  , bgroup "IO operations"
    [ bench "putStrLn simulation" $ nfIO (return () :: IO ())
    ]
  ]