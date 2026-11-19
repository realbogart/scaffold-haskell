module {{project_name_capitalized}} (greeting, main) where

greeting :: String -> String
greeting name = "Hello, " ++ name ++ "!"

main :: IO ()
main = putStrLn (greeting "world")
