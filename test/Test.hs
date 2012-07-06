import System.Exit

main :: IO ()
main = do
  putStrLn "\^[[31;1m>>>> Should have some tests <<<<\^[[0m"
  exitFailure
