import System.IO
import Language
import Parser
import Prettier

readF :: IO String
readF = do
  ioh <- openFile "input.cl" ReadMode
  prog <- readloop ioh
  hClose ioh
  return prog

readloop :: Handle -> IO String
readloop ioh = do 
  iseof <- hIsEOF ioh
  if iseof
    then return []
    else do
      x <- hGetLine ioh
      xs <- readloop ioh
      return (x ++ "\n" ++ xs) -- Line break must be preserved to recognise end of comment

main :: IO ()
main = do
  inp <- readF
  putStrLn (pprint . parser $ inp)
  return ()
  