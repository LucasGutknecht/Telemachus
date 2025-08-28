import System.Environment
import System.Process
import Data.List (isInfixOf)
import Distribution.Compat.Prelude (ExitCode(ExitSuccess))

contemExtensaoC :: String -> Bool
contemExtensaoC s = ".c" `isInfixOf` s

contemArgLex :: String -> Bool
contemArgLex lex = "--lex" `isInfixOf` lex


main :: IO ()

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Sem argumentos"
    (x:xs) -> if contemExtensaoC x && contemArgLex xs
              -- runs the gcc comand to the file, if ok, return 0, if not returns 1
              then do
                status <- system $ "gcc " ++ x
                case status of
                  ExitSuccess   -> return ()
                  ExitFailure _ -> return ()
              else putStrLn $ "O arquivo " ++ x ++ " não é um arquivo C."
