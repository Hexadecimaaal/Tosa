module Main
    (main) where

import System.Console.Haskeline
import Language.Tosa
import Data.Text (pack)

main :: IO ()
main = runInputT defaultSettings $ loop emptyContext
   where
       loop :: Context -> InputT IO ()
       loop ctx = do
           outputStrLn $ show ctx
           minput <- getInputLine ">>> "
           case minput of
               Nothing -> return ()
               Just "QUIT" -> return ()
               Just input -> do 
                    case parseText "*REPL*" $ pack input of
                        Left e -> do 
                            outputStrLn e
                            loop ctx
                        Right e -> do
                            case eval ctx e of
                                Left e -> do 
                                    outputStrLn $ show e
                                    loop ctx
                                Right ctx' -> loop ctx'
