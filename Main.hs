module Main where
import Backend
import System.Environment
import Text.Printf


main :: IO ()
main = getArgs >>= \case
    -- TODO: stub
    [environmentPath,agentPaths] -> putStrLn $ "dtautom called with environment " ++ environmentPath ++ " and agents " ++ agentPaths
    _ -> putStrLn "Usage: dtautom ENVFILE [AGENTFILE] ..."

