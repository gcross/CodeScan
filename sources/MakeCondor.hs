-- @+leo-ver=4-thin
-- @+node:gcross.20081230235952.5:@thin MakeCondor.hs
-- @@language haskell

import Control.Monad

main = do
    putStrLn "Executable = ScanCodes"
    putStrLn "Universe = vanilla"
    putStrLn "Error  = logs/err.$(cluster)"
    putStrLn "Output = logs/out.$(cluster)"
    putStrLn "Log    = logs/log.$(cluster)"
    putStrLn ""
    putStrLn "Environment = \"LD_LIBRARY_PATH=/phys/users/gcross/local/lib\""
    putStrLn ""
    input <- getContents
    forM_ (lines input) $ \graph -> do putStrLn $ "Arguments = " ++ graph
                                       putStrLn "Queue"
                                       putStrLn ""
-- @-node:gcross.20081230235952.5:@thin MakeCondor.hs
-- @-leo
