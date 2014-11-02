import System.IO

main = do
    handle <- openFile "file.hs" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

    
-- another example, this time use "withFile"
-- looks more like "functional" than the do block, which is more "procedural" 
main2 = do
    withFile2 "file.hs" ReadMode (\handle -> do
                                    contents <- hGetContents handle
                                    putStr contents)


-- how we could write withFile:
withFile2 :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile2 path mode f = do
    handle <- openFile path mode
    results <- f handle
    hClose handle
    return result



