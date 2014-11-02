main = do
    content <- getContents
    putStr (shortLinesOnly content)
-- the main block can be replaced with:
-- main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter(\line -> length line < 10) allLines  -- the \ is a indicating a lambda function definition
        result = unlines shortLines
    in result

       
