import System.IO
import System.Directory
import Data.List


main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr (unlines numberedTasks)
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks -- this is also ineficient to incorrect - delete searches the element, doesn't go to an index
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
--    removeFile "todo.txt" -- this seems superfluous and even wrong, since the program might crash between the operations and file with dissappear entirely
--    renameFile tempName "todo.txt"
