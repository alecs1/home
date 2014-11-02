-- a program to add or remove a line of a todo list file, or view the contents of the file

import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add),
             ("view", view),
             ("remove", remove)
             ]

main = do
    (command:args) <- getArgs  -- pattern matching, first argument bound to command, next to args
    putStrLn ("command=" ++ (show command) ++ ", args=" ++ (show args))
    let (Just action) = lookup command dispatch -- Just is a "constructor"
    action args


add::[String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add (fileName:words) =
    appendFile fileName (todoItem ++ "\n")
        where todoItem = myUnwords words


view::[String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks -- #kw=lambda, #kw=anonymous; here is an example of an anonymous function, "\" - resembles lambda; n and line are the variables
    putStr (unlines numberedTasks)


remove::[String] -> IO()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle (unlines newTodoItems)
    hClose handle
    hClose tempHandle
    
    -- check if it worked without removeFile
    -- removeFile fileName
    renameFile tempName fileName


myUnwords::[String]->String
myUnwords[] = ""
myUnwords (x:[]) = x -- avoind writing a space after the last word
myUnwords(x:xs) = x ++ " " ++ myUnwords xs



  
