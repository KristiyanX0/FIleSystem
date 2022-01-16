import Data.List ( intersperse )
data FileSystem x = NoSubFolder | Folder x [x] [FileSystem x]

fileSys :: FileSystem [Char]
fileSys = Folder "" [] [
                    Folder "folder1" ["file5", "file2"] [NoSubFolder],
                    Folder "folder2" ["file", "file2"] [
                                                Folder "folder21" ["file9", "file8"] [NoSubFolder],
                                                Folder "folder22" ["file1", "file2"] [NoSubFolder]
                                                ],
                    Folder "folder3" ["5","8"] [NoSubFolder]
                       ]

--get Files
getFiles :: FileSystem x -> [x]
getFiles NoSubFolder = error "Folder doesn't exist!"
getFiles (Folder _ l _) = l

getName :: FileSystem p -> p
getName NoSubFolder = error "Folder doesn't exist!"
getName (Folder x _ _) = x

getSubFoldersList :: FileSystem x -> [FileSystem x]
getSubFoldersList NoSubFolder = error "Folder doesn't exist!"
getSubFoldersList (Folder _ _ l) = l

getSubFoldersNames :: FileSystem b -> [b]
getSubFoldersNames x = map getName (getSubFoldersList x)

getContent :: FileSystem b -> [b]
getContent f@(Folder _ x [NoSubFolder]) = getFiles f
getContent x = getFiles x ++ getSubFoldersNames x

difference' :: (Foldable t, Eq a) => [a] -> t a -> [a]
difference' xs ys = [x | x <- xs, x `notElem` ys]

--remove files
rmFromTree :: (Foldable t, Eq a) => t a -> FileSystem a -> FileSystem a
rmFromTree line NoSubFolder = error "This action can't be done!"
rmFromTree line f@(Folder a b xs) = Folder a (rmFiles f line) (changeFolder f line)
    where changeFolder fold list = filter (\x -> getName x `elem` rmFolders (Folder a b xs) list) xs
          rmFiles fold line = difference' (getFiles fold) line
          rmFolders fold list = difference' (getSubFoldersNames fold) list

--make folder
mkdir :: FileSystem [a] -> [a] -> FileSystem [a]
mkdir NoSubFolder _ = error "This action can't be done!"
mkdir (Folder name files xs) line = addToFolder (Folder (drop 6 line) [] [])
    where addToFolder f = Folder name files (xs ++ [f])

isItElement :: Eq t1 => [t1] -> t2 -> t1 -> Bool
isItElement [] root currentRoot = False
isItElement (x:xs) root currentRoot
    | x == currentRoot = True
    | otherwise = isItElement xs root currentRoot

buildFileSys :: FileSystem x -> t1 -> t2 -> FileSystem x
buildFileSys (Folder _ _ []) _ _ = NoSubFolder
buildFileSys NoSubFolder _ _ = NoSubFolder
buildFileSys root@(Folder name files l@(y:ys)) currentRoot toBeSwapedWith
    | not $ null l = Folder name files (Folder (getName y) (getFiles y) [buildFileSys y currentRoot toBeSwapedWith]
                                        : [buildFileSys (Folder name files ys) currentRoot toBeSwapedWith])
    -- | currentRoot == y = Folder (getName y) (getFiles y) [buildFileSys y currentRoot toBeSwapedWith]
    | otherwise = NoSubFolder



--helpers
findFolder :: Eq x => FileSystem x -> [x] -> FileSystem x
findFolder x [] = x
findFolder NoSubFolder _ = error "No more subfolders!"
findFolder (Folder _ _ []) _ = error "No more subfolders!"
findFolder f@(Folder name files (y:ys)) l@(x:xs)
    | x == getName y = findFolder y xs
    | otherwise = findFolder (Folder name files ys) l

splitToList :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitToList seperate = foldr emptyChar [[]]
    where emptyChar y ~l@(x:xs)
            | y == seperate = []:l
            | otherwise = (y:x):xs

cd :: Foldable t => t Char -> FileSystem [Char] -> FileSystem [Char]
cd line root = findFolder root (splitToList '/' line)

ls :: Show b => FileSystem b -> IO ()
ls root = do print $ getContent root

extendedls :: Foldable t => t Char -> FileSystem [Char] -> IO ()
extendedls line root = do print $ getContent $ findFolder root (splitToList '/' line)

pathDo :: [[Char]] -> IO ()
pathDo path = do print $ concat (["/"] ++ intersperse "/" path)

looper :: p -> FileSystem [Char] -> IO ()
looper path root = loop [] root root
        where loop path root currentRoot = do
                line <- getLine
                if line == "exit" then return ()
                    else do
                        if line == "ls" then do ls currentRoot
                                                loop path root currentRoot
                        else if take 4 line == "ls /" then do extendedls (drop 4 line) root
                                                              loop path root currentRoot
                        else if take 2 line == "ls" then do extendedls (drop 3 line) currentRoot
                                                            loop path root currentRoot
                        else if line == "cd .." then do loop [] root root
                        else if take 4 line == "cd /" then do loop (splitToList '/' (drop 4 line)) root (cd (drop 4 line) root)
                        else if take 3 line == "cd " then do loop (path ++ splitToList '/' (drop 3 line)) root (cd (drop 3 line) currentRoot)
                        else if take 2 line == "rm" then do loop path root $ rmFromTree (splitToList ' ' line) currentRoot
                        else if line == "getName" then do print $ getName currentRoot
                                                          loop path root currentRoot
                        else if line == "pwd" then do pathDo path
                                                      loop path root currentRoot
                        else if take 5 line == "mkdir" then do loop path (buildFileSys root currentRoot $ mkdir currentRoot line) $ mkdir currentRoot line
                        else do
                                putStrLn "<Invalid operation>"
                                loop path root currentRoot

main :: IO ()
main = do
    looper [] fileSys --(Folder "home" [] [])