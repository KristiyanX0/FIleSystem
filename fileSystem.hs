import Data.List ( intersperse )
data FileSystem x = Folder x [(x,x)] [FileSystem x]

startExample :: FileSystem [Char]
startExample = Folder "" [] [
                        Folder "folder1" [("file11", "Content"), ("file12", "Content")] [],
                        Folder "folder2" [("file21", "Content"), ("file22", "Content")] [
                                                               Folder "folder21" [("file211", "Content"), ("file212", "Content")] [],
                                                               Folder "folder22" [("file221", "Content"), ("file222", "Content")] []
                                                              ],
                        Folder "folder3" [("file31", "Content"),("file32", "Content")] []
                       ]
--           --
-- get Files --
--           --

getFiles :: FileSystem x -> [(x, x)]
getFiles (Folder _ l _) = l

getFilesNames :: FileSystem b -> [b]
getFilesNames (Folder _ l _) = map fst l

getName :: FileSystem p -> p
getName (Folder x _ _) = x

getSubFoldersList :: FileSystem x -> [FileSystem x]
getSubFoldersList (Folder _ _ l) = l

getSubFoldersNames :: FileSystem b -> [b]
getSubFoldersNames x = map getName (getSubFoldersList x)

getContent :: FileSystem b -> [b]
getContent x = map fst (getFiles x) ++ getSubFoldersNames x

difference' :: (Foldable t, Eq a) => [a] -> t a -> [a]
difference' xs ys = [x | x <- xs, x `notElem` ys]



----------            ----------
---------- Build tree ----------
----------            ----------

compareFold :: Eq x => [FileSystem x] -> [FileSystem x] -> Bool
compareFold [] [] = True
compareFold [] (_:_) = False
compareFold (_:_) [] = False
compareFold l1@(y:ys) l2@(x:xs) = getName y == getName x && getFiles y == getFiles x && compareFold ys xs
                                  && compareFold (getSubFoldersList y) (getSubFoldersList x)

compareFolders :: Eq x => FileSystem x -> FileSystem x -> Bool
compareFolders a b = getName a == getName b && getFiles a == getFiles b && compareFold (getSubFoldersList a) (getSubFoldersList b)

buildFileSys :: Eq x => FileSystem x -> FileSystem x -> FileSystem x -> [FileSystem x]
buildFileSys root@(Folder _ _ []) _ _ = []
buildFileSys root@(Folder name files l@(y:ys)) currentRoot toBeSwapedWith
    | compareFolders y currentRoot = toBeSwapedWith : buildFileSys (Folder name files ys) currentRoot toBeSwapedWith
    | otherwise = Folder (getName y) (getFiles y) (buildFileSys y currentRoot toBeSwapedWith ) : buildFileSys (Folder name files ys) currentRoot toBeSwapedWith

buildDirectory :: Eq x => FileSystem x -> FileSystem x -> FileSystem x -> FileSystem x
buildDirectory root currentRoot toBeSwapedWith
    | compareFolders root currentRoot = toBeSwapedWith
    | otherwise = Folder (getName root) (getFiles root) (buildFileSys root currentRoot toBeSwapedWith)

--           --
-- add files --
--           --
addFiles :: FileSystem [a] -> [a] -> FileSystem [a]
addFiles (Folder name xs subFolders) line = Folder name (xs ++ [(drop 6 line,[])]) subFolders

--             --
-- make folder --
--             --
mkdir :: FileSystem [a] -> [a] -> FileSystem [a]
mkdir (Folder name files xs) line = Folder name files (xs ++ [Folder (drop 6 line) [] []])

--              --
-- remove files --
--              --
rmFromTree :: (Foldable t, Eq a) => t a -> FileSystem a -> FileSystem a
rmFromTree line f@(Folder a b xs) = Folder a (rmFiles f line) (changeFolder f line)
    where changeFolder fold list = filter (\x -> getName x `elem` rmFolders (Folder a b xs) list) xs
          rmFiles fold line = filter (\t -> fst t `elem` difference' (getFilesNames fold) line) (getFiles f)
          rmFolders fold list = difference' (getSubFoldersNames fold) list

--                     --
-- change file content --
--                     --
change :: Eq t => [(t, b)] -> t -> b -> [(t, b)]
change [] _ _ = []
change (y:ys) file line -- = Folder name files subFolders
    | fst y == file = (fst y, line) : change ys file line
    | otherwise = y : change ys file line

cat :: FileSystem String -> [String] -> FileSystem String
cat (Folder name files subFolders) line = Folder name (change files (head line) (unwords (tail line))) subFolders



--         --
-- helpers --
--         --

findFolder :: Eq x => FileSystem x -> [x] -> FileSystem x
findFolder x [] = x
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

catshow :: Eq t => [(t, p)] -> t -> p
catshow [] _ = error "Doesn't exist!"
catshow (x:xs) line
    | fst x == line = snd x
    | otherwise = catshow xs line

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
                        else if take 2 line == "rm" then do loop path (buildDirectory root currentRoot (rmFromTree (splitToList ' ' line) currentRoot)) (rmFromTree (splitToList ' ' line) currentRoot)
                        else if line == "getName" then do print $ getName currentRoot
                                                          loop path root currentRoot
                        else if line == "pwd" then do pathDo path
                                                      loop path root currentRoot
                        else if take 5 line == "mkdir" then do loop path (buildDirectory root currentRoot (mkdir currentRoot line)) (mkdir currentRoot line)
                        else if take 5 line == "touch" then do loop path (buildDirectory root currentRoot (addFiles currentRoot line)) (addFiles currentRoot line)
                        else if take 5 line == "cat >" then do loop path (buildDirectory root currentRoot (cat currentRoot (drop 2 (splitToList ' ' line)))) (cat currentRoot (drop 2 (splitToList ' ' line)))
                        else if take 3 line == "cat" then do print $ catshow (getFiles currentRoot) (drop 4 line)
                                                             loop path root currentRoot
                        else do putStrLn "<Invalid operation>"
                                loop path root currentRoot

main :: IO ()
main = do
    looper [] startExample --(Folder "home" [] [])
