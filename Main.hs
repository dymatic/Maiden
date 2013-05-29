import System.IO
import System.Process
import System.Environment

-- Archimedes Imports
import Archimedes.Common
import Archimedes.Sequence.Manipulate
import Archimedes.Sequence.Clarify
import Archimedes.Sequence.Functional

-- Example file input
-- tk1=============================================
-- tk2===============
--     prompt=======
-- opt
-- 
-- y-n:Install Emacs?$apt-get install emacs$nothing
parseline :: String -> (String,[(String,String)])
parseline x = (prompt, (zip options commands))
  where tk1 = splitOn x '$'
        tk2 = splitOn (head tk1) ':'
        prompt = last tk2
        options = splitOn (head tk2) '-'
        commands = tail tk1

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    hFlush stdout
    getLine

look :: (Eq a) => [(a,b)] -> a -> b
look a b = snd $ first (\(c,_) -> c == b) a

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

hasAll :: String -> String -> Bool
hasAll a b = and (map ((flip elem) a) b)

sanitize :: [String] -> [String]
sanitize [] = []
sanitize (x:xs) = if hasAll x ":$-" then (x : sanitize xs) else (sanitize xs)

decline [] = return ()
decline ((prmpt,xs):ys) = do
  mapM putStrLn (map fst xs)
  answer <- promptLine ((prmpt++" ") ++ (init (flatten (intersperse (map fst xs) "/"))) ++ " : ")
  system ( look xs answer)
  decline ys
  
  
main = do
  (file:_)     <- getArgs
  fileHandle   <- openFile file ReadMode
  fileContents <- hGetContents fileHandle

  let parsed = (map parseline (sanitize (lines fileContents)))
  
  decline parsed
  putStrLn "Hello, World"
