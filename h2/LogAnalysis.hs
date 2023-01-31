{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

intAtWord :: Int -> String -> Int
intAtWord n input = read (words input !! n)

wordsAfterK :: Int -> String -> String
wordsAfterK k input = unwords (drop k (words input))

parseMessage :: String -> LogMessage
parseMessage input = case head input of  
  'I' -> LogMessage Info (intAtWord 1 input) (wordsAfterK 2 input)
  'W' -> LogMessage Warning (intAtWord 1 input) (wordsAfterK 2 input)
  'E' -> LogMessage (Error (intAtWord 1 input)) (intAtWord 2 input) (wordsAfterK 3 input)
  _   ->  Unknown input


parse :: String -> [LogMessage]
parse input = inorder (build (p input))

p :: String -> [LogMessage]
p input = map parseMessage (lines input)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ s1 _) (Node left c@(LogMessage _ s2 _) right) 
  = if s1 < s2 then Node (insert msg left) c right else Node left c (insert msg right)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:y) = insert x (build y)


inorder :: MessageTree -> [LogMessage]

inorder Leaf = []
inorder (Node left mid right) = inorder left ++ [mid] ++ inorder right

whatWentWrong :: [LogMessage] -> [String]

whatWentWrong input = [s | (LogMessage (Error x) _ s) <- input, x > 50]
