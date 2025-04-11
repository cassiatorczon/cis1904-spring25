module BST where

data Tree
  = Leaf
  | Branch Tree Int Tree
  deriving (Eq, Show)

-- Look at isBST before Exercise 1.

isBST :: Tree -> Bool
isBST Leaf = True
isBST (Branch l x r) =
  isBST l
    && isBST r
    && all (< x) (toList l)
    && all (> x) (toList r)

toList :: Tree -> [Int]
toList Leaf = []
toList (Branch l x r) = toList l ++ [x] ++ toList r

-- Look at find, insert, and delete before Exercise 2.

findGood :: Int -> Tree -> Bool
findGood _ Leaf = False
findGood x (Branch l x' r)
  | x < x' = findGood x l
  | x > x' = findGood x r
  | otherwise = True

insert :: Int -> Tree -> Tree
insert x Leaf = Branch Leaf x Leaf
insert x (Branch l x' r)
  | x < x' = Branch (insert x l) x' r
  | x > x' = Branch l x' (insert x r)
  | otherwise = Branch l x r

delete :: Int -> Tree -> Tree
delete _ Leaf = Leaf
delete x (Branch l x' r)
  | x < x' = Branch (delete x l) x' r
  | x > x' = Branch l x' (delete x r)
  | otherwise = join l r

join :: Tree -> Tree -> Tree
join Leaf r = r
join l Leaf = l
join (Branch l x r) (Branch l' x' r') =
  Branch l x (Branch (join r l') x' r')

findBad1 :: Int -> Tree -> Bool
findBad1 _ Leaf = False
findBad1 x (Branch l x' r)
  | x <= x' = findBad1 x l
  | x > x' = findBad1 x r
  | otherwise = True

findBad2 :: Int -> Tree -> Bool
findBad2 _ Leaf = False
findBad2 x (Branch l x' r)
  | x < x' = findBad2 x l
  | x >= x' = findBad2 x r
  | otherwise = True

findBad3 :: Int -> Tree -> Bool
findBad3 _ Leaf = True
findBad3 x (Branch l x' r)
  | x < x' = findBad3 x l
  | x >= x' = findBad3 x r
  | otherwise = True