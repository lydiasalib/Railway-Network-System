–getRandomNumbers is a function that inputs a code and an int and returns a list of size int with random code input. It is in the starter file

data PriorityQueue = PQ PQNode PriorityQueue | Empty deriving (Show, Eq) – initial conditions
data PQNode = Node Int Int deriving (Show, Eq)

railsNetwork :: Int -> Int -> [[Int]] –part a
railsNetwork size seedforgen =  
    zeroDiagonal (splits size (getRandomNumbers (size^2) seedforgen))

splits:: Int -> [a] -> [[a]] –helper for part a
splits _ [] = []
splits n xs = take n xs : splits n (drop n xs)

zeroDiagonal :: [[Int]] -> [[Int]] –helper 2 for part a
zeroDiagonal matrix = zipWith setZero matrix [0..]
  where
    setZero row i = take i row ++ [0] ++ drop (i + 1) row

pullLever :: Int -> Int -> Int -> [[Int]] -> [[Int]]  – part b
pullLever seed n m xs =
  let
    rand = head (getRandomNumbers 1 seed)
    updateRow row i =
      if i == n
      then map (\(val, j) -> if j == m then (if n == m then 0 else rand) else val) (zip row [1..])
      else row
  in
    map (\(row, i) -> updateRow row i) (zip xs [1..])




