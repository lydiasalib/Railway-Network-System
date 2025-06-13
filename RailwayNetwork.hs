data PriorityQueue = PQ PQNode PriorityQueue | Empty deriving (Show, Eq) -- initial conditions
data PQNode = Node Int Int deriving (Show, Eq)

railsNetwork :: Int -> Int -> [[Int]] --part a
railsNetwork size seedforgen =  
    zeroDiagonal (splits size (getRandomNumbers (size^2) seedforgen))

splits:: Int -> [a] -> [[a]] -- helper for part qa
splits _ [] = []
splits n xs = take n xs : splits n (drop n xs)

zeroDiagonal :: [[Int]] -> [[Int]] -- helper 2 for part a
zeroDiagonal matrix = zipWith setZero matrix [0..]
  where
    setZero row i = take i row ++ [0] ++ drop (i + 1) row

pullLever :: Int -> Int -> Int -> [[Int]] -> [[Int]]  -- part b
pullLever seed n m xs =
  let
    rand = head (getRandomNumbers 1 seed)
    updateRow row i =
      if i == n
      then map (\(val, j) -> if j == m then (if n == m then 0 else rand) else val) (zip row [1..])
      else row
  in
    map (\(row, i) -> updateRow row i) (zip xs [1..])

initializeSource :: Int -> [[Int]] -> PriorityQueue
initializeSource id matrix = helperloop id (getLength matrix) 1

helperloop :: Int -> Int -> Int -> PriorityQueue
helperloop id total counter
    | counter > total = Empty
    | counter == 1 && id /= 1 = PQ (Node id 0) (addRest 1)
    | counter == id           = PQ (Node id 0) (addRest 1)
    | otherwise               = addRest 1
  where
    addRest i
        | i > total           = Empty
        | i == id             = addRest (i + 1)
        | otherwise           = PQ (Node i 9999) (addRest (i + 1))

getLength :: [[Int]]->Int
getLength [[]]=0
getLength (h:t)= length h


computeShortestPathCost :: Int -> [[Int]] -> PriorityQueue -> Int
computeShortestPathCost destid adjmatrix (PQ (Node u d) rest) =
  if u == destid
    then d
    else computeShortestPathCost destid adjmatrix
           (rebuildQueue rest (updateTempDistance u d adjmatrix))

updateTempDistance :: Int -> Int -> [[Int]] -> [(Int,Int)]
updateTempDistance u d adjm = updatekey d (neighbors u adjm)


rebuildQueue :: PriorityQueue -> [(Int,Int)] -> PriorityQueue
rebuildQueue Empty _ = Empty
rebuildQueue q tent =
  let (Node v oldD, rest) = dequeue q
      ds   = map snd (filter (\(u,_) -> u == v) tent)
      newD = if null ds 
             then oldD 
             else let d2 = head ds 
                  in if d2 < oldD then d2 else oldD
  in enqueue (Node v newD) (rebuildQueue rest tent)

updatekey :: Int -> [(Int, Int)] -> [(Int, Int)]
updatekey curKey =
  map (\(index, weight) -> (index, curKey + weight))

neighbors :: Int -> [[Int]] -> [(Int, Int)]
neighbors row matrix =
  getNeighborsFromRow 1 (matrix !! (row - 1))

getNeighborsFromRow :: Int -> [Int] -> [(Int, Int)]
getNeighborsFromRow _ [] = []
getNeighborsFromRow index (x:xs)
  | x >= 1 && x <= 9 = (index, x) : getNeighborsFromRow (index + 1) xs
  | otherwise        = getNeighborsFromRow (index + 1) xs
keyValue :: PQNode -> Int
keyValue (Node _ key) = key

enqueue :: PQNode -> PriorityQueue -> PriorityQueue
enqueue n Empty = PQ n Empty
enqueue n (PQ h t)
  | keyValue n <= keyValue h = PQ n (PQ h t)     
  | otherwise          = PQ h (enqueue n t)

dequeue :: PriorityQueue -> (PQNode, PriorityQueue)
dequeue Empty = error "Empty queue"
dequeue (PQ h t) = (h, t)
