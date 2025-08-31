module KenKenSolver where

-- Import necessary libraries for removing list duplicates, Maybe handling, and Map data structure
import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

-- Type aliases for clarity
type Position = (Int, Int)  -- Represents a cell in the grid (row, column)
type Domain = [Int]  -- Possible values for a cell
type Domains = Map.Map Position Domain  -- Maps each position to its domain
type Constraint = Int -> Int -> Bool  -- A binary constraint between two values

-- Data type for the operation in a cage
data Operation = Single | Add | Sub | Mul | Div
  deriving (Eq)

-- Data type for a cage: operation, target value, and list of positions
data Cage = Cage {
  operation :: Operation,
  target :: Int,
  positions :: [Position]
}

-- Data type for the KenKen CSP: grid size, list of cages, and domains for each position
data KenKenCSP = KenKenCSP {
  size :: Int,
  cages :: [Cage],
  domains :: Domains
}

-- Data type for an arc in the AC-3 algorithm: constraint, from position, to position
data Arc = Arc Constraint Position Position

-- ======================
-- HELPER FUNCTIONS
-- ======================

-- Generate all positions in an n x n grid
getAllPositions :: Int -> [Position]
getAllPositions n = [(i, j) | i <- [1..n], j <- [1..n]]

-- Check if a domain is a singleton (only one possible value)
isSingleton :: Domain -> Bool
isSingleton domain = length domain == 1

-- Check if all domains are singletons (solution is complete)
isComplete :: Domains -> Bool
isComplete domains = all isSingleton (Map.elems domains)

-- Extract the value from a singleton domain, if possible
getValueOf :: Domain -> Maybe Int
getValueOf [x] = Just x
getValueOf _   = Nothing

-- Get all positions in the same row as the given position
getRowOf :: Int -> Position -> [Position]
getRowOf n (r, c) = [(r, c') | c' <- [1..n], c' /= c]

-- Get all positions in the same column as the given position
getColumnOf :: Int -> Position -> [Position]
getColumnOf n (r, c) = [(r', c) | r' <- [1..n], r' /= r]

-- Get all positions in the same row and column as the given position
getRowColOf :: Int -> Position -> [Position]
getRowColOf n pos = getRowOf n pos ++ getColumnOf n pos

-- Assign a value to a position, reducing its domain to a singleton
assignValue :: Position -> Int -> Domains -> Domains
assignValue pos value = Map.insert pos [value]

-- Get the domains for a list of positions
getDomainsOf :: Domains -> [Position] -> [Domain]
getDomainsOf domains positions = map (\position -> Map.findWithDefault [] position domains) positions

-- Compute the Cartesian product of a list of lists
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct []       = [[]]
cartesianProduct (xs:xss) = [x:ys | x <- xs, ys <- cartesianProduct xss]

-- Select the next variable to assign using the Minimum Remaining Values (MRV) heuristic
selectVariable :: Domains -> Maybe Position
selectVariable domains =
  case [ (pos, dom) | (pos, dom) <- Map.toList domains, not (isSingleton dom) ] of
    [] -> Nothing  -- All variables are assigned
    xs -> Just $ fst $ minimumByLen xs
  where
    -- Find the position with the smallest domain
    minimumByLen = foldl1 (\a@(_,dom1) b@(_,dom2) -> if length dom1 <= length dom2 then a else b)

-- ======================
-- KENKEN CSP CREATION
-- ======================

-- Create a KenKen CSP with initial domains [1..n] for each position
createKenKen :: Int -> [Cage] -> KenKenCSP
createKenKen n cageList = KenKenCSP {
  size = n,
  cages = cageList,
  domains = Map.fromList [(pos, [1..n]) | pos <- getAllPositions n]
}

-- ======================
-- CONSISTENCY CHECKING
-- ======================

-- Check that all assigned values in rows and columns are unique
rowsColsConsistent :: Int -> Domains -> Bool
rowsColsConsistent n domains =
  all allDifferent (extractAssignedValues getAllRows) &&
  all allDifferent (extractAssignedValues getAllColumns)
  where
    positions = getAllPositions n
    -- Generate all rows and columns
    getAllRows = [ [(r,c) | c <- [1..n]] | r <- [1..n] ]
    getAllColumns = [ [(r,c) | r <- [1..n]] | c <- [1..n] ]
    -- Extract assigned values from groups (rows or columns)
    extractAssignedValues groups = [ catMaybes [ getValueOf (Map.findWithDefault [] p domains) | p <- g ] | g <- groups ]
    -- Check if all values in a list are unique
    allDifferent vs = length vs == length (nub vs)

-- Check if a cage's current domain assignments are feasible
cageConsistent :: Cage -> Domains -> Bool
cageConsistent (Cage operation target positions) domains =
  case operation of
    -- For Single cages, the cell must equal the target
    Single -> case positions of
      [pos] -> let domain = Map.findWithDefault [] pos domains
               in not (null domain) && target `elem` domain
      _    -> False  -- Malformed Single cage (should have exactly one position)
    -- For Add cages, check if the sum of possible values can reach the target
    Add    -> feasibleAdd
    -- For Mul cages, check if the product of possible values can reach the target
    Mul    -> feasibleMul
    -- For Sub cages, check if the difference of possible values can reach the target
    Sub    -> feasibleSub
    -- For Div cages, check if the division of possible values can reach the target
    Div    -> feasibleDiv
  where
    cageDomains = getDomainsOf domains positions
    assignedVals = mapM getValueOf cageDomains  -- Try to get assigned values
    -- Check feasibility for Add cages
    feasibleAdd =
      let
        minSum = sum (map minimum cageDomains)
        maxSum = sum (map maximum cageDomains)
      in (target >= minSum && target <= maxSum) &&
         (case assignedVals of
           Just values -> sum values == target
           Nothing -> any ((== target) . sum) (cartesianProduct cageDomains))
    -- Check feasibility for Mul cages
    feasibleMul =
      let
        minProd = product (map minimum cageDomains)
        maxProd = product (map maximum cageDomains)
        withinBounds = target >= minProd && target <= maxProd
      in withinBounds &&
         (case assignedVals of
           Just vs -> product vs == target
           Nothing -> any ((== target) . product) (cartesianProduct cageDomains))
    -- Check feasibility for Sub cages (binary only)
    feasibleSub =
      case positions of
        [pos1,pos2] ->
          let dom1 = Map.findWithDefault [] pos1 domains
              dom2 = Map.findWithDefault [] pos2 domains
          in not (null dom1) && not (null dom2) &&
             any (\(a,b) -> abs (a-b) == target) [ (a,b) | a <- dom1, b <- dom2 ]
        _ -> False  -- Subtraction cages must be binary
    -- Check feasibility for Div cages (binary only)
    feasibleDiv =
      case positions of
        [pos1,pos2] ->
          let dom1 = Map.findWithDefault [] pos1 domains
              dom2 = Map.findWithDefault [] pos2 domains
              goodPair a b =
                (a /= 0 && b /= 0) &&
                (a >= b && a `mod` b == 0 && a `div` b == target ||
                 b >  a && b `mod` a == 0 && b `div` a == target)
          in not (null dom1) && not (null dom2) &&
             any (uncurry goodPair) [ (a,b) | a <- dom1, b <- dom2 ]
        _ -> False  -- Division cages must be binary

-- Check global consistency: rows/columns and all cages must be feasible
consistent :: KenKenCSP -> Domains -> Bool
consistent csp domains =
  rowsColsConsistent (size csp) domains &&
  all (\cage -> cageConsistent cage domains) (cages csp)

-- ======================
-- CONSTRAINTS FOR AC-3
-- ======================

-- Constraint: a + b <= n (for Add cages)
sumLeq :: Int -> Constraint
sumLeq n a b = a + b <= n

-- Constraint: n is divisible by (a * b) (for Mul cages)
productZeroRemainder :: Int -> Constraint
productZeroRemainder n a b =
  let abProduct = a * b
  in n `mod` abProduct == 0

-- Constraint: a /= b (for row/column uniqueness)
neq :: Constraint
neq = (/=)

-- ======================
-- ARC CREATION FOR AC-3
-- ======================

-- Create arcs for row/column uniqueness constraints
createNeqArcs :: KenKenCSP -> [Arc]
createNeqArcs csp =
  [ Arc neq pos1 pos2 |
    pos1 <- getAllPositions (size csp),
    pos2 <- getRowColOf (size csp) pos1
  ]

-- Create arcs for Add cage constraints
createSumArcs :: KenKenCSP -> [Arc]
createSumArcs csp =
  [ Arc (sumLeq (target cage)) pos1 pos2 |
    cage <- cages csp,
    operation cage == Add,
    pos1 <- positions cage,
    pos2 <- positions cage,
    pos1 /= pos2
  ]

-- Create arcs for Mul cage constraints
createProductArcs :: KenKenCSP -> [Arc]
createProductArcs csp =
  [ Arc (productZeroRemainder (target cage)) pos1 pos2 |
    cage <- cages csp,
    operation cage == Mul,
    pos1 <- positions cage,
    pos2 <- positions cage,
    pos1 /= pos2
  ]

-- Create all arcs for AC-3
createArcs :: KenKenCSP -> [Arc]
createArcs csp = createNeqArcs csp ++ createSumArcs csp ++ createProductArcs csp

-- ======================
-- AC-3 ALGORITHM
-- ======================

-- Reduce the domain of a variable based on an arc
reduceDomain :: Arc -> Domains -> Maybe (Domains, Bool)
reduceDomain (Arc constraint pos1 pos2) domains =
  case (Map.lookup pos1 domains, Map.lookup pos2 domains) of
    (Just domain1, Just domain2) ->
      let
        newDomain1 = [x | x <- domain1, any (constraint x) domain2]
      in
        if null newDomain1
          then Nothing  -- Inconsistency detected
          else Just (Map.insert pos1 newDomain1 domains, length newDomain1 < length domain1)
    _ -> Just (domains, False)  -- Should not happen

-- Propagate constraints using AC-3
propagateDomains :: [Arc] -> [Arc] -> Domains -> Maybe Domains
propagateDomains _ [] domains = Just domains
propagateDomains allArcs (arc:arcs) domains =
  case reduceDomain arc domains of
    Nothing -> Nothing  -- Inconsistency detected
    Just (newDomains, changed) ->
      if changed
        then
          let
            -- Add new arcs to the queue if the domain of pos1 was reduced
            newArcs = arcs ++ [Arc constraint posN pos1 |
              Arc constraint posN pos1 <- allArcs, pos1 == arcFrom arc, posN /= arcTo arc]
          in propagateDomains allArcs newArcs newDomains
        else propagateDomains allArcs arcs newDomains
  where
    arcFrom (Arc _ from _) = from
    arcTo (Arc _ _ to) = to

-- Run AC-3 on the CSP
ac3 :: KenKenCSP -> Maybe KenKenCSP
ac3 csp =
  let allArcs = createArcs csp
  in case propagateDomains allArcs allArcs (domains csp) of
    Nothing -> Nothing  -- Inconsistency detected
    Just newDomains -> Just csp { domains = newDomains }

-- ======================
-- BACKTRACKING SEARCH
-- ======================

-- Solve the KenKen puzzle using backtracking with AC-3 and MRV
solveKenKen :: KenKenCSP -> Maybe Domains
solveKenKen csp0 = do
  -- Apply AC-3 to reduce domains
  csp1 <- ac3 csp0
  let ds1 = domains csp1
  if isComplete ds1
    then if consistent csp1 ds1 then Just ds1 else Nothing
    else case selectVariable ds1 of
      Nothing -> Nothing  -- No solution found
      Just var ->
        let tryVals [] = Nothing
            tryVals (v:vs) =
              let ds2  = assignValue var v ds1
                  csp2 = csp1 { domains = ds2 }
              in if not (consistent csp2 ds2)
                 then tryVals vs  -- Skip inconsistent assignments
                 else case solveKenKen csp2 of
                   Just sol -> Just sol  -- Solution found
                   Nothing  -> tryVals vs  -- Backtrack
        in tryVals (Map.findWithDefault [] var ds1)

-- ======================
-- INPUTS
-- ======================

solvable3x3 :: KenKenCSP
solvable3x3 = createKenKen 3 
  [ Cage Add 3 [(1, 1), (1, 2)],
    Cage Single 3  [(1, 3)],
    Cage Single 3  [(2, 1)],
    Cage Add 4  [(2, 2), (2, 3), (3, 3)],
    Cage Add 5 [(3, 1), (3, 2)]
  ]
  
unsolvable3x3 :: KenKenCSP
unsolvable3x3 = createKenKen 3 
  [ Cage Add 3 [(1, 1), (1, 2)],
    Cage Single 2  [(1, 3)],
    Cage Single 3  [(2, 1)],
    Cage Add 4  [(2, 2), (2, 3), (3, 3)],
    Cage Add 5 [(3, 1), (3, 2)]
  ]

manySolutions3x3 :: KenKenCSP
manySolutions3x3 = createKenKen 3 
  [ Cage Add 18 $ getAllPositions 3 ]

solvable4x4 :: KenKenCSP
solvable4x4 = createKenKen 4
  [ Cage Mul 12 [(1, 1), (1, 2), (2, 2)],
    Cage Div 2  [(1, 3), (1, 4)],
    Cage Add 11 [(2, 1), (3, 1), (4, 1), (3, 2)],
    Cage Sub 2  [(2, 3), (3, 3)],
    Cage Single 2 [(2, 4)],
    Cage Add 4  [(3, 4), (4, 4)],
    Cage Sub 3  [(4, 2), (4, 3)]
  ]
  
unsolvable4x4 :: KenKenCSP
unsolvable4x4 = createKenKen 4
  [ Cage Mul 12 [(1, 1), (1, 2), (2, 2)],
    Cage Div 2  [(1, 3), (1, 4)],
    Cage Add 11 [(2, 1), (3, 1), (4, 1), (3, 2)],
    Cage Sub 2  [(2, 3), (3, 3)],
    Cage Single 1 [(2, 4)],
    Cage Add 4  [(3, 4), (4, 4)],
    Cage Sub 3  [(4, 2), (4, 3)]
  ]
  
manySolutions4x4 :: KenKenCSP
manySolutions4x4 = createKenKen 4 
  [ Cage Add 40 $ getAllPositions 4 ]

-- ======================
-- MAIN FUNCTION
-- ======================

-- Solve a KenKenCSP and print the solution
solveAndPrint :: KenKenCSP -> IO ()
solveAndPrint csp = do
  let solution = solveKenKen csp
  case solution of
    Nothing  -> putStrLn "No solution found."
    Just domains -> do
      let getVal pos =
            case Map.lookup pos domains of
              Just [x] -> show x
              _        -> "?"
          rows = [ [ getVal (r, c) | c <- [1..(size csp)] ] | r <- [1..(size csp)] ]
      mapM_ (putStrLn . unwords) rows

-- solve and print a solvable 4x4 example
main :: IO ()
main = solveAndPrint solvable4x4

-- ======================
-- TEST FUNCTION
-- ======================

-- Run a test and return its result in a string format
testKenKen :: String -> KenKenCSP -> Bool -> String
testKenKen testName csp solvable =
  let result = solveKenKen csp
  in case (result, solvable) of
       (Nothing, False) ->
         testName ++ ": PASS (correctly identified that a solution does not exist)"
       (Nothing, True) ->
         testName ++ ": FAIL (did not find any solution, solution exists)"
       (Just solution, True) ->
         if consistent csp solution
           then testName ++ ": PASS (found valid solution)"
           else testName ++ ": FAIL (found invalid solution, a different solution exists)"
       (Just solution, False) ->
         if consistent csp solution
           then testName ++ ": TEST INCORRECT (found valid solution, no solution expected)"
           else testName ++ ": FAIL (found invalid solution, no solution exists)"


-- Run all tests and print their results
runTests :: IO ()
runTests = do
  putStrLn $ testKenKen "Solvable 3x3" solvable3x3 True
  putStrLn $ testKenKen "Unsolvable 3x3" unsolvable3x3 False
  putStrLn $ testKenKen "Many solutions 3x3" manySolutions3x3 True
  putStrLn $ testKenKen "Solvable 4x4" solvable4x4 True
  putStrLn $ testKenKen "Unsolvable 4x4" unsolvable4x4 False
  putStrLn $ testKenKen "Many solutions 4x4" manySolutions4x4 True
