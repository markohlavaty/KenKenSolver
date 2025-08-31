# Programmer manual


This tutorial assumes that the reader is familiar with [the main README](../README.md)


## Testing


Tests utilize six predefined examples (solvable3x3, unsolvable3x3, manySolutions3x3, solvable4x4, unsolvable4x4, manySolutions4x4). 


To add more examples to the tests, first add their definitions to the INPUTS section, and then add them to the runTests function in the form `putStrLn $ testKenKen STRING_NAME DEFINITION_NAME IS_SOLVABLE`, where STRING_NAME is the name that will be printed in test result reports, DEFINITION_NAME is the name under which the example is defined in the INPUTS section, and IS_SOLVABLE is a boolean value. 


To run the tests, simply call runTests. 


## Structure


### Types


The types should be self-explanatory. 
```
type Position = (Int, Int)  -- Represents a cell in the grid (row, column)
type Domain = [Int]  -- Possible values for a cell
type Domains = Map.Map Position Domain  -- Maps each position to its domain
type Constraint = Int -> Int -> Bool  -- A binary constraint between two values
```


### Data


data Operation represents the type of operation in a cage.
```
data Operation = Single | Add | Sub | Mul | Div
  deriving (Eq)
```

data Cage represents one cage: it stores its operation, target, and positions.
```
data Cage = Cage {
  operation :: Operation,
  target :: Int,
  positions :: [Position]
}
```

data KenKenCSP represents a KenKen grid with its size, all of its cages, and domains for all positions. KenKenCSP's domains are narrowing down as the solution process progresses.
```
data KenKenCSP = KenKenCSP {
  size :: Int,
  cages :: [Cage],
  domains :: Domains
}
```

data Arc represents an arc in the AC-3 graph, with its constraint and its source and target positions. 
```
data Arc = Arc Constraint Position Position
```


### Helper functions


Generate all positions of a grid of a given size. 
`getAllPositions :: Int -> [Position]`


Check if a domain contains one value only. 
`isSingleton :: Domain -> Bool`


Check if all domains contain one value only. This means that the assignment is complete. 
`isComplete :: Domains -> Bool`


Extract value of a single-element domain, if possible. 
`getValueOf :: Domain -> Maybe Int`


Get all positions in the same row as the given position, excluding that position. 
`getRowOf :: Int -> Position -> [Position]`


Get all positions in the same column as the given position, excluding that position. 
`getColumnOf :: Int -> Position -> [Position]`


Get all positions in the same row and column as the given position, excluding that position. 
`getRowColOf :: Int -> Position -> [Position]`


Assign a value to a position, reducing its domain to one element. 
`assignValue :: Position -> Int -> Domains -> Domains`


Get the domains for a list of positions in order. 
`getDomainsOf :: Domains -> [Position] -> [Domain]`


Compute the Cartesian product of a list of lists. If there are n lists, all elements of the cartesian product will contain n elements. 
`cartesianProduct :: [[a]] -> [[a]]`


Select the next variable to assign. Choose the variable with an unassigned value with the smallest number of possible values. 
`selectVariable :: Domains -> Maybe Position`


### KenKen CSP creation


Create KenKenCSP of an (n x n) grid with initial domains [1..n] for each position. 
`createKenKen :: Int -> [Cage] -> KenKenCSP`


### Consistency checking


Check that there are no duplicit values in any of the rows and columns. 
`rowsColsConsistent :: Int -> Domains -> Bool`


Check if a cage's current domain assignments are feasible. This function is decomposed into five cases, one for each type of operation. 
`cageConsistent :: Cage -> Domains -> Bool`


Check whether all rows, columns and cages are feasible for a given KenKenCSP. 
`consistent :: KenKenCSP -> Domains -> Bool`


### AC-3 constraints


Based on a parameter, produces a Constraint that ensures that the sum of two numbers is less than or equal to this parameter. 
`sumLeq :: Int -> Constraint`


Based on a parameter, produces a Constraint that ensures that the product of two numbers is a divisor of this parameter. 
`productZeroRemainder :: Int -> Constraint`


A constraint that ensures inequality of two elements. 
`neq :: Constraint`


### AC-3 arc creation


Create arcs for constraints ensuring that all entries in every row and every column are unique. 
`createNeqArcs :: KenKenCSP -> [Arc]`


Create arcs ensuring that the sum of two elements within a cage does not exceed that cage's target. 
`createSumArcs :: KenKenCSP -> [Arc]`


Create arcs ensuring that the product of two elements within a cage is a divisor of that cage's target. 
`createProductArcs :: KenKenCSP -> [Arc]`


Create all arcs for AC-3. 
`createArcs :: KenKenCSP -> [Arc]`


### AC-3 core


Reduce the domain of a variable based on an arc. Return the new domains, along with a boolean value indicating whether the domains have been updated. 
`reduceDomain :: Arc -> Domains -> Maybe (Domains, Bool)`


Propagate constraints to reduce domains using the AC-3 algorithm. 
`propagateDomains :: [Arc] -> [Arc] -> Domains -> Maybe Domains`


Reduce the domains for the CSP using the AC-3 algorithm and return a new CSP with the resuced domains. 
`ac3 :: KenKenCSP -> Maybe KenKenCSP`


### Backtracking


Solve the KenKen puzzle and return its domains reduced to one element per position. 
`solveKenKen :: KenKenCSP -> Maybe Domains`


### Main


Solve a KenKenCSP and print the solution. 
`solveAndPrint :: KenKenCSP -> IO ()`


Solve a solvable 4x4 example and print its solution. 
`main :: IO ()`


### Tests


Run a test of the solver on a KenKenCSP example, given information about whether the example is solvable. Return the result of the test as a String. 
`testKenKen :: String -> KenKenCSP -> Bool -> String`


Run all tests and print their results. 
`runTests :: IO ()`

