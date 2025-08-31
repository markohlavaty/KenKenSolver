# User manual


This tutorial assumes that the reader is familiar with [the main README](../README.md)


## Running the program


To run the program, you have to run the solve.hs file. This can be done either by compiling it with GHC, or by loading it with GHCi. 


## Solving predefined examples


The code contains six predefined examples (solvable3x3, unsolvable3x3, manySolutions3x3, solvable4x4, unsolvable4x4, manySolutions4x4) that the user can solve simply by typing `solveAndPrint EXAMPLE_NAME`, for example `solveAndPrint solvable4x4`. 


## Solving new examples


The user can also define and solve new examples by typing `solveAndPrint $ createKenKen N CAGES`, where N is the size of the grid along one dimension, and CAGES is list of cages. Each of the cages is defined in the form `Cage OPERATION TARGET POSITIONS`, where positions is a list of positions in the grid, given as tuples. For example, 
`solveAndPrint $ createKenKen 4 [ Cage Mul 12 [(1, 1), (1, 2), (2, 2)], Cage Div 2  [(1, 3), (1, 4)], Cage Add 11 [(2, 1), (3, 1), (4, 1), (3, 2)], Cage Sub 2  [(2, 3), (3, 3)], Cage Single 2 [(2, 4)], Cage Add 4  [(3, 4), (4, 4)], Cage Sub 3  [(4, 2), (4, 3)] ]`. 


## Main function


The main function just calls `solveAndPrint solvable4x4`. It is intended for a very quick demo of the program solving a KenKen problem. 


