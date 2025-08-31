# Overview


This project provides an implementation of the KenKen game that uses backtracking and utilizes the AC-3 algorithm to reduce the search space. 


## Game rules


In general, a KenKen example contains a square grid of size (N x N). Each row and each column has to contain each of the numbers 1 to N exactly once. Additionally, there are "cages", which are groups of positions in the grid that have to satisfy a certain rule, depending on the type of cage. The cage also contains a "target", which is a parameter for the rule. The types of cages are:
"Single" - Can contain only one cell. This cell has to contain the target value. 
"Add" - Can contain any number of cells. The values in these cells have to add up to the target. 
"Sub" - Has to contain exactly two cells. The absolute value of the difference of these cells has to be the target. 
"Mul" - Can contain any number of cells. The product of these cells has to be the target.
"Div" - Has to contain exactly two cells. After dividing the larger of these cells by the smaller one, we have to get the target. 
Every position in the grid has to be part of exactly one cage. More information can be found on [Wikipedia](https://en.wikipedia.org/wiki/KenKen). 


## Algorithm implementation


The algorithm uses a combination of backtracking and the AC-3 algorithm for CSP (constraint satisfaction problem) solving. The AC-3 algorithm is used to prune the domains of variables (positions in the grid) to reduce the search space. It also utilizes the MRV heuristic which first picks variables with the smallest domains. 


A solution to the KenKen problem in this program is represented by a map of domains for each position, where each of the domains is singleton (contains only one value). 


Here is a pseudocode that the solveKenKen function in the code is based on:
1. solve(problem):
2. &nbsp;&nbsp;&nbsp;&nbsp;If every domain in the problem has exactly one value:
3. &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Return those domains as the solution
4. &nbsp;&nbsp;&nbsp;&nbsp;Use the AC-3 algorithm to reduce the domains based on constraints
5. &nbsp;&nbsp;&nbsp;&nbsp;Select a variable x whose domain has more than one value, choosing the smallest such domain
6. &nbsp;&nbsp;&nbsp;&nbsp;For each value x\_value in the domain of x:
7. &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Create a new problem where x’s domain is restricted to just x\_value
8. &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;If this new problem is inconsistent:
9. &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Skip to the next x\_value
10. &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Otherwise, call solve recursively on the new problem
11. &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;If the recursive call returns a solution:
12. &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Return that solution
13. &nbsp;&nbsp;&nbsp;&nbsp;If no values work, return None


### AC-3 algorithm


A detailed description of the AC-3 algorithm can be found on [Wikipedia](https://en.wikipedia.org/wiki/AC-3\_algorithm). In short, it represents the CSP as a directed graph, where nodes are constrained variables and arcs represent constraints between these variables. Once this graph is created, the algorithm works as follows:
1. Initialize a queue that contains all arcs
2. While the queue is not empty:
3. &nbsp;&nbsp;&nbsp;&nbsp;Pop an arc (Xi, Xj)
4. &nbsp;&nbsp;&nbsp;&nbsp;From Xi's domain, remove all values for which there are no values in Xj's domain that satisfy the constraint
5. &nbsp;&nbsp;&nbsp;&nbsp;If any value has been removed from Xi's domain, add all arcs (Xk, Xi), where (k != j), to the queue


This implementation utilizes three types of arcs:
1. "not equal" arcs - these ensure that no two elements within the same row or column share the same value. 
2. "sum less than or equal to" arcs - these ensure that the sum of two elements within the same "Add" cage does not exceed that cage's target
3. "product is a divisor of" arcs - these ensure that the product of two elements within the same "Mul" cage is a divisor of that cage's target


# Requirements


GHC or GHCi (tested with GHCi 9.4.8), no external libraries


# Manuals


For information on how to run the program, see the [user manual](docs/user-manual.md). The implementation details are described in the [programmer manual](docs/programmer-manual.md). 

