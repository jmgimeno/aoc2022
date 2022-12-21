# TODOs

## Day 1

- create a ZPipeline to transform the String stream to the sums stream

## Day 7

- use an immutable FileSystem implemented as a Zipper
- then use a ZRef to hold a mutable reference to it

## Day 11

- use a single map for everything and use optics for access/update

## Day 12

- implement a min-heap that allows to change priority to use in A*

## Day 13

- use zio-parse

## Day 14

- organize segments into a tree to not use all of them for each point
- or simply use a set of points for all of them

## Day 15

- intersection-trees for part1
- in part1 sort ranges by x coord and then scan from left to right fusing
- optimize via not building the whole set of candidates

## Day 16

- unify code between parts 1 and 2
- find a better bound for part2
  - to solve the puzzle I have used the `bestImprovement` for part1 but I feel it's not correct
  - the one I feel is correct makes the the search space too big

## Day 17

- Possible design:
  - use bytes for the lines of each piece
  - and a ListBuffer[Byte] for the screen
  - and you'll have one byte free maybe to use in part2

## Day 18

- use <https://www.geeksforgeeks.org/how-to-check-if-a-given-point-lies-inside-a-polygon/> in 3D
- the code needs lots of love & polish

## Day 19

- use A* or BB or any other algorithm (my first approximation) I think it is not the way to go
- think of something else: e.g. maximize a cost funcion under linear (maybe they are) restrictions
  - <https://github.com/vagmcs/Optimus>
  - <https://coin-or.github.io/pulp/CaseStudies/a_two_stage_production_planning_problem.html>
- back to the design table

## Day 20

- two indices (one and its inverse) to track the positions of each element
  - mum:   // immutable
  - index: position of num(i) in mixed // to know where each number is
  - inv:   to know the positions in index with a given value
- permutation group?

## Day 21

- find another way to define the domain (traits, normal classes & extractors)
