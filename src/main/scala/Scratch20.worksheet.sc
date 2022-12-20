/*

Cycles have module 6 (7 - 1)

Initial arrangement:
  elems:  1,  2, -3,  3, -2,  0,  4  -> initial array
  posi :  0   1   2   3   4   5   6  -> elem(posi(i)) -> element at posi i in the permutation
  perm:   1,  2, -3,  3, -2,  0,  4

1 moves between 2 and -3:
  elems:  1,  2, -3,  3, -2,  0,  4
  posi :  1   0   2   3   4   5   6
  perm :  2,  1, -3,  3, -2,  0,  4

2 moves between -3 and 3:
  elems:  1,  2, -3,  3, -2,  0,  4
  posi :  0   2   1   3   4   5   6
  elems:  1, -3,  2,  3, -2,  0,  4

-3 moves between -2 and 0:
  elems:  1,  2, -3,  3, -2,  0,  4
  posi :  0   1   3   4   2   5   6
  perm:   1,  2,  3, -2, -3,  0,  4

3 moves between 0 and 4:
  elems:  1,  2, -3,  3, -2,  0,  4
  posi :  0   1   4   2   5   3   6
  perm:   1,  2, -2, -3,  0,  3,  4

-2 moves between 4 and 1:
  elems:  1,  2, -3,  3, -2,  0,  4
  posi :  0   1   2   5   3   6   4
  perm:   1,  2, -3,  0,  3,  4, -2

0 does not move:
  elems:  1,  2, -3,  3, -2,  0,  4
  posi :  0   1   2   5   3   6   4
  perm:   1,  2, -3,  0,  3,  4, -2

4 moves between -3 and 0:
  elems:  1,  2, -3,  3, -2,  0,  4
  posi :  0   1   2   6   5   3   4
  perm :  1,  2, -3,  4,  0,  3, -2

 */
