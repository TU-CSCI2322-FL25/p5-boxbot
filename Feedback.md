Boxbot
Functionality (65/73 points)
* Game mechanics:                                 20 points
* Exact game solver:                              15 points
* Cut-off depth solver:                           12 points
   * is makeMove returns Nothing, you should probably not score it at all!
* Reasonable evaluation function:                 2 points
* Avoiding unnecessary work:                      0/3 points
   * Not done
* Command-line interface:                         9/10 points
   * Didn’t use usageInfo
   * Doesn’t print moves in the format it reads them in
* Move and verbose flags:                         3/5 points
   * Move can’t specify direction!
   * Verbose: -m, -d but calls whoWillWin, 
* Error-handling:                                 3/5 points
   * -m doesn’t check errors
   * readGame, makeMove do
   * goodMove/bestMove don’t check if the game is over, bestMove crashes if it is.
* Makefile:                                       1 point

Design (25/27 points)
* Well-designed data types                        7/8 points
   * Winner has ‘Ongoing’ still in it.
* Well-decomposed functions                       9/10 points
   * Main is heavily decomposed, but very clunky and not uniformly decomposed. 
   * readGame repeats code (readPlayer) and has do-blocks in list comprehensions.
   * Otherwise very good function design, despite the horrible spacing of ‘let’ and ‘in’. Great variables, never too much on one line.
* Good module decomposition                       2 points
* Good variable names                             2 points
* Efficient/idiomatic code                        5 points
