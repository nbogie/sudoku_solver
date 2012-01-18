What is it?
===========

A private learning exercise.

A haskell beginner's attempt to port Peter Norvig's Sudoku solver to Haskell from the original Python found at http://norvig.com/sudoku.html

Warning: Hazardous!
===================

This is not an example of good Haskell!  I do not recommend learning from it.

Better implementations
======================

If you are looking for a good sudoku solver in Haskell, you will find various on the Haskell wiki: http://www.haskell.org/haskellwiki/Sudoku

Norvig already links to a haskell implementation of his approach: http://www.haskell.org/pipermail/haskell-cafe/2007-August/031049.html


Usage
=====

(Still here?)

To use it as a library, see:

    solve :: String -> Maybe Board
    display :: Board -> String

To use it as a stand-alone program:

run the script and feed it puzzle definitions on stdin.

    runhaskell Sudoku.hs < top95.txt

Both cases require the same one-puzzle-per-line format as seen in Norvig's write-up.

Example: 

    4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......