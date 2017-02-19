# Rummy, in Haskell

[![Build Status](https://travis-ci.org/gladed/haskell-rummy.svg?branch=master)](https://travis-ci.org/gladed/haskell-rummy)

An implementation of the game of Rummy.

For a complete introduction of the rules to this card game, see https://cardgames.io/rummy.

## Project Status

This is a learning project for me. At this time, all game rules are modelled. This means that at
any point in the game, it's possible to discover and apply any legal moves to reach the next point.

In addition, there is an interactive text version of the game. It assumes two players take turns.

## Next steps

* Create a series of AIs and let them train against one another
* Allow a human user to play against those AIs
* Try out UI/web frameworks, or expose games through a REST API.

## Cheat-Sheet
    stack test        # Run tests
    stack build       # Create an executable
    stack exec card   # Run the executable after building it.
    stack ghci        # Open REPL to play with things
