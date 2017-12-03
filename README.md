# Functional Programming: Assignment 1

## Introduction
This is an exploration game where the player have to find as many treasures as possible in a weird desert where you can find sand, but also water and lava !?! To make it even stranger, there exists also portals teleporting you to the exit.

## How to build
It is highly recommended to use *[Stack](https://www.stackage.org/)* to build this game.
Inside the folder containing the game, simply use

    stack setup
    stack build

to compile it and use

    stack exec DesertExplorer

to play this awesome game.

## Notice
At first, a menu will ask the player for
 1. **sight**: it represents the number of tiles the player can see in all directions.
 2. **max water**: maximal quantity of water supplies (when this reach 0, the player instantly dies).
 3. **seed**: a number that is used to generate the desert.
 4. **treasure likelihood**: percentage (1 - 99) of chance that a treasure is hidden in a Sand tile.
 4. **XXX likelihood**: a percentage (1 - 99) representing the probability of founding a XXX tile in the desert (lava adjacent is for tiles probability to find a lava tile next to another lava tile).

This menu will repeat itself until the player specify a valid configuration.

When the game start, the desert will show limited to the sight specified in the configuration. Desert uses ASCII characters representing:
 - **.** : a sand tile, which may or not contain a treasure
 - **_** : a water tile, to refill the water supplies
 - **~** : a lava tile, instant death
 - **!** : a portal, ending the game winning all found treasures
 - **P** : position of the player

Below the desert, informations are displayed:
 - number of collected treasures
 - remaining water
 - distances to closest water, treasure and portal

Distances uses the "*Maybe*" notation, meaning it can be
 - Just X : if distance is X from player position
 - Nothing : if theres is no reachable corresponding tile.

To move the explorer in the desert, player will use keys "w", "a", "s" and "d" to go respectively up, left, down and right. Each move costs 1 supply of water.

## Strictness evaluation
Distances uses a simple BFS implementation using a *State* where the queue is from *Data.Sequence*, a list with efficient insertion and retrieval from both ends. Lazy and strict implementations are exactly the same, with *State* coming either from *Control.Monad.State* or *Control.Monad.State.Strict*. Strict uses also uses *BangPatterns* pragma to immediately evaluate *"!arguments"*. All arguments are bang-patterned, except for the desert, obviously.

Configuration used for testing is

    sight: 10
    max water: 10
    seed: 42
    treasure likelihood: 1
    water likelihood: 1
    portal likelihood: 1
    lava likelihood: 20
    lava (adjacent) likelihood: 60

The game is executed until the three "closest" are computed, then stoped by SIGKILL. Results are on files "*DesertExplorer.Lazy.ps*" "*DesertExplorer.Strict.ps*".

If the reader want to generate those graphics, he should compile the sources using

    stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"

and execute with

    stack exec -- DesertExplorer +RTS -h -i0.01
