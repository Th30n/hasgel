# hasgel

My project for learning Haskell and OpenGL.

The project aims to implement the classic game, Space Invaders. As this is my
first larger project with Haskell, the scope of the game will (hopefully) be
kept at the minimum.

## Planned Features

The following features are planned. For a more detailed overview, check the
`Design.md` file.

  * A 3D model for player.
  * A model for regular alien ships and another model for the special ship.
  * Simple shooting animation.
  * A laser shot effect.
  * An explosion animation.
  * Destructible, defensive blockades.
  * Scoreboard
  * Demo recording and playing.
  * Rebindable keys.

## Building & Running

The source has only been tested on GNU/Linux and Glasgow Haskell Compiler. In
order to build and run you will need the following:

  * SDL 2 library
  * GPU drivers supporting OpenGL 4
  * `ghc` - Glasgow Haskell Compiler
  * `cabal-install` - tool for installing and building Haskell packages
    (usually installed with `ghc`)
  * `stack` - tool for Haskell projects; can be installed using `cabal install
    stack`

This project uses `stack` for building. It will download the required versions
of other Haskell packages which are known to work with the project. `stack`
will also handle that the appropriate `ghc` version is used for the project.

Build steps are as follows:

    stack setup
    stack build

After the compilation is done, the main application can be run with:

    stack exec hasgel
