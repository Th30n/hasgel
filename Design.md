# Design Specification

An overview of features with some implementation details.

## Ship movement

Area is fixed for player and alien ships.

Fixed speed of movement, no acceleration.

Player movement controlled by input.

Alien ships move left-right and down. 5 rows of alien ships, movement is from
the bottom row up. Destroying an alien ship speeds up the movement and
increases fire rate of other alien ships.

## Shooting

Player shooting controlled via input. Fixed rate of fire, shots have no
acceleration and move with fixed speed.

Alien ships shoot if they have a free line of fire (no other alien ships
blocking).

## Collisions

List of collisions to check:

  * player - alien laser shot
  * alien ship - player laser shot
  * blockade - laser shot

## Destructible Blockades

A grid of cube meshes which are removed as each cube gets hit.
Perhaps look into volume rendering, e.g. marching cubes.

## Demos

Create commands for the player, shooting commands for alien ships and a
command for spawning the special alien ship. Store those commands and allow
replaying them. The rest (movement of alien ships) should be deterministic.
