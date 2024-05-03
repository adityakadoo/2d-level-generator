# 2d-level-generator
A procedural 2D level generation library utilizing the Wave-Function Collapse (WFC) algorithm in Haskell programming language.

The library comes with all the necessary utilities to process various tile-sets and their constraints. This library can also visualize the process of generation of the 2-dimensional game layouts through a graphical interface. The generation strategies can be switched and compared easily.

## Directory Structure

```
.
├── build/
├── Makefile
├── resources/
│   ├── shaders/
│   │   ├── shader.frag
│   │   └── shader.vert
│   └── tilesets/
│       └── [example-tileset]/
│           ├── constraints.xml
│           └── whole.png
└── src/
    ├── LoadShaders.hs
    ├── Main.hs
    ├── Randomness.hs
    ├── Strategies.hs
    ├── TilesetLoader.hs
    └── WaveFuncCollapse.hs
```

## Instructions

- Compile: Creates a binary.

```
>>> make compile
```

- Run: Executes the binary with following arguments,
  - `SEED` is an integer that controls the pseudo-random number generator.
  - `TILESET` can be chosen from `["circuit", "castle", "floorplan", "circles"]`.

```
>>> make run SEED="123" TILESET="circuit"
```

- Debug: Starts a GHCI session with all the program files loaded.

```
>>> make load
```
