# 2d-level-generator
A procedural 2D level generation library utilizing the Wave-Function
Collapse (WFC) algorithm in Haskell programming language.
The library comes with all the necessary utilities to process various tile-sets and their
constraints. This library can also visualize the process of generation of the 2-dimensional game
layouts through a graphical interface. The generation strategies can be switched and compared
easily.

# WFC Algorithm Repository
The original [code repository](https://github.com/mxgmn/WaveFunctionCollapse/tree/master)  by Maxim Gumin has the WFC algorithm implemented in
C# programming language. This code base has been translated to several other
programming languages such as C++, Python, Rust, Go, Java, JavaScript, etc. but not to Haskell! </br>
This repository also has a dataset of example tile-sets. These tile-sets have a set of tile
images accompanied by an XML file that stores the constraints between neighbouring
tiles and the tile weights to be used while random sampling. These tile-sets are used for
generating our simulations as well.


