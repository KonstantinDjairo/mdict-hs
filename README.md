# mdict-hs

a mdict library for haskell

first build [mdict-cpp](https://github.com/dictlab/mdict-cpp)

then in the folder there will be some *.a files, which are the library files that we need.
in the example below, we refer to that folder containing those files, as "lib"
### build
```shell
ghc -O2 -Wall -fno-warn-unused-imports \
    -lmdict -lminilzo -lminiz \
    -L./lib \
    MDict.hs
```    