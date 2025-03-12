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


### running the cli 
we do provide a example of how to use this library, as a cli

```shell
 ghc -L./lib -lmdict -lminiz -lstdc++ Main.hs -o my_program
 ```
 the command above assumes that the lib folder contain the *.a files mentioned before, and that you also already compiled the haskell library itself and that this Main.hs file is placed in the same folder as the compiled library.