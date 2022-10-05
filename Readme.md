[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/zip-codec/Test)](https://github.com/jappeace/zip-codec/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)
[![Hackage version](https://img.shields.io/hackage/v/template.svg?label=Hackage)](https://hackage.haskell.org/package/zip-codec) 

> Nobody notices when your zipper is up.

A codec for the zip
https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
format.

heavily inspired by [zip-conduit](https://hackage.haskell.org/package/zip-conduit),
makes monads opt in. 
Unlike zip which is completly baked around a custom monad.

The ambition is to get parralel reading of a single
input file/bytestring.

TODO:

+ [x] get a working implementation
+ [x] See if can get tests from other libraries
  (I pobably can add more but better to focus on main value first which is
  parlalization/speed)
+ [x] See if can get benchmarks
  looks like we need to add zip, and this package to zip-conduit to get 
  a complete overivew:
  https://github.com/tymmym/zip-conduit/blob/master/bench/Bench.hs
+ [x] Add test for concurrent reading.
      (apparantly the zip package does this right? see how they do it)
+ [x] figure out how to bench concurrent functions (I think by 
      default criterion already spawns a bunch of threads).
+ [ ] Add a concurrent writing implementation and test it.
  + [ ] Figure out how to track the central dir with that par writing method.
+ [ ] 64bit support (lol)

## Alternatives

+ [zip](https://hackage.haskell.org/package/zip):
  Intentionally excessivaly file based.
  Feature rich but hard to paralize due to their monadic design.
  A PR to this library won't work because it'll be incredibly breaking.
+ [zip-conduit](https://hackage.haskell.org/package/zip-conduit): 
  zip file reading with conduit.
  I stole most of their sources.
  Also used a monadic design for some reason (which I just ripped out)
  I also exposed most of their internals which are well designed,
  but I was carefull to add Either based error reporting rather,
  then crashing.
+ [zip-archive](https://hackage.haskell.org/package/zip-archive):
   loads everything in memory so only usefull for small files.



## Usage

### Tools
Enter the nix shell.
```
nix-shell
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```
