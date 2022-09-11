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
Unlike zip which is completly baked around IO.

The ambition is to get parralel reading of a single
input file/bytestring.

TODO:

+ [ ] get a working implementation
+ [ ] See if can get tests from other libraries
+ [ ] See if can get benchmarks


## Alternatives

+ [zip](https://hackage.haskell.org/package/zip):
  Intentionally excessivaly file based.
  Feature rich but hard to paralize due to their monadic design.
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
