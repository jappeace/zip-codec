[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/zip-codec/Test)](https://github.com/jappeace/zip-codec/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)
[![Hackage version](https://img.shields.io/hackage/v/template.svg?label=Hackage)](https://hackage.haskell.org/package/zip-codec) 

> Nobody notices when your zipper is up.

A codec for the zip
https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
format.

heavily inspired by zip-conduit,
makes monads opt in. 
Unlike zip which is completly baked around IO.

The ambition is to get parralel reading of a single
input file/bytestring.

TODO:

+ [ ] get a working implementation
+ [ ] See if can get tests from other libraries
+ [ ] See if can get benchmarks


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
