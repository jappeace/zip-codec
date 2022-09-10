[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/zip-codec/Test)](https://github.com/jappeace/zip-codec/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)
[![Hackage version](https://img.shields.io/hackage/v/template.svg?label=Hackage)](https://hackage.haskell.org/package/zip-codec) 

> The eye that looks ahead to the safe course is closed forever.

A codec for the zip
https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT

heavily inspired by zip-conduit,
makes monads opt in. 
Unlike zip which is completly baked around IO.

The ambition is to get parralel reading of a single
input file/bytestring

TODO:

+ [ ] get a working implementation
+ [ ] See if can get tests from other libraries
+ [ ] See if can get benchmarks


## Usage

### Modifying for your project
Assuming the name of your new project is `new-project`.

```
git clone git@github.com:jappeace/haskell-template-project.git new-project
cd new-project
```

+ [x] Edit package.yaml,
    + [x] find and replace template with `new-project`
    + [ ] Update copyright
    + [ ] Update github
+ [x] Run `make hpack` to update cabal files
+ [x] remove template.cabal
+ [x] Edit Changelog.md
  + [ ] replace template with `new-project`
  + [ ] Also describe your version 1.0.0 release.
+ [x] Edit default.nix, replace template with `new-project`.
+ [x] Edit copyright in LICENSE
+ [x] Edit `nix/bundle.nix` to point to the executable
+ [x] Edit `nix/ci.nix` and `nix/pkgs.nix` for name of package
+ [x] Edit `shell.nix`

#### Reconfigure remotes
```
git remote add template git@github.com:jappeace/haskell-template-project.git
git remote set-url origin git@github.com:YOUR-ORG-OR-USER-NAME/new-project.git
```

We can get template updates like this if we want to by doing `git pull template`.
There will be a large amount of conflicts, but the merge commit should solve them permanently.

#### Readme

+ [ ] Select desired badges. 
  + [ ] Point build badges to right project
+ [ ] Give short project description.
+ [ ] Add new quote suited for the project.
  For example for [fakedata-quickcheck](https://github.com/fakedata-haskell/fakedata-quickcheck#readme)
  I used Kant because
  he dealt with the question "what is truth" a lot.
+ [ ] Truncate this checklist
+ [ ] Truncate motivation for using  this template

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
