---
title: "Summer of Code 2018"
output: html_document
css: modest.css
---
Code to solve the [Summer of Code](https://learn2.open.ac.uk/course/view.php?id=206891) puzzles. These sample solutions are in [Python](https://www.python.org/) and [Haskell](https://wiki.haskell.org/Haskell).

# Python
I develop the solutions with [Jupyter Lab](https://github.com/jupyterlab/).

# Haskell

[Learn you a Haskell](http://learnyouahaskell.com/chapters), [Introduction to Haskell 98](https://www.haskell.org/tutorial/index.html), and [Hackage](https://hackage.haskell.org/) are good resources.

The [Stack documentation](https://docs.haskellstack.org/en/stable/README/) and [How I Start: Haskell](http://howistart.org/posts/haskell/1/) are good sources of using the tools. 

## Toolchain

I'm using the basic Haskell Platform installation, togeher with `stack` to manage the packages and dependencies (install with
```
$ sudo aptitude install haskell-platform haskell-stack
```
).

### Creating the repository and project
Create the repository as normal: create the project in Gitolite, clone it, and insert the `.gitignore` and `README.md` files.

There's just one package, with the code in sub-directories of the `src` directory. Each day will generate one (or more) entries in the `summerofcode2018soln.cabal` file.

Create the basic `stack` project. This will create a new directory. Note that this new directory name can't have a hyphen-delimited word that's just digits, so the project will have to be `advent-of-code`

```
stack new summerofcode2018soln --bare simple
```

Modify the `stack.yaml` file as needed, such as adding the `ghc-options` stanza. 

### Creating subsequent days

Each day lives in a separate directory within the `src` directory. It will also need it's own stanza in `summerofcode2018.cabal`.

Compile with
```
stack build
```
or 
```
stack build task1
```

Run with
```
stack exec task1
```

Run interactively with
```
stack ghci summerofcode2018soln:exe:task1
```

To profile, use 
```
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"
```
then run with
```
stack exec -- task1 +RTS -p -hy
```
Make the profile graph visible with 
```
/usr/lib/ghc/bin/hp2ps task1.hp
```

## Packages

Stack is using the [12.9-lts resolver](https://www.stackage.org/lts-12.9) for packages, so make sure you read the [correct documentation for the packages included in it](https://www.stackage.org/lts-12.9/docs).

When you use a new package, use 

```
stack solver
```
to see how the `stack.yaml` file needs to change, and 
```
stack solver --update-yaml
```
to implement the changes.

# Readme

Build this readme file wth
```
pandoc -s README.md > README.html
```

(Using the [Modest style](https://github.com/markdowncss/modest).)
