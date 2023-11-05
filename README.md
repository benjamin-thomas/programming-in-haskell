## Reading the book: Programming in Haskell, by Graham Hutton

## Basic commands

- cabal clean
- cabal build
- cabal run (launches ./app/Main.hs)
- cabal repl
  - > Chapter01.hello

## Possible workflows

### Simple
```
cabal repl
ghci> hello
Hello from lib!
ghci> Chapter01.double 2
4
ghci> import Chapter01
ghci> double 2
5 -- bad result
ghci> :reload -- after code change. NOTE :r is an available shortcut
ghci> double 2
4 -- fixed!

# KISS: reload the module and re-run "double 2" by pressing <Up><Enter>
ghci> :cmd return $ unlines [":reload", "double 2"]
Ok, four modules loaded.
4
```


### Not sure why I would use this, keeping for ref.

```
cabal exec ghci src/Chapter01.hs
ghci> hello
"World!"
ghci> Chapter01.double 2
4
```

### Ghcid workflow

```
# verbose
ghcid --command "cabal repl" --test "Chapter01.double 2"
# short
ghcid -c "cabal repl" -T "Chapter01.double 2"
```

```
 # Needs to be in sync with the current ghc, that's a bit of a pain.
cabal install doctest --overwrite-policy=always
doctest --version;ghc --version

# Run all the test examples and exit with a summary.
cabal repl --with-ghc=doctest
cabal exec doctest src/

# Re-run all the doctests on any file change
find ./src/ | entr -rc cabal exec doctest src/

# Re-run chapter-specific doctests on any file change -- BETTER!
ghcid --command 'cabal new-repl' --test ':!doctest ./src/Chapter01.hs'
```

## Run tests

```
ghcid -c 'cabal repl' -T ':!doctest ./src/Chapter01.hs'
ghcid -c 'cabal repl' -T ':!doctest ./src/'
```
