## Reading the book: Programming in Haskell, by Graham Hutton

## Run tests

```
ghcid -c 'cabal repl' -T ':!doctest ./src/Chapter01.hs'
ghcid -c 'cabal repl' -T ':!doctest ./src/'
```


## Tips

Reload and re-run a function into `ghci`:

```
:cmd return $ unlines [":reload", "double 2"]
```