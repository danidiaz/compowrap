# compowrap

Utility for inserting [`Compose`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Functor-Compose.html#t:Compose) newtypes into deeply nested functor applications in order to handle them as a single functor. The level of nesting is specified using a type-level `Natural`.

```
stuff, stuff' :: Either Int (Either Int (Either Int Bool))
stuff = Right (Right (Right False))
stuff' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in stuff
        & wrap
        & fmap not
        & unwrap

stuff'' :: Either Int (Either Int (Either Int String))
stuff'' =
  let WrapUnwrap {wrap, unwrap} = askWrapUnwrap 3
   in unwrap $ show <$> ((&&) <$> wrap stuff <*> wrap stuff)
```

__NOTE:__ I'm not claiming this is practical, it's just a fun experiment.

## Motivation

[post](https://bsky.app/profile/diazcarrete.bsky.social/post/3mur4mxba5k2n).

## Build

Run `$ cabal build` to build the project
## Documentation

Run `$ cabal haddock --open` to generate a reference for the API of the project.
# compowrap
