{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "rewrite-live"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "identity"
  , "lists"
  , "maybe"
  , "pairs"
  , "parsing"
  , "prelude"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "strings"
  , "tuples"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "purs/**/*.purs", "purs_test/**/*.purs" ]
}
