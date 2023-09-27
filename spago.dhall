{ name = "purely-planning"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "enums"
  , "halogen"
  , "halogen-formless"
  , "integers"
  , "js-uri"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  , "uri"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
