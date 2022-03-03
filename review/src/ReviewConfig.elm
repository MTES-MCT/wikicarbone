module ReviewConfig exposing (config)

import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []
        |> Rule.ignoreErrorsForFiles [ "src/Views/Modal.elm" ]
    , NoUnused.CustomTypeConstructorArgs.rule
        |> Rule.ignoreErrorsForFiles [ "src/Server/Route.elm" ]
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForFiles [ "src/Views/Button.elm" ]
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    ]
