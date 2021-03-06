module Elm.Parser.ModuleTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Modules as Parser
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Expect
import Test exposing (..)


all : Test
all =
    describe "ModuleTests"
        [ test "formatted moduleDefinition" <|
            \() ->
                parseFullStringWithNullState "module Foo exposing (Bar)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal
                        (Just
                            (NormalModule
                                { moduleName = Node emptyRange <| [ "Foo" ]
                                , exposingList = Node emptyRange <| Explicit [ Node emptyRange <| TypeOrAliasExpose "Bar" ]
                                }
                            )
                        )
        , test "port moduleDefinition" <|
            \() ->
                parseFullStringWithNullState "port module Foo exposing (Bar)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (PortModule { moduleName = Node emptyRange <| [ "Foo" ], exposingList = Node emptyRange <| Explicit [ Node emptyRange <| TypeOrAliasExpose "Bar" ] }))
        , test "port moduleDefinition with spacing" <|
            \() ->
                parseFullStringWithNullState "port module Foo exposing ( Bar )" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (PortModule { moduleName = Node emptyRange <| [ "Foo" ], exposingList = Node emptyRange <| Explicit [ Node emptyRange <| TypeOrAliasExpose "Bar" ] }))
        , test "effect moduleDefinition" <|
            \() ->
                parseFullStringWithNullState "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal
                        (Just
                            (EffectModule
                                { moduleName = Node emptyRange <| [ "Foo" ]
                                , exposingList = Node emptyRange <| Explicit [ Node emptyRange <| TypeOrAliasExpose "Bar" ]
                                , command = Just <| Node emptyRange <| "MyCmd"
                                , subscription = Just <| Node emptyRange <| "MySub"
                                }
                            )
                        )
        , test "unformatted" <|
            \() ->
                parseFullStringWithNullState "module \n Foo \n exposing  (..)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (NormalModule { moduleName = Node emptyRange <| [ "Foo" ], exposingList = Node emptyRange <| All emptyRange }))
        , test "unformatted wrong" <|
            \() ->
                parseFullStringWithNullState "module \nFoo \n exposing  (..)" Parser.moduleDefinition
                    |> Expect.equal Nothing
        , test "exposing all" <|
            \() ->
                parseFullStringWithNullState "module Foo exposing (..)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (NormalModule { moduleName = Node emptyRange <| [ "Foo" ], exposingList = Node emptyRange <| All emptyRange }))
        , test "module name with _" <|
            \() ->
                parseFullStringWithNullState "module I_en_gb exposing (..)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (NormalModule { moduleName = Node emptyRange <| [ "I_en_gb" ], exposingList = Node emptyRange <| All emptyRange }))
        ]
