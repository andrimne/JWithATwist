(*
    JWithATwist - A Twisted Version of the Programming Language J
    Copyright (C) 2016 Erling Hellenäs

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
namespace JWithATwist.Test

open System
open System.IO
open System.Collections
open System.Collections.Generic
open Xunit
open Xunit.Abstractions;
open JWithATwist.ParserMock
open JWithATwist.ParserMockDefinitions
open JWithATwist.ParserMockInterface
open FParsec

module ParserMock =

    type ``Test ParserMock`` () =
    
        [<Fact>]
        member x.``Run with empty input, verify the result of type unit`` () =
            let resultOption = parseAllSpeach ""
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeUnitUnit f} ->
                            Assert.True(true) 
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Write something random, verify that there is an error message`` () =
            let parseResult = parseAllSpeach "whatever"
            match parseResult with
                | Failure(errorMsg, _, _) -> 
                        Assert.True(true)
                | Success(result, _, rest)   -> 
                        Assert.True(false)

        [<Fact>]
        member x.``Write a left and right curly bracket, verify the result of type unit `` () =
            let resultOption = parseAllSpeach "{ } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeUnitUnit f} ->
                            Assert.True(true) 
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Write a Noun and expect a Noun result `` () =
            let resultOption = parseAllSpeach "{ 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value=1} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Write  an addition of two nouns and expect a Noun result `` () =
            let resultOption = parseAllSpeach "{ 1 + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value=2} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 
        [<Fact>]
        member x.``Write  a subtraction of two nouns and expect a correct result `` () =
            let resultOption = parseAllSpeach "{ 1 - 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value=0} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``A missing blank after an integer should give an error. `` () =
            let parseResult = parseAllSpeach "{ 123} "
            match parseResult with
                | Success(result, _, rest)   -> 
                    Assert.True(false) 
                |_ -> 
                    Assert.True(true) 

        [<Fact>]
        member x.``A missing blank after { should give an error. `` () =
            let parseResult = parseAllSpeach "{} "
            match parseResult with
                | Success(result, _, rest)   -> 
                    Assert.True(false) 
                |_ -> 
                    Assert.True(true) 

        [<Fact>]
        member x.``Negate a noun. `` () =
            let resultOption = parseAllSpeach "{ |- 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value=(-1)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Use Insert monadic adverb `` () =
            let resultOption = parseAllSpeach "{ + / 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value=(2)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Use Table dyadic adverb `` () =
            let resultOption = parseAllSpeach "{ 2 + /- 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value= 3} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Use Determinant monadic conjunction `` () =
            let resultOption = parseAllSpeach "{ |- |. + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value= -2} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Use DotProduct dyadic conjunction `` () =
            let resultOption = parseAllSpeach "{ 2 |- . + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value= (-3)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Use DotProduct dyadic conjunction with preceding adverb `` () =
            let resultOption = parseAllSpeach "{ 2 + / . + 2 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value= (8)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Use Determinant monadic conjunction with preceding adver `` () =
            let resultOption = parseAllSpeach "{ + / |. + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value= 4} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 


        [<Fact>]
        member x.``Repeated monadic verbs in a verb train `` () =
            let resultOption = parseAllSpeach "{ |- |- 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value= (1)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Monadic adverb in verb train `` () =
            let resultOption = parseAllSpeach "{ |- + / |- 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (2)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Monadic conjunction in verb train `` () =
            let resultOption = parseAllSpeach "{ |- |- |. + |- 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{Value= (-2)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Monadic adverb as argument to monadic conjunction in verb train `` () =
            let resultOption = parseAllSpeach "{ |- + / |. + |- 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (4)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Noun train starting with noun `` () =
            let resultOption = parseAllSpeach "{ 1 + 1 + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (3)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Noun train starting with verb train `` () =
            let resultOption = parseAllSpeach "{ |- 1 + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (-2)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Handle one bracketed expression in noun train `` () =
            let resultOption = parseAllSpeach "{ ( 1 + 1 ) } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (2)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Handle bracketed expression in beginning of  noun train `` () =
            let resultOption = parseAllSpeach "{ ( 1 + 1 ) + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (3)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Handle bracketed expression in the end of noun train `` () =
            let resultOption = parseAllSpeach "{ 1 + ( 1 + 1 ) } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (3)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Handle bracketed expression in the middle of noun train `` () =
            let resultOption = parseAllSpeach "{ 1 + ( 1 + 1 ) + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (4)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Handle recursive bracketed expressions in noun train `` () =
            let resultOption = parseAllSpeach "{ 1 + ( 1 + ( 1 + 1 ) ) + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (5)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Handle recursive noun definitions `` () =
            let resultOption = parseAllSpeach "{ 1 + { 1 + { 1 + 1 } } + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (5)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``A single right noun should create a monadic verb which always returns its argument `` () =
            let resultOption = parseAllSpeach "{! ] } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbMonadic f} ->
                            let (y:Noun) = {Value= 5}
                            Assert.True((f y) = y) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Right noun should work as last part of a noun train `` () =
            let resultOption = parseAllSpeach "{! 1 + ] } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbMonadic f} ->
                                let (y:Noun) = {Value= 5}
                                let (expected:Noun) = {Value= 6}
                                let actual = f y
                                Assert.Equal(expected,actual) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Monadic open definition `` () =
            let resultOption = parseAllSpeach "{! |- ] } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbMonadic f} ->
                                let (y:Noun) = {Value= 5}
                                let (expected:Noun) = {Value= -5}
                                let actual = f y
                                Assert.Equal(expected,actual) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Monadic open definition with adverb `` () =
            let resultOption = parseAllSpeach "{! + / ] } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbMonadic f} ->
                                let (y:Noun) = {Value= 5}
                                let (expected:Noun) = {Value= 10}
                                let actual = f y
                                Assert.Equal(expected,actual) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Monadic open definition with dyadic conjunction `` () =
            let resultOption = parseAllSpeach "{! 5 |- . + ] } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbMonadic f} ->
                                let (y:Noun) = {Value= 5}
                                let (expected:Noun) = {Value= -10}
                                let actual = f y
                                Assert.Equal(expected,actual) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)


        [<Fact>]
        member x.``Right noun should work as first part of a noun train `` () =
            let resultOption = parseAllSpeach "{! ] + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbMonadic f} ->
                                let (y:Noun) = {Value= 5}
                                let (expected:Noun) = {Value= 6}
                                let actual = f y
                                Assert.Equal(expected,actual) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Right noun should work as middle part of a noun train `` () =
            let resultOption = parseAllSpeach "{! 1 + ] + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbMonadic f} ->
                                let (y:Noun) = {Value= 5}
                                let (expected:Noun) = {Value= 7}
                                let actual = f y
                                Assert.Equal(expected,actual) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``The resulting definition should have a verb function `` () =
            let resultOption = parseAllSpeach "{ {! 1 + ] } 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (2)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``The resulting definition should work as first part of a verb train `` () =
            let resultOption = parseAllSpeach "{ {! 1 + ] } |- 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (0)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``The resulting definition should work as last part of a verb train `` () =
            let resultOption = parseAllSpeach "{ |- {! 1 + ] } 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (-2)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``The resulting definition should work as middle part of a verb train `` () =
            let resultOption = parseAllSpeach "{ |- {! 2 + ] } |- 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (-1)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``A single left noun should create a dyadic verb which always returns its left argument `` () =
            let resultOption = parseAllSpeach "{! [ } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbDyadic f} ->
                            let (x:Noun) = {Value= 5}
                            let (y:Noun) = {Value= -5}
                            Assert.True((f x y) = x) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Left noun should work as first part of a noun train `` () =
            let resultOption = parseAllSpeach "{! [ + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbDyadic f} ->
                                let (x:Noun) = {Value= 5}
                                let (y:Noun) = {Value= -5}
                                let (expected:Noun) = {Value= 6}
                                let actual = f x y
                                Assert.Equal(expected,actual) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Left noun should work as last part of a noun train `` () =
            let resultOption = parseAllSpeach "{! 1 + [ } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbDyadic f} ->
                                let (x:Noun) = {Value= 5}
                                let (y:Noun) = {Value= -5}
                                let (expected:Noun) = {Value= 6}
                                let actual = f x y
                                Assert.Equal(expected,actual) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Left noun should work as middle part of a noun train `` () =
            let resultOption = parseAllSpeach "{! 1 + [ + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbDyadic f} ->
                                let (x:Noun) = {Value= 5}
                                let (y:Noun) = {Value= -5}
                                let (expected:Noun) = {Value= 7}
                                let actual = f x y
                                Assert.Equal(expected,actual) 
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``The resulting definition should act as a verb `` () =
            let resultOption = parseAllSpeach "{ 5 {! 1 + [ + 1 } 7 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (7)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``The resulting definition should act as input to an adverb `` () =
            let resultOption = parseAllSpeach "{ {! 1 + [ } / 7 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (8)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``The resulting definition should act as input to a conjunction `` () =
            let resultOption = parseAllSpeach "{ 5 |- . {! [ + 1 } 7 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (-6)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``It should be possible to mix right and left noun `` () =
            let resultOption = parseAllSpeach "{ 5 {! [ + ] } 7 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (12)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``This expression should give a syntax error `` () =
            let resultOption = parseAllSpeach "{ 1 + |- + 5 } "
            match resultOption with
                | Failure(errorMsg, _, _) -> 
                        Assert.True(true)
                | Success(result, _, rest)   -> 
                        Assert.True(false)


        [<Fact>]
        member x.``A right noun within brackets references the right argument of the surrounding open definition `` () =
            let resultOption = parseAllSpeach "{! {! ( ] ) } 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (1)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Now we have a new way to write a fork `` () =
            let resultOption = parseAllSpeach "{! 1 {! ( [ + ] ) + [ + ] } 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (4)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Add something to each of left and right argument and add the result `` () =
            let resultOption = parseAllSpeach "{! 3 {! ( [ + 1 ) + ] + 2 } 5 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (11)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``A right verb and no left or right noun should give syntax error `` () =
            let resultOption = parseAllSpeach "{! ]. } "
            match resultOption with
                | Failure(errorMsg, _, _) -> 
                        Assert.True(true)
                | Success(result, _, rest)   -> 
                        Assert.True(false)

        [<Fact>]
        member x.``Adverb with monadic left argument `` () =
            let resultOption = parseAllSpeach "{! |- // 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (-1)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Adverb with monadic left argument as left argument of dyadic conjunction `` () =
            let resultOption = parseAllSpeach "{! 2 |- // . + 5 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (-7)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``Adverb with monadic left argument as left argument of monadic conjunction `` () =
            let resultOption = parseAllSpeach "{! |- // |. + 5 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{Value= (-10)} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``A system exception during program execution should generate an error message `` () =
            let resultOption = parseAllSpeach @"{!  -2147483647 - 200 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let a = new StringWriter()
                            Console.SetOut(a)
                            ParsePrint parseResult
                            let expected = "Arithmetic operation resulted in an overflow.\r\n"
                            let actual = a.ToString()
                            Assert.Equal(expected,actual)
                        |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false)

        [<Fact>]
        member x.``A syntax error from the interpreter should generate an error message `` () =
            let resultOption = parseAllSpeach @"{! 1 |- / 2 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            Assert.True(false) 
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                | Failure(errorMsg, _, _) -> 
                    Assert.True(true)

                        