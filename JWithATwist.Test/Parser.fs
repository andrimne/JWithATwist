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
open System.Collections
open System.Collections.Generic
open System.Text
open System.IO
open System.Runtime.Serialization.Formatters.Soap
open Xunit
open Xunit.Abstractions
open JWithATwist.Base
open JWithATwist.ParserDefinitions
open JWithATwist.ParserInterface
open JWithATwist.Parser
open FParsec

module Parser =

    type ``Test Parser`` () =
    
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
                            |{JValue=JTypeIntegerArray[|1L|]} ->
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
        member x.``Write a string and expect a boxed string result `` () =
            let resultOption = parseAllSpeach "{ \"Hej ! \" } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let s ={JType = JTBType.JTypeUnicode; JShape= [|6|]; JValue = JTypeUnicodeArray[|'H';'e';'j';' ';'!';' '|] ;}
                            let expected ={JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray[|s|] ;}
                            let actual = f ()
                            Assert.Equal(expected,actual)
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Write a string with double quotes inside `` () =
            let resultOption = parseAllSpeach "{ \"Hej\"\" ! \" } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let s = {JType = JTBType.JTypeUnicode; JShape= [|7|]; JValue = JTypeUnicodeArray[|'H';'e';'j';'\"';' ';'!';' '|] ;}
                            let expected ={JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray[|s|] ;}
                            let actual = f ()
                            Assert.Equal(expected,actual)
                        |_ ->
                            Assert.True(false)
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Write two strings `` () =
            let resultOption = parseAllSpeach "{ \"Hej ! \" \"Hej ! \" } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let s = {JType = JTBType.JTypeUnicode; JShape= [|6|]; JValue = JTypeUnicodeArray[|'H';'e';'j';' ';'!';' '|] ;}
                            let expected ={JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray[|s;s|] ;}
                            let actual = f ()
                            Assert.Equal(expected,actual)
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Write an addition of two nouns and expect a Noun result `` () =
            let resultOption = parseAllSpeach "{ 1 + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeIntegerArray[|2L|]} ->
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
        member x.``Add several values `` () =
            let resultOption = parseAllSpeach "{ 1  2 + 1  2 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeIntegerArray[|2L;4L|]} ->
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
        member x.``Negate `` () =
            let resultOption = parseAllSpeach "{ |- 1 2 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeIntegerArray[|-1L;-2L|]} ->
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
        member x.``Monadic rank operator `` () =
            let resultOption = parseAllSpeach "{ |- |\'/ 0 / 1 2 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeIntegerArray[|-1L;-2L|]} ->
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
        member x.``Dyadic rank operator `` () =
            let resultOption = parseAllSpeach "{ 1 2 + \'/ 0 0 / 1 2 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeIntegerArray[|2L;4L|]} ->
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
        member x.``Add floats `` () =
            let resultOption = parseAllSpeach "{ 1.0 + 1.0 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeFloatArray[|2.0|]} ->
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
        member x.``Add left float with right integer `` () =
            let resultOption = parseAllSpeach "{ 1.0 + 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeFloatArray[|2.0|]} ->
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
        member x.``Add left integer with right float `` () =
            let resultOption = parseAllSpeach "{ 1 + 1.0 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeFloatArray[|2.0|]} ->
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
        member x.``Reciprocal of float `` () =
            let resultOption = parseAllSpeach "{ |% 0.5 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeFloatArray[|2.0|]} ->
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
        member x.``Reciprocal of integer `` () =
            let resultOption = parseAllSpeach "{ |% 2 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeFloatArray[|0.5|]} ->
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
        member x.``Box `` () =
            let resultOption = parseAllSpeach "{ |< 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let boxed = {JType = JTBType.JTypeInteger; JShape = [||]; JValue = JTypeIntegerArray [|2L|] ; }
                        match (f ()) with
                        |{JValue=JTypeBoxedArray[|boxed|]} ->
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
        member x.``Open `` () =
            let resultOption = parseAllSpeach "{ |> |< 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        match (f ()) with
                        |{JValue=JTypeIntegerArray [|2L|]} ->
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
        member x.``Monadic Iota `` () =
            let resultOption = parseAllSpeach "{ |i. 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        match (f ()) with
                        |{JValue=JTypeIntegerArray [|0L;1L|]} ->
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
        member x.``Shape `` () =
            let resultOption = parseAllSpeach "{ 2 2 $ 1 2 3 4 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeInteger;JShape=[|2;2|];JValue=JTypeIntegerArray [|1L;2L;3L;4L|]} ->
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
        member x.``Ravel `` () =
            let resultOption = parseAllSpeach "{ |, |i. 2 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeInteger;JShape=[|4|];JValue=JTypeIntegerArray [|0L;1L;2L;3L|]} ->
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
        member x.``Tally `` () =
            let resultOption = parseAllSpeach "{ |# |i. 2 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeInteger;JShape=[||];JValue=JTypeIntegerArray [|2L|]} ->
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
        member x.``ShapeOf `` () =
            let resultOption = parseAllSpeach "{ |$ 1 2 3 4 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeInteger;JShape=[||];JValue=JTypeIntegerArray [|4L|]} ->
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
        member x.``Signum `` () =
            let resultOption = parseAllSpeach "{ |* 1 -2 3 4 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeInteger;JShape=[|4|];JValue=JTypeIntegerArray [|1L;-1L;1L;1L|]} ->
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
        member x.``Magnitude `` () =
            let resultOption = parseAllSpeach "{ || 1 -2 3 4 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeInteger;JShape=[|4|];JValue=JTypeIntegerArray [|1L;2L;3L;4L|]} ->
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
        member x.``Not `` () =
            let resultOption = parseAllSpeach "{ |-. true } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeBoolean;JShape=[||];JValue=JTypeBooleanArray [|false|]} ->
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
        member x.``Times `` () =
            let resultOption = parseAllSpeach "{  1 * 1 -2 3 4 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeInteger;JShape=[|4|];JValue=JTypeIntegerArray [|1L;-2L;3L;4L|]} ->
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
        member x.``Divide `` () =
            let resultOption = parseAllSpeach "{ 1 % 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeFloat;JShape=[||];JValue=JTypeFloatArray [|0.5|]} ->
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
        member x.``Power `` () =
            let resultOption = parseAllSpeach "{ 10 ^ 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeFloat;JShape=[||];JValue=JTypeFloatArray [|100.0|]} ->
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
        member x.``And `` () =
            let resultOption = parseAllSpeach "{  true *. false } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeBoolean;JShape=[||];JValue=JTypeBooleanArray [|false|]} ->
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
        member x.``Or `` () =
            let resultOption = parseAllSpeach "{  true +. false } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeBoolean;JShape=[||];JValue=JTypeBooleanArray [|trua|]} ->
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
        member x.``Equal `` () =
            let resultOption = parseAllSpeach "{  1 = 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeBoolean;JShape=[||];JValue=JTypeBooleanArray [|false|]} ->
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
        member x.``Not Equal `` () =
            let resultOption = parseAllSpeach "{  1 ~: 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeBoolean;JShape=[||];JValue=JTypeBooleanArray [|true|]} ->
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
        member x.``LessThan `` () =
            let resultOption = parseAllSpeach "{  1 < 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeBoolean;JShape=[||];JValue=JTypeBooleanArray [|true|]} ->
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
        member x.``LargerThan `` () =
            let resultOption = parseAllSpeach "{  1 > 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeBoolean;JShape=[||];JValue=JTypeBooleanArray [|false|]} ->
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
        member x.``LessOrEqual `` () =
            let resultOption = parseAllSpeach "{  1 <: 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeBoolean;JShape=[||];JValue=JTypeBooleanArray [|true|]} ->
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
        member x.``LargerOrEqual `` () =
            let resultOption = parseAllSpeach "{  1 >: 2 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeBoolean;JShape=[||];JValue=JTypeBooleanArray [|false|]} ->
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
        member x.``Take `` () =
            let resultOption = parseAllSpeach "{  1 <.- 3 2 1 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeInteger;JShape=[|1|];JValue=JTypeIntegerArray [|3L|]} ->
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
        member x.``Drop `` () =
            let resultOption = parseAllSpeach "{  1 >.- 3 2 1 } "
            match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        let noun = f ()
                        match noun with
                        |{JType=JTBType.JTypeInteger;JShape=[|2|];JValue=JTypeIntegerArray [|2L;1L|]} ->
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
        member x.``Subtract `` () =
            let resultOption = parseAllSpeach "{ 1.0 - 2.0 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeFloatArray[|-1.0|]} ->
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
        member x.``Min `` () =
            let resultOption = parseAllSpeach "{ 1.0 <. 2.0 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeFloatArray[|1.0|]} ->
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
        member x.``Max `` () =
            let resultOption = parseAllSpeach "{ 1.0 >. 2.0 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeFloatArray[|2.0|]} ->
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
        member x.``Floor `` () =
            let resultOption = parseAllSpeach "{  |<. 2.3 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeIntegerArray[|2L|]} ->
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
        member x.``Ceiling `` () =
            let resultOption = parseAllSpeach "{  |>. 2.3 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let a = f ()
                            match a with
                            |{JValue=JTypeIntegerArray[|3L|]} ->
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
        member x.``Bti `` () =
            let resultOption = parseAllSpeach "{  |_ true false } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let a = f ()
                            match a with
                            |{JValue=JTypeIntegerArray[|1L;0L|]} ->
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
        member x.``Fold `` () =
            let resultOption = parseAllSpeach "{ 0 + / 1 2 3 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            match (f ()) with
                            |{JValue=JTypeIntegerArray[|6L|]} ->
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
        member x.``Scan `` () =
            let resultOption = parseAllSpeach @"{ 0 + \ 1 2 3 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeIntegerArray[|1L;3L;6L|]} ->
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
        member x.``From `` () =
            let resultOption = parseAllSpeach @"{ 2 4 <- 0 1 2 3 4 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeIntegerArray[|2L;4L|]} ->
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
        member x.``Amend `` () =
            let resultOption = parseAllSpeach "{ ( ( |< 2 4 ) , \"xy\" ) >- |> \"abcde\" } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeUnicodeArray [|'a';'b';'x';'d';'y'|]} ->
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
        member x.``GradeUp `` () =
            let resultOption = parseAllSpeach "{ |/: 3 1 2 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeIntegerArray [|1L;2L;0L|]} ->
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
        member x.``Select `` () =
            let resultOption = parseAllSpeach "{ true false /::  4 5 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeIntegerArray [|4L|]} ->
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
        member x.``Replicate `` () =
            let resultOption = parseAllSpeach "{ 2 2 0 # 4 5 6 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeIntegerArray [|4L;4L;5L;5L|]} ->
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
        member x.``Dyadic iota `` () =
            let resultOption = parseAllSpeach "{ 0 1 2 3 4 i. 2 3 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeIntegerArray [|2L;3L|]} ->
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
        member x.``Transpose `` () =
            let resultOption = parseAllSpeach "{ 1 0 \:: |i. 2 3  } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeIntegerArray [|0L;3L;1L;4L;2L;5L|]} ->
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
        member x.``Default format `` () =
            let resultOption = parseAllSpeach "{ |\': 1 } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeUnicodeArray [|' ';'1'|]} ->
                                Assert.True(true) 
                            |_ ->
                            Assert.True(false)                                
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

