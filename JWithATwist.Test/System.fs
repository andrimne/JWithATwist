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
open JWithATwist.Define
open JWithATwist.ParserDefinitions
open JWithATwist.ParserInterface
open JWithATwist.Parser
open FParsec

module System =

    type ``System Tests`` () =

        [<Fact>]
        member x.``Definition on several lines. `` () =
            let resultOption = parseAllSpeach @"
{
    1 + 
    0 + \ 1 2 3 
} "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let r = f ()
                            match r with
                            |{JValue=JTypeIntegerArray[|2L;4L;7L|]} ->
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
        member x.``Use JWithATwist from F# `` () =
            let resultOption = parseAllSpeach @"{! [ + ] } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbDyadic v} ->
                            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
                            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
                            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
                            let actual = v x y
                            Assert.Equal(expected,actual)  
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Use JWithATwist from F# - Define strings `` () =
            let resultOption = parseAllSpeach @"{ |> ""a"" } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeNounUnit f} ->
                            let expected = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'a'|] ;}
                            let actual = f ()
                            Assert.Equal(expected,actual)  
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 

        [<Fact>]
        member x.``Save and restore JWithATwist generated FSharp code `` () =
            // Serialization of function values
            // ("Deserialization will only work from within
            //  a completely identical binary.")
            // https://fsnotebook.net/notebook/fssnip-2R/Serialization_of_functions
            let resultOption = parseAllSpeach @"{! [ + ] } "
            match resultOption with
                | Success(result, _, rest)   -> 
                    match result with
                    |[parseResult] ->
                        match parseResult with
                        |{Value=TypeVerbDyadic v} ->
                            let sf = new System.Runtime.Serialization.Formatters.Soap.SoapFormatter();
                            let Ser v =
                                use fs = new FileStream("c:/windows/temp/function", FileMode.OpenOrCreate, FileAccess.ReadWrite)
                                sf.Serialize(fs, v)
                            let Deser () =
                                use fs2 = new FileStream("c:/windows/temp/function", FileMode.Open, FileAccess.Read)
                                sf.Deserialize(fs2) :?> (JVerbDyadic)
                            Ser v
                            let vv = Deser ()
                            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
                            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
                            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
                            let actual = vv x y
                            Assert.Equal(expected,actual)  
                         |_ ->
                            Assert.True(false) 
                    |_ ->
                        Assert.True(false)
                |_ -> 
                    Assert.True(false) 




    type ``Code snippets`` () =
    //System tests and supposed to give some useful code snippets


        [<Fact>]
        member x.``Matrix multiplication`` () =
            let MatrixMultiplication = JVerbDyadicDefine @"
{! {! ( ( -1 <.- |$ ] ) $ 0 ) + / ] }  |'/ 2 /  [  * '/ 1 2 /  ] }"
            let x = JNounDefine @"{ |i. 2 3 }"
            let y = JNounDefine @"{ |i. 3 2 }"
            let actual = MatrixMultiplication x y
            let expected = JNounDefine @"{ 2 2 $ 10 13 28 40 }"
            Assert.Equal(expected,actual)
            let x = JNounDefine @"{ |i. 0 3 }"
            let y = JNounDefine @"{ |i. 3 0 }"
            let actual = MatrixMultiplication x y
            let expected = JNounDefine @"{ 0 0 $ 0 }"
            Assert.Equal(expected,actual)
            let x = JNounDefine @"{ |i. 2 0 }"
            let y = JNounDefine @"{ |i. 0 2 }"
            let actual = MatrixMultiplication x y
            let expected = JNounDefine @"{ 2 2 $ 0 }"
            Assert.Equal(expected,actual)

        [<Fact>]
        member x.``Reverse data along axis of arrays`` () =
            let ReverseArrayAxis = JVerbDyadicDefine @"
{! ( |i. ( ( ( |_ |-. [ ) - |_ [ ) * |$ ] ) ) <- |, ]  }"
            let x = JNounDefine @"{ false true }"
            let y = JNounDefine @"{ |i. 3 2 }"
            let actual = ReverseArrayAxis x y
            let expected = JNounDefine @"{ |i. 3 -2 }"
            Assert.Equal(expected,actual)

        [<Fact>]
        member x.``FirstTrueInGroup`` () =
            let FirstTrueInGroup = JVerbMonadicDefine @"
{! {! ] *. |-. false , -1 >.- ] } false +. \ ] }"
            let y = JNounDefine @"{ false true false true false }"
            let actual = FirstTrueInGroup y
            let expected = JNounDefine @"{ false true false false false }"
            Assert.Equal(expected,actual)

        [<Fact>]
        member x.``GroupLengthFromBooleanGroupDescription`` () =
            let GroupLengthFromBooleanGroupDescription = JVerbMonadicDefine @"
    {! 
        {! 
            ] - ( |# ] ) <.- -1 , -1 >.- ]
        } 
        (  ( |# ] ) <.-  1 >.- ] , true ) /:: |i. |# ] 
    }"
            let y = JNounDefine @"{ true false false true false true false false }"
            let actual = GroupLengthFromBooleanGroupDescription  y
            let expected = JNounDefine @"{ 3 2 3 }"
            Assert.Equal(expected,actual)
            let y = JNounDefine @"{ 0 <.- true }"
            let actual = GroupLengthFromBooleanGroupDescription  y
            let expected = JNounDefine @"{ |i. 0 }"
            Assert.Equal(expected,actual)
            let y = JNounDefine @"{ 1 <.- true }"
            let actual = GroupLengthFromBooleanGroupDescription  y
            let expected = JRavel (JNounDefine @"{ 1 }")
            Assert.Equal(expected,actual)


        [<Fact>]
        member x.``GroupLengthFromCategoriesAndCategorization`` () =
            let GroupLengthFromCategoriesAndCategorization = JVerbDyadicDefine @"
    {! 
        ( ( |# ] ) = ] i. [ )
        {!
            ( |/: [ ) <- ] , ( 0 + / |_ [ = true ) $ 0
        } 
        {! 
            ] - ( |# ] ) <.- -1 , -1 >.- ]
        } 
        {!
            ( ( |# ] ) <.- 1 >.- ] , true ) /:: |i. |# ] 
        }
        ] ~: ( |# ] ) <.- 0 , -1 >.- ]
    }"
            let x = JNounDefine @"{ 1 2 3 }"
            let y = JNounDefine @"{ 1 1 1 2 2 }"
            let actual = GroupLengthFromCategoriesAndCategorization  x y
            let expected = JNounDefine @"{ 3 2 0 }"
            Assert.Equal(expected,actual)
            let x = JNounDefine @"{ |i. 0 }"
            let y = JNounDefine @"{ |i. 0 }"
            let actual = GroupLengthFromCategoriesAndCategorization  x y
            let expected = JNounDefine @"{ |i. 0 }"
            Assert.Equal(expected,actual)
            let x = JRavel (JNounDefine @"{ 1 }")
            let y = JNounDefine @"{ |i. 0 }"
            let actual = GroupLengthFromCategoriesAndCategorization  x y
            let expected = JRavel (JNounDefine @"{ 0 }")
            Assert.Equal(expected,actual)




        [<Fact>]
        member x.``cut indices `` () =
        //In case the cut testcase fails in this part of cut this testcase makes it easier to find the problem.
            let actual = JNounDefine @"
{ 
    true *. / ( ( |< 0 1 ) , ( |< 2 3 4 ) , |< |i. 0 ) = 
    {! 
        ( |< |, -1 ) {! |< ( 1 + ( |i. 0 ) $ -1 <.- |> [ ) + |i. ] } \ ] 
    } 2 3 0 
} "
            let expected = JNounDefine "{ true }"
            Assert.Equal(expected,actual)



        [<Fact>]
        member x.``cut `` () =
            let Cut = JVerbDyadicDefine @"
{! 
    ( 
        ( |< |, -1 ) {! |< ( ( |i. 0 ) $ 1 + -1 <.- |> [ ) + |i. ] } \ [ 
    )  
    {! |< ( |> [ ) <- ] } '/ 0 1 / ] 
} "
            let x = JNounDefine @"{ 2 3 0 }"
            let y = JNounDefine @"{ 1 2 3 4 5 }"
            let actual = Cut  x y
            let expected = JNounDefine @"{ ( |< 1 2 ) , ( |< 3 4 5 ) , |< |i. 0 }"
            Assert.Equal(expected,actual)
            let x = JNounDefine @"{ |i. 0 }"
            let y = JNounDefine @"{ |i. 0 }"
            let actual = Cut  x y
            let expected = JNounDefine @"{ 0 <.- |< |i. 0 }"
            Assert.Equal(expected,actual)
            let x = JNounDefine @"{ |, 0 }"
            let y = JNounDefine @"{ |i. 0 }"
            let actual = Cut  x y
            let expected = JNounDefine @"{ |, |< |i. 0 }"
            Assert.Equal(expected,actual)

        [<Fact>]
        member x.``Group mean`` () =
            let GroupMean = JVerbMonadicDefine @"
{!
    {! 
        |<
        {! ( 0 + / ] ) % |# ] } 
        |> ] 
    } 
    |'/ 0 / ]
}"
            let y = JNounDefine @"{ ( |< 4 2 8 ) , ( |< 13 7 9 2 ) , |< 0 1 }"
            let actual = GroupMean y
            let expected = JNounDefine @"{ |< |'/ 0 / ( 4 + 2 % 3 ) , 7.75 0.5 }"
            Assert.Equal(expected,actual)

        [<Fact>]
        member x.``Group mean i F#`` () =
            let each y u = 
                JRankMonadic y (fun yy -> JBox (u (JOpen yy))) ZeroNounConstant
            let mean  y = 
                JDivide (JFold ZeroNounConstant y JAdd) (JTally y)
            let sets = JNounDefine @"{ ( |< 4 2 8 ) , ( |< 13 7 9 2 ) , |< 0 1 }"
            let actual = each sets mean
            let expected = JNounDefine @"{ |< |'/ 0 / ( 4 + 2 % 3 ) , 7.75 0.5 }"
            Assert.Equal(expected,actual)


    type ``Use case magic`` () =
    //This use case is supposed to show that you can easily use JWithATwist as an extension to F#
    //Inputs and result is a simple record type you can use in your F# programs
    //The result of a verb definition is an F# function
    //If something goes wrong you get a J exception. See the exception list in the beginning of J4Mono.Base
    //The tests of the parts of magic is just to make it easier to troubleshoot problems.
    //"magic" is a code snippet by Roger Hui. This code is supposed to do the same thing, but within the limits
    //of the present JWithATwist implementation.  
    //magic=: ; @ (<@(</\&.|.);.2)~


        [<Fact>]
        member x.``magic `` () =
            let xNoun = JNounDefine     @"{ 0 0 0 1 0 0 1 0 0 1 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1 0 0 0 1 0 0 1 }"
            let yNoun = JNounDefine     @"{ 0 0 1 0 0 1 0 0 1 0 1 0 0 1 0 1 0 1 0 1 0 1 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1 0 0 0 1 0 0 }"
            let expected = JNounDefine  @"{ 0 0 1 0 0 1 0 0 1 0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1 0 0 0 1 0 0 }"
            let magic = JVerbDyadicDefine @"
{! 
    |> 
    ( |< |i. 0 ) {! |< ( |> [ ) , |> ] } /  
    ( 
        {! 
            ( |< |, -1 ) 
            {! 
                |< 
                ( 
                    1 + 
                    ( |i. 0 ) $ 
                    -1 <.- |> [ 
                ) + 
                |i. ] 
            } \ ] 
        } 
        {! 
            ] - 0 , -1 >.- ] 
        } 
        {! 
            ( 1 = ] ) /:: |i. |# ] 
        }  
        [ , 1 
    ) 
    {! 
        |< 
        {! 
            ( |i. |- |# ] ) <- ] 
        } 
        {! 
            ( |_ 1 = 0 + \  ] ) * ] 
        } 
        {! 
            ( |i. |- |# ] ) <- ] 
        } 
        (  |>  [ ) <- ] 
    } '/ 0 1 / ] 
} 
 "
            let actual = magic xNoun yNoun
            Assert.Equal(expected,actual)




        [<Fact>]
        member x.``magic reverse `` () =
            let actual = JNounDefine @"
{ 
    {! ( |i. |- |# ] ) <- ] } 1 2 3 
} "
            let expected = JNounDefine "{ 3 2 1 }"
            Assert.Equal(expected,actual)




        [<Fact>]
        member x.``magic pack up `` () =
            let actual = JNounDefine @"
{ 
    |> 
    ( |< |i. 0 ) {! |< ( |> [ ) , |> ] } / 
    ( |< 0 0 1 0 ) , |< 1 0 0 
} "
            let expected = JNounDefine "{ 0 0 1 0 1 0 0 }"
            Assert.Equal(expected,actual)


        [<Fact>]
        member x.``magic first true`` () =
            let actual = JNounDefine @"
{ 
    {! 
        ( |_ 1 = 0 + \  ] ) * ] 
    } 
    0 1 0 1 0 
} "
            let expected = JNounDefine "{ 0 1 0 0 0 }"
            Assert.Equal(expected,actual)

                 

        [<Fact>]
        member x.``magic calculate and pack indices `` () =
            let actual = JNounDefine @"
{ 
    true *. / ( ( |< 0 1 ) , ( |< 2 3 ) , |< 4 5 ) = 
    {! 
        {! 
            ( |< |, -1 ) {! |< ( 1 + ( |i. 0 ) $ -1 <.- |> [ ) + |i. ] } \ ] 
        } 
        {! 
            ] - 0 , -1 >.- ] 
        } 
        {! 
            ( 1 = ] ) /:: |i. |# ] 
        }  
        ] , 1 
    } 0 0 1 0 1 0 
} "
            let expected = JNounDefine "{ true }"
            Assert.Equal(expected,actual)
                 

        [<Fact>]
        member x.``magic group length `` () =
            let actual = JNounDefine @"
{ 
    true *. / 2 2 2 = 
    {! 
        {! 
            ] - 0 , -1 >.- ] 
        } 
        {!
            ( 1 = ] ) /:: |i. |# ] 
        }  
        ] , 1 
    } 0 0 1 0 1 0 
} "
            let expected = JNounDefine "{ true }"
            Assert.Equal(expected,actual)

    type ``Use case magic in F#-version`` () =
    //This use case is supposed to show that you can easily use JWithATwist as an extension to F#
    //You can use the language elements directly from F# and pass the interpreter.
    //You can define JWithATwist verbs and use as functions or lambda expressions.
    //If you define the verbs in the module, interpretation is only done once.
    //magic=: ; @ (<@(</\&.|.);.2)~

        [<Fact>]
        member x.``magic in F#-version `` () =
            let CreateBoxedGroupIndices = JVerbMonadicDefine @" 
{! 
    {! 
        ( |< |, -1 ) {! |< ( 1 + ( |i. 0 ) $ -1 <.- |> [ ) + |i. ] } \ ] 
    } 
    {! 
        ] - 0 , -1 >.- ] 
    } 
    {! 
        ( 1 = ] ) /:: |i. |# ] 
    }  
    ] , 1 
}"
            let FirstTrueInGroup = JVerbMonadicDefine @" 
{! 
    ( |_ 1 = 0 + \  ] ) * ] 
}"
            let Raze = JVerbMonadicDefine @"
{!
    |> 
    ( |< |i. 0 ) {! |< ( |> [ ) , |> ] } / ]
}"
            let Reverse = JVerbMonadicDefine @"
{! ( |i. |- |# ] ) <- ] }"
            let Cut xNoun yNoun = 
                JRankDyadic (CreateBoxedGroupIndices  xNoun) yNoun (JVerbDyadicDefine @"{! |< ( |> [ ) <- ] }" ) (JNounDefine @"{ 0 1 }")
            let magic xNoun yNoun =
                let ExecuteForEachBox yNoun =
                    JBox (Reverse(FirstTrueInGroup(Reverse (JOpen yNoun))))
                Raze (JRankMonadic (Cut xNoun yNoun) ExecuteForEachBox (JNounDefine @"{ 0 }"))
            let xNoun = JNounDefine     @"{ 0 0 0 1 0 0 1 0 0 1 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1 0 0 0 1 0 0 1 }"
            let yNoun = JNounDefine     @"{ 0 0 1 0 0 1 0 0 1 0 1 0 0 1 0 1 0 1 0 1 0 1 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1 0 0 0 1 0 0 }"
            let expected = JNounDefine  @"{ 0 0 1 0 0 1 0 0 1 0 1 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1 0 0 0 1 0 0 }"
            let actual = magic xNoun yNoun
            Assert.Equal(expected,actual)