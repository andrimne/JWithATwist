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
namespace JWithATwist

open ParserMockDefinitions
open FParsec
open System
open Checked

module ParserMockInterface =

    let parseNoun:Parser<int,unit> =
        (pint32 .>> ((pchar ' ') .>> spaces) )

    let setPRNoun (x:int) : ParseResult=
        let (noun :Noun)= {Value = x}
        let f () = noun
        {Value= TypeNounUnit f}

    let Add (x:Noun) (y:Noun) : Noun =
        let r = x.Value + y.Value
        {Value= r}

    let Subtract (x:Noun) (y:Noun) : Noun =
        let r = x.Value - y.Value
        {Value= r}

    let Negate (y:Noun) : Noun =
        let r = - y.Value
        {Value= r}

    let Insert (y:Noun) (u:VerbDyadic) : Noun =
        u y y

    let Table (x:Noun) (y:Noun) (u:VerbDyadic) : Noun =
        u x y

    let InsertInsert (y:Noun) (u:VerbMonadic) : Noun =
        u y

    let Determinant  (y:Noun) (u:VerbMonadic) (v:VerbDyadic): Noun =
        u (v y y)

    let DotProduct  (x:Noun) (y:Noun) (u:VerbMonadic) (v:VerbDyadic): Noun =
        u (v x y)

    let setPRAdd x =
        {Value = TypeVerbDyadic Add}

    let setPRSubtract x =
        {Value = TypeVerbDyadic Subtract}

    let setPRNegate x =
        {Value = TypeVerbMonadic Negate}

    let setPRInsert x =
        {Value = TypeAdverbMonadicD Insert}

    let setPRTable x =
        {Value = TypeAdverbDyadic Table}

    let setPRInsertInsert x =
        {Value = TypeAdverbMonadicM InsertInsert}


    let setPRDeterminant x =
        {Value = TypeConjunctionMonadic Determinant}

    let setPRDotProduct x =
        {Value = TypeConjunctionDyadic DotProduct}

    let parseAdd:Parser<string,unit> =
        ((pstring "+ ")  .>> spaces) 

    let parseSubtract:Parser<string,unit> =
        ((pstring "- ") .>>  spaces) 

    let parseNegate:Parser<string,unit> =
        ((pstring "|- ") .>> spaces) 

    let parseInsert:Parser<string,unit> =
        ((pstring "/ ")  .>> spaces) 

    let parseInsertInsert:Parser<string,unit> =
        ((pstring "// ")  .>> spaces) 

    let parseTable:Parser<string,unit> =
        ((pstring "/- ")  .>> spaces) 

    let parseDeterminant:Parser<string,unit> =
        ((pstring "|. ")  .>> spaces) 

    let parseDotProduct:Parser<string,unit>  =
        ((pstring ". ")  .>> spaces) 

    let parseVerbAdverbConjunction =
        (
            //Dyadic verbs
            (parseAdd|>>setPRAdd)
            <|>
            (parseSubtract|>>setPRSubtract)
            <|>
            //Monadic verbs
            (parseNegate|>>setPRNegate)
            <|>
            //Dyadic adverbs
            (parseTable|>>setPRTable)
            <|>
            //Monadic adverbs
            (parseInsert|>>setPRInsert)
            <|>
            (parseInsertInsert|>>setPRInsertInsert)
            <|>
            //Dyadic conjunctions
            (parseDotProduct|>>setPRDotProduct)
            <|>
            //Monadic conjunctions
            (parseDeterminant|>>setPRDeterminant)

        )

    let ParsePrint (parseResult:ParseResult) =
        match parseResult with
        |{Value=TypeUnitUnit f} ->
            printfn "No result"
        |{Value=TypeVerbMonadic f} ->
            printfn "Monadic verb"
        |{Value=TypeVerbDyadic f} ->
            printfn "Dyadic verb"
        |{Value=TypeAdverbMonadicM f} ->
            printfn "Monadic adverb with monadic left verb"
        |{Value=TypeAdverbMonadicD f} ->
            printfn "Monadic adverb with dyadic left verb"
        |{Value=TypeAdverbDyadic f} ->
            printfn "Dyadic adverb"
        |{Value=TypeConjunctionMonadic f} ->
            printfn "Monadic conjunction"
        |{Value=TypeConjunctionDyadic f} ->
            printfn "Dyadic conjunction"
        |{Value=TypeNounUnit f} ->
            try
                let noun = f ()
                printfn "%A" noun.Value
            with
                | _ as e -> 
                    printfn "%s" e.Message
        |_ ->
            raise ExceptionSystemError
