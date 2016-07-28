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
//module Parser
open FParsec
open ParserDefinitions
open Base
open Checked

module ParserInterface =

    let setNounInteger (x:int64 list) :Noun=
        let rValue = List.toArray x
        let rShape =
            match rValue.Length with
            |1 ->
                [||]
            |_ ->
                [|rValue.Length|]
        {JType=JTBType.JTypeInteger;JShape=rShape;JValue=JTypeIntegerArray rValue}

    let setNounFloat (x:float list) :Noun=
        let rValue = List.toArray x
        let rShape =
            match rValue.Length with
            |1 ->
                [||]
            |_ ->
                [|rValue.Length|]
        {JType=JTBType.JTypeFloat;JShape=rShape;JValue=JTypeFloatArray rValue}

    let setNounBoolean (x:bool list) :Noun=
        let rValue = List.toArray x
        let rShape =
            match rValue.Length with
            |1 ->
                [||]
            |_ ->
                [|rValue.Length|]
        {JType=JTBType.JTypeBoolean;JShape=rShape;JValue=JTypeBooleanArray rValue}

    let setNounUnicode (x:string) :Noun=
        let rValue = Array.ofSeq x
        let rShape =
            match rValue.Length with
            |1 ->
                [||]
            |_ ->
                [|rValue.Length|]
        {JType=JTBType.JTypeUnicode;JShape=rShape;JValue=JTypeUnicodeArray rValue}

    let setNounBoxed (x:Noun list) :Noun=
        let rValue = Array.ofSeq x
        let rShape =
            match rValue.Length with
            |1 ->
                [||]
            |_ ->
                [|rValue.Length|]
        {JType=JTBType.JTypeBoxed;JShape=rShape;JValue=JTypeBoxedArray rValue}


    let parseNounInteger =
        ((attempt (many1 (( pint64) .>> ((pchar ' ') .>> spaces)) ))|>>setNounInteger)

    let parseNounFloat =
        ( (many1 (( pfloat) .>> ((pchar ' ') .>> spaces)) )|>>setNounFloat)

    let parseNounBoolean =
        ( (many1 (
            (
                (( pstring "true")>>. preturn true)
                <|>
                (( pstring "false")>>. preturn false)
            ) .>> ((pchar ' ') .>> spaces)) )|>>setNounBoolean)

    let parseNounUnicode =
        let parseStringPart =
            ( (pchar '\"') >>. (manyCharsTill anyChar (pchar '\"'))  )
        let parseString =
            (many1 parseStringPart)|>>(fun x -> String.concat "\"" x)|>>setNounUnicode
        (many1 ( parseString  .>> ( (pchar ' ') .>> spaces)))|>>setNounBoxed
       

    let parseNoun:Parser<Noun,unit> =
        (parseStart
            .>>.
            (
            parseNounInteger
            <|>
            parseNounBoolean
            <|>
            parseNounUnicode
            <|>
            parseNounFloat
            )|>> (fun (x,y) -> y)
        )


    let setPRNoun (x:Noun) : ParseResult =
        let f () = x
        {Value= TypeNounUnit f}

    //let setPRAddInteger x =
    //    {Value = TypeVerbDyadic JAdd}

    //Monadic verbs
    //Negate
    let setPRNegate x =
        {Value = TypeVerbMonadic JNegate}

    let parseNegate:Parser<string,unit> =
        ((pstring "|- ") .>> spaces) 

    //Magnitude
    let setPRMagnitude x =
        {Value = TypeVerbMonadic JMagnitude}

    let parseMagnitude:Parser<string,unit> =
        ((pstring "|| ") .>> spaces) 


    //Not
    let setPRNot x =
        {Value = TypeVerbMonadic JNot}

    let parseNot:Parser<string,unit> =
        ((pstring "|-. ") .>> spaces) 

    //Signum
    let setPRSignum x =
        {Value = TypeVerbMonadic JSignum}

    let parseSignum:Parser<string,unit> =
        ((pstring "|* ") .>> spaces) 

    //Floor
    let setPRFloor x =
        {Value = TypeVerbMonadic JFloor}

    let parseFloor:Parser<string,unit> =
        ((pstring "|<. ") .>> spaces) 

    //Ceiling
    let setPRCeiling x =
        {Value = TypeVerbMonadic JCeiling}

    let parseCeiling:Parser<string,unit> =
        ((pstring "|>. ") .>> spaces) 
    
    //Monadic Iota
    let setPRIotaMonadic x =
        {Value = TypeVerbMonadic JIotaMonadic}

    let parseIotaMonadic:Parser<string,unit> =
        ((pstring "|i. ") .>> spaces) 

    //ShapeOf
    let setPRShapeOf x =
        {Value = TypeVerbMonadic JShapeOf}

    let parseShapeOf:Parser<string,unit> =
        ((pstring "|$ ") .>> spaces) 

    //Ravel
    let setPRRavel x =
        {Value = TypeVerbMonadic JRavel}

    let parseRavel:Parser<string,unit> =
        ((pstring "|, ") .>> spaces) 

    //Tally
    let setPRTally x =
        {Value = TypeVerbMonadic JTally}

    let parseTally:Parser<string,unit> =
        ((pstring "|# ") .>> spaces) 

    //Bti
    let setPRBti x =
        {Value = TypeVerbMonadic JBti}

    let parseBti:Parser<string,unit> =
        ((pstring "|_ ") .>> spaces) 

    //Reciprocal
    let setPRReciprocal x =
        {Value = TypeVerbMonadic JReciprocal}

    let parseReciprocal:Parser<string,unit> =
        ((pstring "|% ") .>> spaces) 

    //Box
    let setPRBox x =
        {Value = TypeVerbMonadic JBox}

    let parseBox:Parser<string,unit> =
        ((pstring "|< ") .>> spaces) 

    //Open
    let setPROpen x =
        {Value = TypeVerbMonadic JOpen}

    let parseOpen:Parser<string,unit> =
        ((pstring "|> ") .>> spaces) 

    //Open
    let setPRGradeUp x =
        {Value = TypeVerbMonadic JGradeUp}

    let parseGradeUp:Parser<string,unit> =
        ((pstring "|/: ") .>> spaces) 

    //Default format
    let setPRDefaultFormat x =
        {Value = TypeVerbMonadic JDefaultFormat}

    let parseDefaultFormat:Parser<string,unit> =
        ((pstring "|\': ") .>> spaces) 


    //Dyadic verbs  
    // Add 

    let setPRAdd x =
        {Value = TypeVerbDyadic JAdd}

    let parseAdd:Parser<string,unit> =
        ((pstring "+ ")  .>> spaces) 

    // Subtract

    let setPRSubtract x =
        {Value = TypeVerbDyadic JSubtract}

    let parseSubtract:Parser<string,unit> =
        ((pstring "- ")  .>> spaces) 

    // Times

    let setPRTimes x =
        {Value = TypeVerbDyadic JTimes}

    let parseTimes:Parser<string,unit> =
        ((pstring "* ")  .>> spaces) 

    // Divide

    let setPRDivide x =
        {Value = TypeVerbDyadic JDivide}

    let parseDivide:Parser<string,unit> =
        ((pstring "% ")  .>> spaces) 

    // Power

    let setPRPower x =
        {Value = TypeVerbDyadic JPower}

    let parsePower:Parser<string,unit> =
        ((pstring "^ ")  .>> spaces) 

    // Min

    let setPRMin x =
        {Value = TypeVerbDyadic JMin}

    let parseMin:Parser<string,unit> =
        ((pstring "<. ")  .>> spaces) 

    // Max

    let setPRMax x =
        {Value = TypeVerbDyadic JMax}

    let parseMax:Parser<string,unit> =
        ((pstring ">. ")  .>> spaces) 

    // And

    let setPRAnd x =
        {Value = TypeVerbDyadic JAnd}

    let parseAnd:Parser<string,unit> =
        ((pstring "*. ")  .>> spaces) 

    // Or

    let setPROr x =
        {Value = TypeVerbDyadic JOr}

    let parseOr:Parser<string,unit> =
        ((pstring "+. ")  .>> spaces) 

    // Equal

    let setPREqual x =
        {Value = TypeVerbDyadic JEqual}

    let parseEqual:Parser<string,unit> =
        ((pstring "= ")  .>> spaces) 

    // Not Equal

    let setPRNotEqual x =
        {Value = TypeVerbDyadic JNotEqual}

    let parseNotEqual:Parser<string,unit> =
        ((pstring "~: ")  .>> spaces) 

    // LessThan

    let setPRLessThan x =
        {Value = TypeVerbDyadic JLessThan}

    let parseLessThan:Parser<string,unit> =
        ((pstring "< ")  .>> spaces) 

    // LargerThan

    let setPRLargerThan x =
        {Value = TypeVerbDyadic JLargerThan}

    let parseLargerThan:Parser<string,unit> =
        ((pstring "> ")  .>> spaces) 

    // LessOrEqual

    let setPRLessOrEqual x =
        {Value = TypeVerbDyadic JLessOrEqual}

    let parseLessOrEqual:Parser<string,unit> =
        ((pstring "<: ")  .>> spaces) 

    // LargerOrEqual

    let setPRLargerOrEqual x =
        {Value = TypeVerbDyadic JLargerOrEqual}

    let parseLargerOrEqual:Parser<string,unit> =
        ((pstring ">: ")  .>> spaces) 

    //Shape
    let setPRShapeDyadic x =
        {Value = TypeVerbDyadic JShape}

    let parseShapeDyadic:Parser<string,unit> =
        ((pstring "$ ")  .>> spaces) 

    //Take
    let setPRTake x =
        {Value = TypeVerbDyadic JTake}

    let parseTake:Parser<string,unit> =
        ((pstring "<.- ")  .>> spaces) 

    //Drop
    let setPRDrop x =
        {Value = TypeVerbDyadic JDrop}

    let parseDrop:Parser<string,unit> =
        ((pstring ">.- ")  .>> spaces) 

    //Catenate
    let setPRCatenate x =
        {Value = TypeVerbDyadic JCatenate}

    let parseCatenate:Parser<string,unit> =
        ((pstring ", ")  .>> spaces) 

    //From
    let setPRFrom x =
        {Value = TypeVerbDyadic JFrom}

    let parseFrom:Parser<string,unit> =
        ((pstring "<- ")  .>> spaces) 

    //Amend
    let setPRAmend x =
        {Value = TypeVerbDyadic JAmend}

    let parseAmend:Parser<string,unit> =
        ((pstring ">- ")  .>> spaces) 

    //Select
    let setPRSelect x =
        {Value = TypeVerbDyadic JSelect}

    let parseSelect:Parser<string,unit> =
        ((pstring "/:: ")  .>> spaces) 

    //Replicate
    let setPRReplicate x =
        {Value = TypeVerbDyadic JReplicate}

    let parseReplicate:Parser<string,unit> =
        ((pstring "# ")  .>> spaces) 

    //Dyadic Iota
    let setPRIotaDyadic x =
        {Value = TypeVerbDyadic JIotaDyadic}

    let parseIotaDyadic:Parser<string,unit> =
        ((pstring "i. ")  .>> spaces) 

    //Transpose
    let setPRTranspose x =
        {Value = TypeVerbDyadic JTranspose}

    let parseTranspose:Parser<string,unit> =
        ((pstring "\:: ")  .>> spaces) 

    //Monadic adverbs
    //Monadic Rank
    let setPRRankMonadic (n:Noun) =
        let f  (y:Noun) (u:VerbMonadic) : Noun = JRankMonadic y u n
        {Value = TypeAdverbMonadicM f}

    let parseRankMonadic:Parser<Noun,unit> =
        ((pstring "|\'/ ") >>. ( parseNounInteger ) .>> (pstring"/ ").>> spaces) 



    //Dyadic adverbs
    //Dyadic Rank
    let setPRRankDyadic (n:Noun) =
        let f  (x:Noun) (y:Noun) (u:VerbDyadic) : Noun = JRankDyadic x y u n
        {Value = TypeAdverbDyadic f}

    let parseRankDyadic:Parser<Noun,unit> =
        ((pstring "\'/ ") >>. ( parseNounInteger ) .>> (pstring"/ ").>> spaces) 

    //Dyadic Fold
    let setPRFold x =
        let f  (x:Noun) (y:Noun) (u:VerbDyadic) : Noun = JFold x y u 
        {Value = TypeAdverbDyadic f}

    let parseFold:Parser<string,unit> =
        ((pstring "/ ") .>> spaces) 

    //Dyadic Scan
    let setPRScan x =
        let f  (x:Noun) (y:Noun) (u:VerbDyadic) : Noun = JScan x y u 
        {Value = TypeAdverbDyadic f}

    let parseScan:Parser<string,unit> =
        ((pstring @"\ ") .>> spaces) 



    let parseVerbAdverbConjunction =
        (
            //Monadic verbs
            (parseNegate|>>setPRNegate)
            <|>
            (parseMagnitude|>>setPRMagnitude)
            <|>
            (parseNot|>>setPRNot)
            <|>
            (parseReciprocal|>>setPRReciprocal)
            <|>
            (parseBox|>>setPRBox)
            <|>
            (parseOpen|>>setPROpen)
            <|>
            (parseIotaMonadic|>>setPRIotaMonadic)
            <|>
            (parseShapeOf|>>setPRShapeOf)
            <|>
            (parseRavel|>>setPRRavel)
            <|>
            (parseTally|>>setPRTally)
            <|>
            (parseSignum|>>setPRSignum)
            <|>
            (parseBti|>>setPRBti)
            <|>
            (parseFloor|>>setPRFloor)
            <|>
            (parseCeiling|>>setPRCeiling)
            <|>
            (parseGradeUp|>>setPRGradeUp)
            <|>
            (parseDefaultFormat|>>setPRDefaultFormat)
            <|>
            //Dyadic verbs
            //
            (parseAdd|>>setPRAdd)
            <|>
            (parseSubtract|>>setPRSubtract)
            <|>
            (parseTimes|>>setPRTimes)
            <|>
            (parseDivide|>>setPRDivide)
            <|>
            (parsePower|>>setPRPower)
            <|>
            (parseMin|>>setPRMin)
            <|>
            (parseMax|>>setPRMax)
            <|>
            (parseAnd|>>setPRAnd)
            <|>
            (parseOr|>>setPROr)
            <|>
            (parseEqual|>>setPREqual)
            <|>
            (parseNotEqual|>>setPRNotEqual)
            <|>
            (parseLessThan|>>setPRLessThan)
            <|>
            (parseLargerThan|>>setPRLargerThan)
            <|>
            (parseLessOrEqual|>>setPRLessOrEqual)
            <|>
            (parseLargerOrEqual|>>setPRLargerOrEqual)
            <|>
            (parseShapeDyadic|>>setPRShapeDyadic)
            <|>
            (parseTake|>>setPRTake)
            <|>
            (parseDrop|>>setPRDrop)
            <|>
            (parseFrom|>>setPRFrom)
            <|>
            (parseAmend|>>setPRAmend)
            <|>
            (parseCatenate|>>setPRCatenate)
            <|>
            (parseSelect|>>setPRSelect)
            <|>
            (parseReplicate|>>setPRReplicate)
            <|>
            (parseIotaDyadic|>>setPRIotaDyadic)
            <|>
            (parseTranspose|>>setPRTranspose)
            <|>
            //Monadic adverbs
            (parseRankMonadic|>>setPRRankMonadic)
            <|>
            //Dyadic adverbs
            (parseRankDyadic|>>setPRRankDyadic)
            <|>
            (parseFold|>>setPRFold)
            <|>
            (parseScan|>>setPRScan)
            //Dyadic conjunctions
            //Monadic conjunctions
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
            //let noun = f ()
            //JPrint noun
            try
                let noun = f ()
                JPrint noun
            with
                | :? JExceptionRankError -> 
                    printfn "Rank error"
                | :? JExceptionDomainError -> 
                    printfn "Domain error"
                | :? JExceptionSystemError -> 
                    printfn "System error"
                | :? JExceptionValueError -> 
                    printfn "Value error"
                | :? JExceptionLengthError -> 
                    printfn "Length error"
                | :? JExceptionIndexError -> 
                    printfn "Index error"
                | :? JExceptionStackFull -> 
                    printfn "Stack full"
                | :? JExceptionMemoryFull -> 
                    printfn "Memory full"
                | _ as e -> 
                    printfn "%s" e.Message
        |_ -> 
            raise ExceptionSystemError