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

//OBS ! You are not supposed to change this program.
//You should:
//1. Modify the mock parser and run the tests.
//2. Replace this program with the new mock parser.
//3. Replace "open ParserMockDefinitions" with "open ParserDefinitions"
//4. Replace "open ParserMockInterface" with "open ParserInterface"
//4. Change module name to Parser

open Microsoft.FSharp.Collections
open Checked
open System
open FParsec
//open J4Mono.Base
open ParserDefinitions
open ParserInterface
//open J4Mono.ParserMockDefinitions
//open J4Mono.ParserMockInterface


module Parser =

    let parseNounTrain, parseNounTrainImpl = createParserForwardedToRef()

    let parseBracketedNounTrain, parseBracketedNounTrainImpl = createParserForwardedToRef()

    let parseODNoun, parseODNounImpl = createParserForwardedToRef()
    let parseODVerbAdverbConjunction, parseODVerbAdverbConjunctionImpl = createParserForwardedToRef()


    let parseStart:Parser<ParseResultList,unit> =
        let (parseResultList:ParseResultList) = []
        (preturn parseResultList)

    let setPRODStart x =
        {Value = TypeODStart ()}

    let setPRPStart x =
        {Value = TypePStart ()}

    let setPROD x =
        match x with
        |[hd] ->
            hd
        |_ ->
            raise ExceptionSystemError



    let setPRRightNoun (x:string) : ParseResult =
        let f (y:Noun) : Noun = y
        {Value= TypeNounFunctionMonadic f}

    let setPRLeftNoun (x:string) : ParseResult =
        let f (x:Noun) (y:Noun) : Noun = x
        {Value= TypeNounFunctionDyadic f}

    let setPRUnit x =
        let f ()  =
            ()
        {Value = TypeUnitUnit f}



    let addPR (x,y) =
        y::x

    let addPRL (x,y) =
        List.append y x

    let addPRLRev (x,y) =
        List.append (List.rev y) x

    let concatPRLRev (x:ParseResult list list) =
        List.concat (List.toSeq  (List.rev x))

    let parseODNounStart : Parser<string,unit> =
        ((pstring "{ ") .>>  spaces )

    let parseODVerbAdverbConjunctionStart =
        ((pstring "{!" )  .>> spaces )

    let parseODEnd =
        ((pstring "} ") .>>  spaces) 

    let parsePStart : Parser<string,unit> =
        ((pstring "( ") .>> spaces) 

    let parsePEnd =
        ((pstring ") ")  .>> spaces) 

    let parseRightNoun =
        ((pstring "] ")  .>> spaces ) 

    let parseLeftNoun =
        ((pstring "[ ")  .>> spaces ) 

    let parseEOF =
        (spaces>>.eof )



    let parseP (pRL,y) =
        let rec parseRec (pRL:ParseResult list) =
            match pRL with
            |[] ->
                raise ExceptionSystemError                
            |hd::tl ->
                let l1 = hd::[]
                match tl with
                |[] ->
                    raise ExceptionSystemError                    
                |hd::tl ->
                    let l2 = hd::l1
                    match l2 with
                    |[bb;aa] ->
                        match bb.Value,aa.Value with
                        //Monadic verb application
                        |TypeVerbMonadic v,TypeNounUnit y ->
                            let f () = v (y ())
                            parseRec ({Value= TypeNounUnit f}::(List.skip 2 pRL))
                        |TypeVerbMonadic v,TypeNounFunctionMonadic y ->
                            let f yy = v (y yy)
                            parseRec ({Value= TypeNounFunctionMonadic f}::(List.skip 2 pRL))
                        |TypeVerbMonadic v,TypeNounFunctionDyadic y ->
                            let f xx yy = v (y xx yy)
                            parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 2 pRL))
                        //Parse results
                        |TypePStart _,TypeNounUnit n ->
                            {Value= TypeNounUnit n}::(List.skip 2 pRL)
                        |TypePStart _,TypeNounFunctionMonadic f ->
                            {Value= TypeNounFunctionMonadic f}::(List.skip 2 pRL)
                        |TypePStart _,TypeNounFunctionDyadic f ->
                            {Value= TypeNounFunctionDyadic f}::(List.skip 2 pRL)
                        |_ ->
                            match tl with
                            |[] ->
                                raise ExceptionSyntaxError
                            |hd::tl ->
                                let l3 = hd::l2
                                match l3 with
                                |[cc;bb;aa] ->
                                    match cc.Value,bb.Value,aa.Value with
                                    //Dyadic verb application
                                    //Any noun type - Type noun unit
                                    |TypeNounUnit x,TypeVerbDyadic v,TypeNounUnit y ->
                                        let f () = v (x ()) (y ())
                                        parseRec ({Value= TypeNounUnit f}::(List.skip 3 pRL))
                                    |TypeNounFunctionMonadic x,TypeVerbDyadic u, TypeNounUnit y ->
                                        let f yy = u (x yy) (y ())
                                        parseRec ({Value= TypeNounFunctionMonadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionDyadic x,TypeVerbDyadic u, TypeNounUnit y ->
                                        let f xx yy = u (x xx yy) (y ())
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    //Any noun type - Type nounfunction monadic
                                    |TypeNounUnit x,TypeVerbDyadic u,TypeNounFunctionMonadic y ->
                                        let f yy = u (x ()) (y yy)
                                        parseRec ({Value= TypeNounFunctionMonadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionMonadic x,TypeVerbDyadic u, TypeNounFunctionMonadic y ->
                                        let f yy = u (x yy) (y yy)
                                        parseRec ({Value= TypeNounFunctionMonadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionDyadic x,TypeVerbDyadic u, TypeNounFunctionMonadic y ->
                                        let f xx yy = u (x xx yy) (y yy)
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    //Any noun type - Type nounfunction dyadic
                                    |TypeNounUnit x,TypeVerbDyadic u,TypeNounFunctionDyadic y ->
                                        let f xx yy = u (x ()) (y xx yy)
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionMonadic x,TypeVerbDyadic u, TypeNounFunctionDyadic y ->
                                        let f xx yy = u (x yy) (y xx yy)
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionDyadic x,TypeVerbDyadic u, TypeNounFunctionDyadic y ->
                                        let f xx yy = u (x xx yy) (y xx yy)
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    //Monadic adverb
                                    |TypeVerbMonadic u,TypeAdverbMonadicM a,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                        let f yy = a yy u
                                        parseRec (aa::({Value= TypeVerbMonadic f}::(List.skip 3 pRL)))
                                    |TypeVerbDyadic u,TypeAdverbMonadicD a,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                        let f yy = a yy u
                                        parseRec (aa::({Value= TypeVerbMonadic f}::(List.skip 3 pRL)))
                                    |_ ->
                                        match tl with
                                        |[] ->
                                            raise ExceptionSyntaxError
                                        |hd::tl ->
                                            let l4 = hd::l3
                                            match l4 with 
                                            |[dd;cc;bb;aa] ->
                                                match dd.Value,cc.Value,bb.Value,aa.Value with
                                                //Dyadic adverb
                                                |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbDyadic u,TypeAdverbDyadic a,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                    let f xx yy = a xx yy u
                                                    parseRec (aa::({Value= TypeVerbDyadic f}::(List.skip 3 pRL)))
                                                //Monadic conjunction
                                                |TypeVerbMonadic u,TypeConjunctionMonadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                    let f yy = c yy u v
                                                    parseRec (aa::{Value= TypeVerbMonadic f}::(List.skip 4 pRL))
                                                |_->
                                                    match tl with
                                                    |[] ->
                                                        raise ExceptionSyntaxError
                                                    |hd::tl ->
                                                        let l5 = hd::l4
                                                        match l5 with 
                                                        |[ee;dd;cc;bb;aa] ->
                                                            match ee.Value,dd.Value,cc.Value,bb.Value,aa.Value with
                                                            //Dyadic conjunction
                                                            |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbMonadic u,TypeConjunctionDyadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                let f xx yy = c xx yy u v
                                                                parseRec (aa::({Value= TypeVerbDyadic f}::(List.skip 4 pRL)))
                                                            //Monadic adverb - monadic conjunctioncombination
                                                            |TypeVerbMonadic u,TypeAdverbMonadicM a,TypeConjunctionMonadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                let leftVerb y = a y u 
                                                                let f yy = c yy leftVerb v
                                                                parseRec (aa::({Value= TypeVerbMonadic f}::(List.skip 5 pRL)))
                                                            |TypeVerbDyadic u,TypeAdverbMonadicD a,TypeConjunctionMonadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                let leftVerb y = a y u 
                                                                let f yy = c yy leftVerb v
                                                                parseRec (aa::({Value= TypeVerbMonadic f}::(List.skip 5 pRL)))
                                                            |_->
                                                                match tl with
                                                                |[] ->
                                                                    raise ExceptionSyntaxError
                                                                |hd::tl ->
                                                                    let l6 = hd::l5
                                                                    match l6 with 
                                                                    |[ff;ee;dd;cc;bb;aa] ->  
                                                                        match ff.Value,ee.Value,dd.Value,cc.Value,bb.Value,aa.Value with
                                                                        //Monadic adverb-dyadic conjunctioncombination
                                                                        |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbMonadic u,TypeAdverbMonadicM a,TypeConjunctionDyadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                            let leftVerb y = a y u 
                                                                            let f xx yy = c xx yy leftVerb v
                                                                            parseRec (aa::({Value= TypeVerbDyadic f}::(List.skip 5 pRL)))
                                                                        |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbDyadic u,TypeAdverbMonadicD a,TypeConjunctionDyadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                            let leftVerb y = a y u 
                                                                            let f xx yy = c xx yy leftVerb v
                                                                            parseRec (aa::({Value= TypeVerbDyadic f}::(List.skip 5 pRL)))
                                                                        |_ ->
                                                                            raise ExceptionSyntaxError    
                                                                    | _ ->
                                                                        raise ExceptionSystemError                                                                                                                                              
                                                                                                                                
                                                        | _ ->
                                                            raise ExceptionSystemError
                                            | _ ->
                                                raise ExceptionSystemError
                                |_->
                                    raise ExceptionSystemError
                    |_->
                        raise ExceptionSystemError

        try 
            preturn (parseRec pRL)
        with
            | :? ExceptionSyntaxError -> 
                fail "syntax error"
            | :? ExceptionSystemError -> 
                fail "system error"

    let parseOD (pRL,y) =
        let rec parseRec (pRL:ParseResult list) =
            match pRL with
            |[] ->
                raise ExceptionSystemError                
            |hd::tl ->
                let l1 = hd::[]
                match tl with
                |[] ->
                    raise ExceptionSystemError                    
                |hd::tl ->
                    let l2 = hd::l1
                    match l2 with
                    |[bb;aa] ->
                        match bb.Value,aa.Value with
                        //Monadic verb application
                        |TypeVerbMonadic v,TypeNounUnit y ->
                            let f () = v (y ())
                            parseRec ({Value= TypeNounUnit f}::(List.skip 2 pRL))
                        |TypeVerbMonadic v,TypeNounFunctionMonadic y ->
                            let f yy = v (y yy)
                            parseRec ({Value= TypeNounFunctionMonadic f}::(List.skip 2 pRL))
                        |TypeVerbMonadic v,TypeNounFunctionDyadic y ->
                            let f xx yy = v (y xx yy)
                            parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 2 pRL))
                        //Parse results
                        |TypeODStart _,TypeUnitUnit f ->
                            aa::(List.skip 2 pRL)
                        |TypeODStart _,TypeNounUnit n ->
                            {Value= TypeNounUnit n}::(List.skip 2 pRL)
                        |TypeODStart _,TypeNounFunctionMonadic f ->
                            {Value= TypeVerbMonadic f}::(List.skip 2 pRL)
                        |TypeODStart _,TypeNounFunctionDyadic f ->
                            {Value= TypeVerbDyadic f}::(List.skip 2 pRL)
                        |_ ->
                            match tl with
                            |[] ->
                                raise ExceptionSyntaxError
                            |hd::tl ->
                                let l3 = hd::l2
                                match l3 with
                                |[cc;bb;aa] ->
                                    match cc.Value,bb.Value,aa.Value with
                                    //Dyadic verb application
                                    //Any noun type - Type noun unit
                                    |TypeNounUnit x,TypeVerbDyadic v,TypeNounUnit y ->
                                        let f () = v (x ()) (y ())
                                        parseRec ({Value= TypeNounUnit f}::(List.skip 3 pRL))
                                    |TypeNounFunctionMonadic x,TypeVerbDyadic u, TypeNounUnit y ->
                                        let f yy = u (x yy) (y ())
                                        parseRec ({Value= TypeNounFunctionMonadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionDyadic x,TypeVerbDyadic u, TypeNounUnit y ->
                                        let f xx yy = u (x xx yy) (y ())
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    //Any noun type - Type nounfunction monadic
                                    |TypeNounUnit x,TypeVerbDyadic u,TypeNounFunctionMonadic y ->
                                        let f yy = u (x ()) (y yy)
                                        parseRec ({Value= TypeNounFunctionMonadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionMonadic x,TypeVerbDyadic u, TypeNounFunctionMonadic y ->
                                        let f yy = u (x yy) (y yy)
                                        parseRec ({Value= TypeNounFunctionMonadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionDyadic x,TypeVerbDyadic u, TypeNounFunctionMonadic y ->
                                        let f xx yy = u (x xx yy) (y yy)
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    //Any noun type - Type nounfunction dyadic
                                    |TypeNounUnit x,TypeVerbDyadic u,TypeNounFunctionDyadic y ->
                                        let f xx yy = u (x ()) (y xx yy)
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionMonadic x,TypeVerbDyadic u, TypeNounFunctionDyadic y ->
                                        let f xx yy = u (x yy) (y xx yy)
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    |TypeNounFunctionDyadic x,TypeVerbDyadic u, TypeNounFunctionDyadic y ->
                                        let f xx yy = u (x xx yy) (y xx yy)
                                        parseRec ({Value= TypeNounFunctionDyadic f}::(List.skip 3 pRL))
                                    //Monadic adverb
                                    |TypeVerbMonadic u,TypeAdverbMonadicM a,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                        let f yy = a yy u
                                        parseRec (aa::({Value= TypeVerbMonadic f}::(List.skip 3 pRL)))
                                    |TypeVerbDyadic u,TypeAdverbMonadicD a,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                        let f yy = a yy u
                                        parseRec (aa::({Value= TypeVerbMonadic f}::(List.skip 3 pRL)))
                                    |_ ->
                                        match tl with
                                        |[] ->
                                            raise ExceptionSyntaxError
                                        |hd::tl ->
                                            let l4 = hd::l3
                                            match l4 with 
                                            |[dd;cc;bb;aa] ->
                                                match dd.Value,cc.Value,bb.Value,aa.Value with
                                                //Dyadic adverb
                                                |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbDyadic u,TypeAdverbDyadic a,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                    let f xx yy = a xx yy u
                                                    parseRec (aa::({Value= TypeVerbDyadic f}::(List.skip 3 pRL)))
                                                //Monadic conjunction
                                                |TypeVerbMonadic u,TypeConjunctionMonadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                    let f yy = c yy u v
                                                    parseRec (aa::{Value= TypeVerbMonadic f}::(List.skip 4 pRL))
                                                |_->
                                                    match tl with
                                                    |[] ->
                                                        raise ExceptionSyntaxError
                                                    |hd::tl ->
                                                        let l5 = hd::l4
                                                        match l5 with 
                                                        |[ee;dd;cc;bb;aa] ->
                                                            match ee.Value,dd.Value,cc.Value,bb.Value,aa.Value with
                                                            //Dyadic conjunction
                                                            |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbMonadic u,TypeConjunctionDyadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                let f xx yy = c xx yy u v
                                                                parseRec (aa::({Value= TypeVerbDyadic f}::(List.skip 4 pRL)))
                                                            //Monadic adverb - monadic conjunctioncombination
                                                            |TypeVerbMonadic u,TypeAdverbMonadicM a,TypeConjunctionMonadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                let leftVerb y = a y u 
                                                                let f yy = c yy leftVerb v
                                                                parseRec (aa::({Value= TypeVerbMonadic f}::(List.skip 5 pRL)))
                                                            |TypeVerbDyadic u,TypeAdverbMonadicD a,TypeConjunctionMonadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                let leftVerb y = a y u 
                                                                let f yy = c yy leftVerb v
                                                                parseRec (aa::({Value= TypeVerbMonadic f}::(List.skip 5 pRL)))
                                                            |_->
                                                                match tl with
                                                                |[] ->
                                                                    raise ExceptionSyntaxError
                                                                |hd::tl ->
                                                                    let l6 = hd::l5
                                                                    match l6 with 
                                                                    |[ff;ee;dd;cc;bb;aa] ->  
                                                                        match ff.Value,ee.Value,dd.Value,cc.Value,bb.Value,aa.Value with
                                                                        //Monadic adverb-dyadic conjunctioncombination
                                                                        |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbMonadic u,TypeAdverbMonadicM a,TypeConjunctionDyadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                            let leftVerb y = a y u 
                                                                            let f xx yy = c xx yy leftVerb v
                                                                            parseRec (aa::({Value= TypeVerbDyadic f}::(List.skip 5 pRL)))
                                                                        |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbDyadic u,TypeAdverbMonadicD a,TypeConjunctionDyadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
                                                                            let leftVerb y = a y u 
                                                                            let f xx yy = c xx yy leftVerb v
                                                                            parseRec (aa::({Value= TypeVerbDyadic f}::(List.skip 5 pRL)))
                                                                        |_ ->
                                                                            raise ExceptionSyntaxError    
                                                                    | _ ->
                                                                        raise ExceptionSystemError                                                                                                                                              
                                                                                                                                
                                                        | _ ->
                                                            raise ExceptionSystemError
                                            | _ ->
                                                raise ExceptionSystemError
                                |_->
                                    raise ExceptionSystemError
                    |_->
                        raise ExceptionSystemError

        try 
            preturn (parseRec pRL)
        with
            | :? ExceptionSyntaxError -> 
                fail "syntax error"
            | :? ExceptionSystemError -> 
                fail "system error"




    let parseVerbTrain  =
        (
            parseStart
            .>>.
                (
                many1 
                    (
                        parseVerbAdverbConjunction
                        <|>
                        (parseODVerbAdverbConjunction|>>setPROD)
                    )
                )|>>addPRLRev
        )


    

    let parseNounTrainEnd =
        (
            many1 
                (
                    parseVerbTrain
                    .>>.
                    (
                        (
                            (
                                parseStart
                                .>>.
                                (
                                    (parseNoun|>>setPRNoun)
                                    <|>
                                    (parseRightNoun|>>setPRRightNoun)
                                    <|>
                                    (parseLeftNoun|>>setPRLeftNoun)
                                )|>>addPR
                            )
                            <|>
                            parseBracketedNounTrain
                            <|>
                            parseODNoun
                        )
                    )|>>addPRL
                   
                )
        )|>>concatPRLRev



    parseNounTrainImpl :=
        (
            (   
                (
                    (
                    parseStart
                    .>>.
                    (
                        (parseNoun|>>setPRNoun)
                        <|>
                        (parseRightNoun|>>setPRRightNoun)
                        <|>
                        (parseLeftNoun|>>setPRLeftNoun)
                    )|>>addPR
                    )
                    <|>
                    parseBracketedNounTrain
                    <|>
                    parseODNoun
                )
                .>>.
                    (
                        (
                            parseNounTrainEnd
                        )
                        <|>
                        (
                            parseStart
                        )
                    )
            )|>>addPRL
            <|>
            (   
                parseNounTrainEnd
            )
            <|>
            (
                parseStart
                .>>.
                (preturn (setPRUnit ()))|>>addPR
            )
        )

    parseBracketedNounTrainImpl :=
        (
            parseStart
            .>>.
            (parsePStart|>>setPRPStart)|>>addPR
            .>>.
            parseNounTrain|>>addPRL
            .>>.
                (parsePEnd)>>=parseP
        )

  
    parseODNounImpl :=
        (
            parseStart
            .>>.
            (parseODNounStart|>>setPRODStart)|>>addPR
            .>>.
            parseNounTrain|>>addPRL
            .>>.
                (parseODEnd)>>=parseOD
        )           

    parseODVerbAdverbConjunctionImpl :=
        (
            parseStart
            .>>.
            (parseODVerbAdverbConjunctionStart|>>setPRODStart)|>>addPR
            .>>.
            parseNounTrain|>>addPRL
            .>>.
                (parseODEnd)>>=parseOD
        ) 

    let parseAllSpeach (inputStream:String) =

        let parseAll =
            ( 
                parseStart
                .>>. 
                (
                    parseODNoun
                    <|>
                    parseODVerbAdverbConjunction
                    <|>
                    (
                        parseStart
                        .>>.
                        ((preturn 0)|>>setPRUnit )|>>addPR
                    )
                 )|>>addPRL              
                .>> parseEOF
            )
        let modifiedInputStream = inputStream.Replace(Environment.NewLine," "+Environment.NewLine)
        runParserOnString (spaces >>. parseAll ) () "" modifiedInputStream 
  


    let Parser () =
        let mutable quit = false
        while not quit do
            let inputStream = Console.ReadLine()
            quit <- inputStream.Length=0
            let inputStream = inputStream+" "
            match quit with
            |false ->
                match parseAllSpeach inputStream with
                | Success(parseResultList, _, rest)   -> 
                    match parseResultList with
                    |[parseResult] ->
                        match parseResult with
                        |parseResult ->
                            ParsePrint parseResult
                    |_ ->
                        raise ExceptionSystemError
                | Failure(errorMsg, _, _) -> 
                    printfn "%s" errorMsg
            |true 
                ->
                ()                         