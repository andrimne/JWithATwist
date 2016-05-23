(*
    JWithATwist Mock Parser - Parser for the programming language JWithATwist
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

open Microsoft.FSharp.Collections
open System
open FParsec

module ParserMock =

    type Noun =
        {
            Value: int
        }
    type VerbUnit = unit -> unit
    type NounUnit = unit -> Noun
    type VerbDyadic = Noun -> Noun -> Noun
    type VerbMonadic = Noun -> Noun 
    type VerbDyadicUnit = unit -> Noun -> Noun -> Noun
    type VerbMonadicUnit = unit -> Noun -> Noun 
    type AdverbMonadic = Noun -> VerbDyadic-> Noun 
    type AdverbDyadic = Noun -> Noun -> VerbDyadic-> Noun
    type ConjunctionMonadic = Noun -> VerbMonadic -> VerbDyadic -> Noun
    type ConjunctionDyadic = Noun -> Noun -> VerbMonadic -> VerbDyadic -> Noun
    type ParseResult = 
        {
            Value: ValueDU
        }
    and ValueDU =
    |TypeNounUnit of NounUnit
    |TypeVerbUnit of VerbUnit
    |TypeVerbMonadicUnit of VerbMonadicUnit
    |TypeVerbDyadicUnit of VerbDyadicUnit
    |TypeUnitUnit of VerbUnit
    |TypeODStart of unit
    |TypePStart of unit
    |TypeNoun of Noun
    |TypeVerbDyadic of VerbDyadic
    |TypeVerbMonadic of VerbMonadic
    |TypeNounFunctionDyadic of VerbDyadic
    |TypeNounFunctionMonadic of VerbMonadic
    |TypeAdverbMonadic of AdverbMonadic
    |TypeAdverbDyadic of AdverbDyadic
    |TypeConjunctionMonadic of ConjunctionMonadic
    |TypeConjunctionDyadic of ConjunctionDyadic


    type ParseResultList = ParseResult list

    exception ExceptionRankError
    exception ExceptionSyntaxError
    exception ExceptionDomainError
    exception ExceptionSystemError
    exception ExceptionValueError
    exception ExceptionLengthError
    exception ExceptionNotYetImplemented

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

    let setPRNoun (x:int) : ParseResult=
        let (noun :Noun)= {Value = x}
        let f () = noun
        {Value= TypeNounUnit f}

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
        {Value = TypeAdverbMonadic Insert}

    let setPRTable x =
        {Value = TypeAdverbDyadic Table}


    let setPRDeterminant x =
        {Value = TypeConjunctionMonadic Determinant}

    let setPRDotProduct x =
        {Value = TypeConjunctionDyadic DotProduct}

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

    let parseNoun =
        (pint32 .>> ((pchar ' ') .>> spaces) )

    let parseAdd =
        ((pstring "+ ")  .>> spaces) 

    let parseSubtract =
        ((pstring "- ") .>>  spaces) 

    let parseNegate =
        ((pstring "|- ") .>> spaces) 

    let parseInsert =
        ((pstring "/ ")  .>> spaces) 

    let parseTable =
        ((pstring "/- ")  .>> spaces) 

    let parseDeterminant =
        ((pstring "|. ")  .>> spaces) 

    let parseDotProduct  =
        ((pstring ". ")  .>> spaces) 


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
                                    |TypeVerbDyadic u,TypeAdverbMonadic a,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
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
                                                            //Monadic adverb - conjunctioncombination
                                                            |TypeVerbDyadic u,TypeAdverbMonadic a,TypeConjunctionMonadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
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
                                                                        //Dyadic adverb-conjunctioncombination
                                                                        |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbDyadic u,TypeAdverbMonadic a,TypeConjunctionDyadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
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
            |_ ->
                fail "syntax error"


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
                                    |TypeVerbDyadic u,TypeAdverbMonadic a,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
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
                                                            //Monadic adverb - conjunctioncombination
                                                            |TypeVerbDyadic u,TypeAdverbMonadic a,TypeConjunctionMonadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
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
                                                                        //Dyadic adverb-conjunctioncombination
                                                                        |(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _),TypeVerbDyadic u,TypeAdverbMonadic a,TypeConjunctionDyadic c,TypeVerbDyadic v,(TypeNounUnit _|TypeNounFunctionMonadic _ |TypeNounFunctionDyadic _) ->
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
            |_ ->
                fail "syntax error"

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
                //Dyadic conjunctions
                (parseDotProduct|>>setPRDotProduct)
                <|>
                //Monadic conjunctions
                (parseDeterminant|>>setPRDeterminant)

            )


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

    let parseAllSpeach inputStream =

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
            
        runParserOnString (spaces >>. parseAll ) () "" inputStream 
      


    let MockPrint (parseResult:ParseResult) =
        match parseResult with
        |{Value=TypeUnitUnit f} ->
            printfn "No result"
        |{Value=TypeVerbMonadic f} ->
            printfn "Monadic verb"
        |{Value=TypeVerbDyadic f} ->
            printfn "Dyadic verb"
        |{Value=TypeAdverbMonadic f} ->
            printfn "Monadic adverb"
        |{Value=TypeAdverbDyadic f} ->
            printfn "Dyadic adverb"
        |{Value=TypeConjunctionMonadic f} ->
            printfn "Monadic conjunction"
        |{Value=TypeConjunctionDyadic f} ->
            printfn "Dyadic conjunction"
        |{Value=TypeNounUnit f} ->
            let noun = f ()
            printfn "%A" noun.Value 
        |_ -> 
            raise ExceptionSystemError

    let MockParser () =
        let mutable quit = false
        while not quit do
            let inputStream = Console.ReadLine()
            quit <- inputStream.Length=0
            let inputStream = inputStream+" "
            match quit with
            |false ->
                try
                    match parseAllSpeach inputStream with
                    | Success(parseResultList, _, rest)   -> 
                        match parseResultList with
                        |[parseResult] ->
                            match parseResult with
                            |parseResult ->
                                MockPrint parseResult
                        |_ ->
                            raise ExceptionSystemError
                    | Failure(errorMsg, _, _) -> 
                        Console.WriteLine errorMsg
                    ()
                with
                |_->
                    ()
            |true 
                ->
                ()

