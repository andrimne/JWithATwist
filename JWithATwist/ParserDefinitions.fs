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

open JWithATwist
open JWithATwist.Base 
open FParsec

module ParserDefinitions =

    type Noun = JNoun
    type VerbUnit = unit -> unit
    //type NounUnit = unit -> Noun
    type NounUnit = unit -> JNoun
    //type VerbDyadic = Noun -> Noun -> Noun
    type VerbDyadic = JVerbDyadic
    //type VerbMonadic = Noun -> Noun 
    type VerbMonadic = JVerbMonadic
    //type VerbDyadicUnit = unit -> Noun -> Noun -> Noun
    type VerbDyadicUnit = unit -> JVerbDyadic
    //type VerbMonadicUnit = unit -> Noun -> Noun 
    type VerbMonadicUnit = unit -> JVerbMonadic
    type AdverbMonadicM = Noun -> VerbMonadic-> Noun 
    type AdverbMonadicD = Noun -> VerbDyadic-> Noun 
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
    |TypeAdverbMonadicM of AdverbMonadicM
    |TypeAdverbMonadicD of AdverbMonadicD
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

    let parseStart:Parser<ParseResultList,unit> =
        let (parseResultList:ParseResultList) = []
        (preturn parseResultList)

