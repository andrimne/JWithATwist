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

open JWithATwist.Base

open JWithATwist.ParserDefinitions
open JWithATwist.ParserInterface
open JWithATwist.Parser

open System
open System.Text
open System.IO
open System.Diagnostics
open FParsec
open Operators.Checked


module Define =

    let JNounDefine (nounDefinition:String) :JNoun =
        let resultOption = parseAllSpeach (nounDefinition+" ")
        match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeNounUnit f} ->
                        f ()
                    |_ ->
                        raise JExceptionDomainError
                |_ ->
                    raise JExceptionSystemError
            |_ -> 
                raise JExceptionSyntaxError


    let JVerbMonadicDefine (verbDefinition:String) :JVerbMonadic =
        let resultOption = parseAllSpeach (verbDefinition+" ")
        match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeVerbMonadic f} ->
                        f
                    |_ ->
                        raise JExceptionDomainError
                |_ ->
                    raise JExceptionSystemError
            |_ -> 
                raise JExceptionSyntaxError

    let JVerbDyadicDefine (verbDefinition:String) :JVerbDyadic =
        let resultOption = parseAllSpeach (verbDefinition+" ")
        match resultOption with
            | Success(result, _, rest)   -> 
                match result with
                |[parseResult] ->
                    match parseResult with
                    |{Value=TypeVerbDyadic f} ->
                        f
                    |_ ->
                        raise JExceptionDomainError
                |_ ->
                    raise JExceptionSystemError
            |_ -> 
                raise JExceptionSyntaxError