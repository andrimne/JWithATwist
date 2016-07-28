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
open Xunit
open Xunit.Abstractions;
open JWithATwist.Base
open JWithATwist.Define

module Define =

    type ``Test JNounDefine:`` () =    
        [<Fact>]
        member x.``Define J Noun`` () =
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let actual = JNounDefine @"{ 1 }"
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``Domain error`` () =
            let f () =
                let actual = JNounDefine @"{! [ + ] }"
                ()
            Assert.Throws<JExceptionDomainError>(f)   
        [<Fact>]
        member x.``Syntax error`` () =
            let f () =
                let actual = JNounDefine @"{! [ + ]}"
                ()
            Assert.Throws<JExceptionSyntaxError>(f)  

    type ``Test JVerbMonadicDefine:`` () =    
        [<Fact>]
        member x.``Define J Monadic Verb`` () =
            let verbMonadic = JVerbMonadicDefine @"{! |- ] } "
            let noun = JNounDefine "{ 1 } "
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            let actual = verbMonadic noun
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Domain error`` () =
            let f () =
                let actual = JVerbMonadicDefine @"{! [ + ] }"
                ()
            Assert.Throws<JExceptionDomainError>(f)   
        [<Fact>]
        member x.``Syntax error`` () =
            let f () =
                let actual = JVerbMonadicDefine @"{! [ + ]}"
                ()
            Assert.Throws<JExceptionSyntaxError>(f)  

    type ``Test JVerbDyadicDefine:`` () =    
        [<Fact>]
        member x.``Define J Dyadic Verb`` () =
            let verbDyadic = JVerbDyadicDefine @"{! [ + ] } "
            let noun = JNounDefine "{ 1 } "
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let actual = verbDyadic noun noun
            Assert.Equal(expected,actual)   
        [<Fact>]
        member x.``Domain error`` () =
            let f () =
                let actual = JVerbDyadicDefine @"{! |- ] }"
                ()
            Assert.Throws<JExceptionDomainError>(f)   
        [<Fact>]
        member x.``Syntax error`` () =
            let f () =
                let actual = JVerbDyadicDefine @"{! [ + ]}"
                ()
            Assert.Throws<JExceptionSyntaxError>(f)                                             

            
                                       