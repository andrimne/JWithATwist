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

module Base =



    type ``Test GenIota:`` () =
        [<Fact>]
        member x.``No values in/empty vector in`` () =
            Assert.Equal<JTypeInt []>((Seq.toArray(GenIota [||])), [|0|])
        [<Fact>]
        member x.``One value in, 0`` () =
            Assert.Equal<JTypeInt []>((Seq.toArray(GenIota [|0|])),[||])
        [<Fact>]
        member x.``One positive value in.`` () =
            Assert.Equal<JTypeInt []>((Seq.toArray(GenIota [|3|])),[|0;1;2|])
        [<Fact>]
        member x.``One negative value in`` () =
            Assert.Equal<JTypeInt []>((Seq.toArray(GenIota [|-3|])), [|2;1;0|])
        [<Fact>]
        member x.``Two values in and the second value in is 0`` () =
            Assert.Equal<JTypeInt []>((Seq.toArray(GenIota [|3;0|])),[||])
        [<Fact>]
        member x.``Two values in and the second value is positive`` () =
            Assert.Equal<JTypeInt []>((Seq.toArray(GenIota  [|-3;3|])),[|6;7;8;3;4;5;0;1;2|])
        [<Fact>]
        member x.``Two values in and the second value is negative`` () =
            Assert.Equal<JTypeInt []>((Seq.toArray(GenIota  [|3;-3|])),[|2;1;0;5;4;3;8;7;6|])
        [<Fact>]
        member x.``Three values in`` () =
            Assert.Equal<JTypeInt []>((Seq.toArray(GenIota  [|-3;-3;-3|])),[|26; 25; 24; 23; 22; 21; 20; 19; 18; 17; 16; 15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0|])
            
 

type ``Test JScalarFunctionDyadic:`` () =
        [<Fact>]
        member x.``x and y are empty vectors`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let result = JAdd x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``x and y are scalars`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let result = JAdd x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``x is scalar, y array`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let result = JAdd x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``x is array, y scalar`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let result = JAdd x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``x and y are arrays of same size`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;1L|] ;}
            let result = JAdd x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;3L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``The shape of x is the same as the shape of the first part of y - Agreement`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L;5L;6L|] ;}
            let result = JAdd x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue =  JTypeIntegerArray [|2L;3L;4L;6L;7L;8L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``The shape of the first part of x is the same as the shape of y - Agreement`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L;5L;6L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let result = JAdd x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue =  JTypeIntegerArray [|2L;3L;4L;6L;7L;8L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``x and y are arrays of different dimensions`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;3L;1L|] ;}
                let result = JAdd x y
                ()
            Assert.Throws<JExceptionLengthError>(f)
        [<Fact>]
        member x.``x and y are both float`` () =
            let x = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|1.0;2.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.0;1.0|] ;}
            let result = JAdd x y
            let expected = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|3.0;3.0|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``x and y are both boolean`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|4|]; JValue = JTypeBooleanArray [|true;true;false;false|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|4|]; JValue = JTypeBooleanArray [|true;false;true;false|] ;}
            let result = JAnd x y
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|4|]; JValue = JTypeBooleanArray [|true;false;false;false|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``x and y are both unicode, result is boolean`` () =
            let x = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'a';'a'|] ;}
            let result = JEqual x y
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            Assert.Equal(result,expected)   
        [<Fact>]
        member x.``x and y are both float, result is boolean`` () =
            let x = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|1.0;2.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|1.0;1.0|] ;}
            let result = JEqual x y
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            Assert.Equal(result,expected) 
        [<Fact>]
        member x.``x and y are both integer, result is boolean`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
            let result = JEqual x y
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            Assert.Equal(result,expected)  
        [<Fact>]
        member x.``x and y are both boolean, result is boolean`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;true|] ;}
            let result = JEqual x y
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            Assert.Equal(result,expected)   
        [<Fact>]
        member x.``x and y are both boxed, result is boolean`` () =
            let box1 = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeBooleanArray [|false;true;true;true|] ;}
            let box2 = {JType = JTBType.JTypeFloat; JShape= [|2;2|]; JValue = JTypeFloatArray [|1.0;2.0;3.0;4.0|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|box1;box2|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|box1;box1|] ;}
            let result = JEqual x y
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            Assert.Equal(result,expected)     
        [<Fact>]
        member x.``Overflow should give Value Error`` () =
            //9223372036854775807 + 1
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|9223372036854775807L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
                let actual = JAdd x y
                ()
            Assert.Throws<JExceptionValueError>(f)  

                  

    type ``Test JScalarFunctionMonadic:`` () =
        [<Fact>]
        member x.``Signum empty vector is empty vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let result = JSignum x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Signum scalar -2 is scalar -1`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-2L|] ;}
            let result = JSignum x
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Signum 2 2 Shape Iota 4 is 2 2 Shape 0 1 1 1`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let result = JSignum x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;1L;1L|] ;}
            Assert.Equal(result,expected)
               

    type ``Test JIotaMonadic:`` () =
        [<Fact>]
        member x.``Normal execution`` () =
            let result = JIotaMonadic {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|3L;-3L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;3;3|]; JValue = JTypeIntegerArray [|6L; 7L; 8L; 3L; 4L; 5L; 0L; 1L; 2L; 15L; 16L; 17L; 12L; 13L; 14L; 9L; 10L; 11L; 24L; 25L; 26L; 21L; 22L; 23L; 18L; 19L; 20L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Domain error`` () =  
            let f () =
                let result = JIotaMonadic {JType = JTBType.JTypeFloat; JShape= [|1|]; JValue = JTypeFloatArray [|3.0|] ;}    
                ()
            Assert.Throws<JExceptionDomainError>(f)

    
    type ``Test ArrayTake:`` () =
        [<Fact>]
        member x.``Take 0 from 0 length vector`` () =
            Assert.Equal<int[]>((ArrayTake 0 0 [||]),[||])
        [<Fact>]
        member x.``Take 3 from 0 length vector`` () =
            Assert.Equal<int[]>((ArrayTake 3 0 [||]),[|0;0;0|])
        [<Fact>]
        member x.``Take 0 from 3 long vector`` () =
            Assert.Equal<int[]>((ArrayTake 0 0 [|1;2;3|]),[||])
        [<Fact>]
        member x.``Take 2 from a 3 long vector`` () =
            Assert.Equal<int[]>((ArrayTake 2 0 [|1;2;3|]),[|1;2|])
        [<Fact>]
        member x.``Take 4 from a 3 long vector`` () =
            Assert.Equal<int[]>((ArrayTake 4 0 [|1;2;3|]),[|1;2;3;0|])
        [<Fact>]
        member x.``Take 2 from the tail of a 3 long vector`` () =
            Assert.Equal<int[]>((ArrayTake -2 0 [|1;2;3|]),[|2;3|])
        [<Fact>]
        member x.``Take 4 from the tail of a 3 long vector`` () =
            Assert.Equal<int[]>((ArrayTake -4 0 [|1;2;3|]),[|0;1;2;3|])
        [<Fact>]
        member x.``Take 4 from the tail of a 3 long vector of float`` () =
            Assert.Equal<double[]>((ArrayTake -4 0.0 [|1.0;2.0;3.0|]),[|0.0;1.0;2.0;3.0|])
        [<Fact>]
        member x.``Take 2 from the tail of a 1 long vector of strings`` () =
            Assert.Equal<string[]>((ArrayTake -2 "" [|"whatever"|]),[|"";"whatever"|])


    
  

type ``Test JTake:`` () =
        [<Fact>]
        member x.``Take, x = scalar, y = scalar`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|7L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|7L;0L;0L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Take, x = scalar, y = empty vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|0L;0L;0L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Take, x = scalar, y = non-empty vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|7L;8L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|7L;8L;0L|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Take, x = scalar, y = array with more than one dimensions`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Take, x = empty vector, y = empty vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            Assert.Equal(result ,expected)   
        [<Fact>]
        member x.``Take x = empty vector, y = scalar`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|7L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|7L|] ;}
            Assert.Equal(result ,expected)  
        [<Fact>]
        member x.``Take, x = empty vector, Y = non-empty vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|7L;8L|] ;}
            let result = JTake  x y
            let expected = y
            Assert.Equal(result ,expected) 
        [<Fact>]               
        member x.``Take, x = empty vector, y = array with more than 1 dimensions`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L|] ;}
            let result = JTake  x y
            let expected = y
            Assert.Equal(result ,expected)  
        [<Fact>]
        member x.``Take, x = one long vector, y = empty vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|0L;0L;0L|] ;}
            Assert.Equal(result ,expected)  
        [<Fact>]             
        member x.``Take, x = one long vector, y = scalar`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|7L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|7L;0L;0L|] ;}
            Assert.Equal(result ,expected)  
        [<Fact>]
        member x.``Take, x = one long vector, y = non-empty vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|7L;8L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|7L;8L;0L|] ;}
            Assert.Equal(result ,expected)   
        [<Fact>]             
        member x.``Take, x = one long vector, y = array with more than 1 dimensions`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L|] ;}
            Assert.Equal(result ,expected)  
        [<Fact>]             
        member x.``Take, x = two long vector, y = scalar`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|7L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|7L;0L;0L;0L|] ;}
            Assert.Equal(result ,expected)   
        [<Fact>]             
        member x.``Take, x = two long vector, y = array with 2 dimensions`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;3|]; JValue = JTypeIntegerArray [|1L;2L;0L;3L;4L;0L;0L;0L;0L|] ;}
            Assert.Equal(result ,expected)  
        [<Fact>]             
        member x.``Take, x = two long vector, y = array with 3 dimensions`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;3;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;0L;0L;4L;5L;6L;7L;0L;0L;0L;0L;0L;0L;0L;0L|] ;}
            Assert.Equal(result ,expected)   
        [<Fact>]             
        member x.``Take, x = two long vector, y = array with 4 dimensions`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L;10L;11L;12L;13L;14L;15L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;3;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;0L;0L;0L;0L;8L;9L;10L;11L;12L;13L;14L;15L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L;0L|] ;}
            Assert.Equal(result ,expected)   
        [<Fact>]             
        member x.``x has more elements than the rank of y.`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;2L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|3;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L|] ;}
                let result = JTake  x y
                ()
            Assert.Throws<JExceptionRankError>(f)
        [<Fact>]             
        member x.``x and y are not both integer.`` () =
            let f () =
                let x = {JType = JTBType.JTypeFloat; JShape= [|3|]; JValue = JTypeFloatArray [|2.0;2.0;2.0|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|3;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L|] ;}
                let result = JTake  x y
                ()
            Assert.Throws<JExceptionDomainError>(f)                
        [<Fact>]             
        member x.``Take the last element in the first two dimensions of a three-dimensional array`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|-1L;-1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1;1;2|]; JValue = JTypeIntegerArray [|6L;7L|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]             
        member x.``Take with boolean`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;3L|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2;2|]; JValue = JTypeBooleanArray [|false;true;true;true|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3;3|]; JValue = JTypeBooleanArray [|false;true;false;true;true;false;false;false;false|] ;}
            Assert.Equal(result ,expected)  
        [<Fact>]             
        member x.``Take with float`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;3L|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|2;2|]; JValue = JTypeFloatArray [|1.0;2.0;3.0;4.0|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeFloat; JShape= [|3;3|]; JValue = JTypeFloatArray [|1.0;2.0;0.0;3.0;4.0;0.0;0.0;0.0;0.0|] ;}
            Assert.Equal(result ,expected)  
        [<Fact>]             
        member x.``Take with unicode`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;3L|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|2;2|]; JValue = JTypeUnicodeArray [|'a';'b';'c';'d'|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|3;3|]; JValue = JTypeUnicodeArray [|'a';'b';' ';'c';'d';' ';' ';' ';' '|] ;}
            Assert.Equal(result ,expected)  
        [<Fact>]             
        member x.``Take with boxed`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;3L|] ;}
            let box1 = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeBooleanArray [|false;true;true;true|] ;}
            let box2 = {JType = JTBType.JTypeFloat; JShape= [|2;2|]; JValue = JTypeFloatArray [|1.0;2.0;3.0;4.0|] ;}
            let box3 = {JType = JTBType.JTypeUnicode; JShape= [|2;2|]; JValue = JTypeUnicodeArray [|'a';'b';'c';'d'|] ;}
            let box4 = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|2;2|]; JValue = JTypeBoxedArray [|box1;box2;box3;box4|] ;}
            let result = JTake  x y
            let expected = {JType = JTBType.JTypeBoxed; JShape= [|3;3|]; JValue = JTypeBoxedArray [|box1;box2;ZeroNounConstant;box3;box4;ZeroNounConstant;ZeroNounConstant;ZeroNounConstant;ZeroNounConstant|] ;}
            Assert.Equal(result ,expected)  

type ``Test JDrop:`` () =
        [<Fact>]
        member x.``Drop 1 from a vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
            let result = JDrop  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Drop 1 from a Shape 2 2 array`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let result = JDrop  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1;2|]; JValue = JTypeIntegerArray [|3L;4L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Drop 1 1 from a Shape 2 2 array`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let result = JDrop  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1;1|]; JValue = JTypeIntegerArray [|4L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Drop 1 1 from a Shape 2 2 2 array`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L;5L;6L;7L;8L|] ;}
            let result = JDrop  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1;1;2|]; JValue = JTypeIntegerArray [|7L;8L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Drop -1 from a vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
            let result = JDrop  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Drop -1 -1 from a Shape 2 2 array`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|-1L;-1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let result = JDrop  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1;1|]; JValue = JTypeIntegerArray [|1L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Drop -1 -1 from a Shape 2 2 2 array`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|-1L;-1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L;5L;6L;7L;8L|] ;}
            let result = JDrop  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1;1;2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Drop 1 from a 1 long vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|1L|] ;}
            let result = JDrop  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Drop 1 from a 0 long vector`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let result = JDrop  x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            Assert.Equal(result,expected)
     

type ``Test RankMonadic:`` () =
        [<Fact>]
        member x.``Iota  (Iota 0)`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Iota  0`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Iota  3`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|0L;1L;2L|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Iota  0 1`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0;1|]; JValue = JTypeIntegerArray [||] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Iota  0 2`` () = 
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;2L|] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0;2|]; JValue = JTypeIntegerArray [||] ;}  
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Iota  2 1 $ 1 2`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;1|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;0L;0L;1L|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Iota  2 2 $ 1 2 2 3`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;2L;3L|] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2;3|]; JValue = JTypeIntegerArray [|0L;1L;0L;0L;0L;0L;0L;1L;2L;3L;4L;5L|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Iota  2 2 $ 0 1 2 0`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;0L|] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2;1|]; JValue = JTypeIntegerArray [|0L;0L;0L;0L|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Iota  2 2 1 $ 2 1 1 2`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;1|]; JValue = JTypeIntegerArray [|2L;1L;1L;2L|] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;0L;0L;0L;0L;0L;1L|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Domain error when x is not integer`` () =  
            let f () =
                let x = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|0.0;2.0|] ;}
                let result = RankMonadic JIotaMonadic 1 x 
                ()                                                                                    
            Assert.Throws<JExceptionDomainError>(f)              
        [<Fact>]
        member x.``Test rank 0 verb: Signum -2 should give -1`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-2L|] ;}
            let result = RankMonadic JSignum 0 x 
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            Assert.Equal(result ,expected)   
        [<Fact>]
        member x.``Test rank 0 verb: Signum 2 2 Shape -2 0 -3 4 should give 2 2 Shape -1 0 -1 1 `` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|-2L;0L;-3L;4L|] ;}
            let result = RankMonadic JSignum 0 x 
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|-1L;0L;-1L;1L|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``(2 = ]) Rank 0 [1 2 3 should give false true false `` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let result = RankMonadic (JEqual y) 0 x 
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|false;true;false|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Iota  2 2 $ 2 1 1 3 should give 2 2 3 Shape 0 0 0 1 0 0 0 1 2 0 0 0`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|2L;1L;1L;3L|] ;}
            let result = RankMonadic JIotaMonadic 1 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2;3|]; JValue = JTypeIntegerArray [|0L;0L;0L;1L;0L;0L;0L;1L;2L;0L;0L;0L|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Reciprocal 2_0 should give 0_5 `` () =  
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.0|] ;}
            let result = RankMonadic JReciprocal 0 x
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|0.5|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Ravel 2 1 Shape "ab" should give "ab" `` () =  
            let x = {JType = JTBType.JTypeUnicode; JShape= [|2;1|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}
            let result = RankMonadic JRavel 2 x
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Ravel 2 1 Shape true false should give true false `` () =  
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2;1|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let result = RankMonadic JRavel 2 x
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Ravel 2 1 Shape ( Box "a" ) , Box "b" should give  ( Box "a" ) , Box "b" `` () =  
            let boxa = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'a'|] ;}
            let boxb = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'b'|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2;1|]; JValue = JTypeBoxedArray [|boxa;boxb|] ;}
            let result = RankMonadic JRavel 2 x
            let expected = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|boxa;boxb|] ;}             
            Assert.Equal(result ,expected) 
        [<Fact>]
        member x.``Signum -2_0 should give -1 `` () =  
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-2.0|] ;}
            let result = RankMonadic JSignum 2 x
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}             
            Assert.Equal(result ,expected) 

    type ``Test JBox:`` () =
        [<Fact>]
        member x.``Box 1 2 3 should give a scalar boxed array with one cell containing 1 2 3`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
            let result = JBox x
            let expected = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|x|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Box should box float arrays`` () =  
            let x = {JType = JTBType.JTypeFloat; JShape= [|3|]; JValue = JTypeFloatArray [|1.0;2.0;3.0|] ;}
            let result = JBox x
            let expected = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|x|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Box should box unicode arrays`` () =  
            let x = {JType = JTBType.JTypeUnicode; JShape= [|3|]; JValue = JTypeUnicodeArray [|'a';'b';'c'|] ;}
            let result = JBox x
            let expected = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|x|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Box should box boolean arrays`` () =  
            let x = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|true;false;true|] ;}
            let result = JBox x
            let expected = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|x|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Box should box boxed arrays`` () =  
            let boxedJGA = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxedJGA|] ;}
            let result = JBox x
            let expected = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|x|] ;}
            Assert.Equal(result ,expected)


    type ``Test JRankMonadic:`` () =
        [<Fact>]
        member x.``Box(rank 0) 2 2 2 Shape Iota 8 should give a boxed array of shape 2 2 2 `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic x JBox n
            let matching =
                match result with
                |{JType = JTBType.JTypeBoxed; JShape= [|2;2;2|]; JValue = _ ;} ->
                    true
                |_ ->
                    false
            Assert.True(matching)
        [<Fact>]
        member x.``Box(rank 0) 2 2 2 Shape Iota 8 should give a boxed array where the second value is 1 `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic x JBox  n
            let matching =
                match result with
                |{JType = JTBType.JTypeBoxed; JShape= [|2;2;2|]; JValue = JTypeBoxedArray rValue ;} ->
                    match rValue with
                    |r when (unbox (r.GetValue 1)) = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;} ->
                        true
                    |_ -> 
                        false
                |_ ->
                    false
            Assert.True(matching)
        [<Fact>]
        member x.``Box(rank 1) 2 2 2 Shape Iota 8 should give a boxed array of shape 2 2 `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic x JBox  n
            let matching =
                match result with
                |{JType = JTBType.JTypeBoxed; JShape= [|2;2|]; JValue = _ ;} ->
                    true
                |_ ->
                    false
            Assert.True(matching)
        [<Fact>]
        member x.``Box(rank 1) 2 2 2 Shape Iota 8 should give a boxed array where the second value is 2 3 `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic x  JBox n
            let matching =
                match result with
                |{JType = JTBType.JTypeBoxed; JShape= [|2;2|]; JValue = JTypeBoxedArray rValue ;} ->
                    match rValue with
                    |r when (unbox (r.GetValue 1)) = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;} ->
                        true
                    |_ -> 
                        false
                |_ ->
                    false
            Assert.True(matching)
        [<Fact>]
        member x.``Box(rank 2) 2 2 2 Shape Iota 8 should give a boxed array of shape 2 `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic x JBox  n
            let matching =
                match result with
                |{JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = _ ;} ->
                    true
                |_ ->
                    false
            Assert.True(matching)
        [<Fact>]
        member x.``Box(rank 2) 2 2 2 Shape Iota 8 should give a boxed array where the second value is 2 2 Shape 4 5 6 7 `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic x JBox  n
            let matching =
                match result with
                |{JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray rValue ;} ->
                    match rValue with
                    |r when (unbox (r.GetValue 1)) = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|4L;5L;6L;7L|] ;} ->
                        true
                    |_ -> 
                        false
                |_ ->
                    false
            Assert.True(matching)
        [<Fact>]
        member x.``Box(rank 3) 2 2 2 Shape Iota 8 should return the argument boxed. `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic  x JBox n
            let matching =
                match result with
                |{JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray rArray ;} ->
                    let xInResult = unbox (rArray.GetValue 0)
                    x=xInResult
                |_ ->
                    false
            Assert.True(matching)
        [<Fact>]
        member x.``Box(rank -1) 2 2 2 Shape Iota 8 should be equal to Box(rank 2) 2 2 2 Shape Iota 8  `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic x  JBox n
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let expected = JRankMonadic x JBox  n
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Box(rank -2) 2 2 2 Shape Iota 8 should be equal to Box(rank 1) 2 2 2 Shape Iota 8  `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-2L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic x  JBox n
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let expected = JRankMonadic x  JBox n
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Box(rank -3) 2 2 2 Shape Iota 8 should be equal to Box(rank 0) 2 2 2 Shape Iota 8  `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-3L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic x JBox  n
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let expected = JRankMonadic x JBox  n
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Box(rank -4) 2 2 2 Shape Iota 8 should be equal to Box(rank 0) 2 2 2 Shape Iota 8  `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-4L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic  x JBox n
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let expected = JRankMonadic x  JBox n
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Box(rank 4) 2 2 2 Shape Iota 8 should be equal to Box(rank 3) 2 2 2 Shape Iota 8  `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|4L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let result = JRankMonadic  x JBox  n
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let expected = JRankMonadic  x JBox n
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Reciprocal(rank 0) 1 2 3 should be equal to Reciprocal(rank 1) 1 2 3  `` () =  
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
            let result = JRankMonadic x JReciprocal n
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
            let expected = JRankMonadic x JReciprocal  n
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Iota(rank 0) 0 0 0 Shape Iota 0 should give 0 0 0 0 Shape Iota 0  `` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|0;0;0|]; JValue = JTypeIntegerArray [||] ;}
            let n = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let result = JRankMonadic x  JIotaMonadic n
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0;0;0;0|]; JValue = JTypeIntegerArray [||] ;}
            Assert.Equal(expected,result)

            
          

    type ``Test JOpen:`` () =
        [<Fact>]
        member x.``Open Iota 0 should give Iota 0`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let result = JOpen x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Open 2 should give 2`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let result = JOpen x
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Open 0 2 should give 0 2`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;2L|] ;}
            let result = JOpen x
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;2L|] ;}
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Open Box Iota 0 should give Iota 0`` () =  
            let boxed = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxed|] ;}
            let result = JOpen x
            let expected = boxed
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Open Box 2 should give 2`` () =  
            let boxed = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxed|] ;}
            let result = JOpen x
            let expected = boxed
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Open Box 0 2 should give 0 2`` () =  
            let boxed = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;2L|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxed|] ;}
            let result = JOpen x
            let expected = boxed
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Open Box 'we' should give 'we' `` () =  
            let boxed = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'w';'e'|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxed|] ;}
            let result = JOpen x
            let expected = boxed
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Open Box 2_3 2_4 should give 2_3 2_4`` () =  
            let boxed = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.3;2.4|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxed|] ;}
            let result = JOpen x
            let expected = boxed
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Boolean: Open Box 0 1 should give 0 1`` () =  
            let boxed = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|false;true|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxed|] ;}
            let result = JOpen x
            let expected = boxed
            Assert.Equal(result ,expected)
        [<Fact>]
        member x.``Open (Box 0 2),Box 'we' should give Domain Error`` () =  
            let f () =
                let boxed1 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;2L|] ;}
                let boxed2 = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'w';'e'|] ;}
                let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|boxed1;boxed2|] ;}
                let result = JOpen x
                ()
            Assert.Throws<JExceptionDomainError>(f)  
        [<Fact>]
        member x.``Boolean/Float: Open (Box false true),Box 2.3 2.4 should give Domain Error`` () =  
            let f () =
                let boxed1 = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|false;true|] ;}
                let boxed2 = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.3;2.4|] ;}
                let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|boxed1;boxed2|] ;}
                let result = JOpen x
                ()
            Assert.Throws<JExceptionDomainError>(f) 
        [<Fact>]
        member x.``Integer/Float to Float: Open (Box 0 2),Box 2.3 2.4 should give 2 2 $ 0 2 2.3 2.4`` () =  
            let boxed1 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;2L|] ;}
            let boxed2 = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.3;2.4|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|boxed1;boxed2|] ;}
            let result = JOpen x
            let expected = {JType = JTBType.JTypeFloat; JShape= [|2;2|]; JValue = JTypeFloatArray [|0.0;2.0;2.3;2.4|] ;}
            let equal = (result = expected)
            Assert.True(equal)
        [<Fact>]
        member x.``Open boxes containing three arrays of different rank`` () =  
            let box1 = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let box2 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let box3 = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let boxed = {JType = JTBType.JTypeBoxed; JShape= [|3|]; JValue = JTypeBoxedArray [|box1;box2;box3|] ;}
            let result = JOpen boxed
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;2;2|]; JValue = JTypeIntegerArray [|1L;0L;0L;0L;0L;1L;0L;0L;0L;1L;2L;3L|] ;}
            Assert.Equal(result,expected)


    type ``Test RankMonadicBox RankMonadicOpen:`` () =
        [<Fact>]
        member x.``Rank 0 JBox 1 2 should give two boxes with one box each containing 1 and 2 `` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let int1 = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let int2 = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let box1 = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|int1|] ;}
            let box2 = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|int2|] ;}
            let boxed = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|box1;box2|] ;}
            let result = RankMonadicBox JBox 0 x
            let expected = boxed
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Open two boxes with one box each of containing 1 and 2 should give two boxes containing 1 and 2 `` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let int1 = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let int2 = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let box1 = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|int1|] ;}
            let box2 = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|int2|] ;}
            let boxed = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|box1;box2|] ;}
            let result = RankMonadicOpen  boxed
            let expected = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|int1;int2|] ;}
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Rank 0 Iota 2 3 should give two boxes containing 0 1 and 0 1 2 `` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let box1 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let box2 = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|0L;1L;2L|] ;}
            let boxed = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|box1;box2|] ;}
            let result = RankMonadicBox JIotaMonadic 0 x
            let expected = boxed
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Open two boxes containing 0 1 and 0 1 2 should give Rank 0 Iota 2 3`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let box1 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let box2 = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|0L;1L;2L|] ;}
            let boxed = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|box1;box2|] ;}
            let result = RankMonadicOpen boxed
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|0L;1L;0L;0L;1L;2L|] ;}
            Assert.Equal(result,expected)



    type ``Test RankMonadicEmptyArraySpecialHandling:`` () =
        [<Fact>]
        member x.``Box(Rank 0) Iota 0 should give an empty boxed array`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let boxed = {JType = JTBType.JTypeBoxed; JShape= [|0|]; JValue = JTypeBoxedArray [||] ;}
            let result = JRankMonadic x JBox ZeroNounConstant
            let expected = boxed
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``Box(Rank 0) 0 0$ 0 should give a shape 0 0 boxed array`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|0;0|]; JValue = JTypeIntegerArray [||] ;}
            let boxed = {JType = JTBType.JTypeBoxed; JShape= [|0;0|]; JValue = JTypeBoxedArray [||] ;}
            let result = JRankMonadic x JBox ZeroNounConstant
            let expected = boxed
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``(2 2 $ ] ) (Rank 0) Iota 0 should give a shape 0 2 2 integer array`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0;2;2|]; JValue = JTypeIntegerArray [||] ;}
            let twoTwo = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let f y = JShape twoTwo y
            let result = JRankMonadic x f ZeroNounConstant
            Assert.Equal(result,expected)
        [<Fact>]
        member x.``(2 2 $ ] ) (Rank 1) 0 2 $ 0 should give a shape 0 2 2 integer array`` () =  
            let x = {JType = JTBType.JTypeInteger; JShape= [|0;2|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0;2;2|]; JValue = JTypeIntegerArray [||] ;}
            let twoTwo = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let f y = JShape twoTwo y
            let result = JRankMonadic x f OneNounConstant
            Assert.Equal(result,expected)



    type ``Test JRankDyadic:`` () =
        [<Fact>]
        member x.``Simple application`` () =
            //1 Plus(Rank 0 0) 1 should give 2  
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;0L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let actual = JRankDyadic x y JAdd  n
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Each in left argument against the whole of right argument`` () = 
            //1 2 Plus(Rank 0 1) 2 3 should give 2 2 Shape 3 4 4 5 
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|3L;4L;4L;5L|] ;}
            let actual = JRankDyadic x y JAdd   n
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Each in right argument against the whole of left argument`` () = 
            //1 2 Plus(Rank 1 0) 2 3 should give 2 2 Shape 3 4 4 5 
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|3L;4L;4L;5L|] ;}
            let actual = JRankDyadic x y JAdd  n
            Assert.Equal(expected,actual)    

        [<Fact>]
        member x.``Empty arrays on both sides`` () = 
            //(Iota 0) Plus(Rank 1 1) Iota 0 should give Iota 0  
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JRankDyadic x y JAdd  n 
            Assert.Equal(expected,actual)             
        [<Fact>]
        member x.``Empty arrays on both sides, both ranks zero and an operation which should give a shape and type`` () = 
            //(Iota 0) (2 1 Shape 1 Take =)(Rank 0 0) Iota 0 should give empty array of shape 0 2 1  
            //This is a tricky case. With a rank 0 operation you need a scalar to operate on to get
            //rank and type of the result. However the arrays are empty, so you have none.
            //The J interpreter creates a replacement default element and operates on this element
            //to get type and shape.
            //However, if there is an error when you execute this operation, which the user never
            //asked for, the J interpreter disregards the error and returns empty from this operation
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;0L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|0;2;1|]; JValue = JTypeBooleanArray [||] ;}
            let x2 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;1L|] ;}
            let twoOneShape = JShape x2
            let x3 = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let takeOne = JTake x3
            let operation x y = twoOneShape (takeOne (JEqual x y) )
            let actual = JRankDyadic x y operation n
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Empty arrays on both sides, one rank zero and an operation which should give a shape and type`` () = 
            //(Iota 0) (2 1 Shape 1 Take =)(Rank 1 0) Iota 0 should give empty array of shape 0 2 1  
            //This is a tricky case. With a rank 0 operation you need a scalar to operate on to get
            //rank and type of the result. However the arrays are empty, so you have none.
            //The J interpreter creates a replacement default element and operates on this element
            //to get type and shape.
            //However, if there is an error when you execute this operation, which the user never
            //asked for, the J interpreter disregards the error and returns empty from this operation
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|0;2;1|]; JValue = JTypeBooleanArray [||] ;}
            let x2 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;1L|] ;}
            let twoOneShape = JShape x2
            let x3 = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let takeOne = JTake x3
            let operation x y = twoOneShape (takeOne (JEqual x y) )
            let actual = JRankDyadic x y operation  n
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Empty arrays on both sides, one rank zero and an operation which if applied would give length error`` () = 
            //(Iota 0) (2 1 Shape =)(Rank 1 0) Iota 0 should give Iota 0 
            //This is a tricky case. With a rank 0 operation you need a scalar to operate on to get
            //rank and type of the result. However the arrays are empty, so you have none.
            //The J interpreter creates a replacement default element and operates on this element
            //to get type and shape.
            //However, if there is an error when you execute this operation, which the user never
            //asked for, the J interpreter disregards the error and returns empty from this operation
            //My present standpoint is that you can do this operation, but you have to be open to users about it.
            //If there is an error, it should not be hidden.
            let f () =
                let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
                let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
                let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
                let x2 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;1L|] ;}
                let twoOneShape = JShape x2
                let operation x y = twoOneShape (JEqual x y) 
                let actual = JRankDyadic x y operation  n
                ()
            Assert.Throws<JExceptionLengthError>(f)  
        [<Fact>]
        member x.``(2 2 Shape 1) + \"/ 1 1 / 4 2 Shape 1 should give length error `` () = 
            let f () =
                let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
                let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;1L;1L;1L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|4;2|]; JValue = JTypeIntegerArray [|1L;1L;1L;1L;1L;1L;1L;1L|] ;}
                let actual = JRankDyadic x y JAdd  n
                ()
            Assert.Throws<JExceptionLengthError>(f) 
        [<Fact>]
        member x.``Float arguments.`` () =  
            //1.0 2.0 Plus(Rank 0 1) 2.0 3.0 should give 2 2 Shape 3.0 4.0 4.0 5.0 
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let x = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|1.0;2.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.0;3.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [|2;2|]; JValue = JTypeFloatArray [|3.0;4.0;4.0;5.0|] ;}
            let actual = JRankDyadic x y  JAdd  n
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Boolean arguments`` () =  
            //true false And(Rank 1 0) false true should give 2 2 Shape false false true false
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|false;true|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2;2|]; JValue = JTypeBooleanArray [|false;false;true;false|] ;}
            let actual = JRankDyadic x y JAnd   n
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Character arguments`` () =  
            //'ab' = (Rank 1 0) 'bc' should give 2 2 Shape false true false false
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let x = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'b';'c'|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2;2|]; JValue = JTypeBooleanArray [|false;true;false;false|] ;}
            let actual = JRankDyadic x y  JEqual  n
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Boxed arguments`` () =  
            //(Box 1 2) ((Cap Open x) Add Cap Open y)(Rank 1 1) 2 3 should give 3 5
            let n = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
            let boxx = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let boxy = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxx|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxy|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;5L|] ;}
            let operation x y = JAdd (JOpen x) (JOpen y)
            let actual = JRankDyadic x y  operation  n
            Assert.Equal(expected,actual)   
        [<Fact>]
        member x.``Two results in which the first has the largest first dimension, the second the largest second dimension`` () = 
            //(2 2 $ 2 1 1 3) Shape Rank 1 1 Iota 3 should give 2 2 3 Shape 0 0 0 1 0 0 0 1 2 0 0 0 
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|2L;1L;1L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|0L;1L;2L|] ;}
            let result = RankDyadic JShape [|1;1|] x y
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2;3|]; JValue = JTypeIntegerArray [|0L;0L;0L;1L;0L;0L;0L;1L;2L;0L;0L;0L|] ;}             
            Assert.Equal(result ,expected) 

    type ``Test JShape:`` () =
        [<Fact>]
        member x.``(Iota 0) Shape y`` () =
            //(Iota 0) Shape 1 2 should give scalar 1
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual)     
        [<Fact>]
        member x.``0 1 Shape Ioto 0 should give 0 1 Shape Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0;1|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``1 Shape Iota 0 should give length error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
                let actual = JShape x y
                ()
            Assert.Throws<JExceptionLengthError>(f) 
        [<Fact>]
        member x.``(Iota 0) Shape Iota 0 should give length error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
                let actual = JShape x y
                ()
            Assert.Throws<JExceptionLengthError>(f) 
        [<Fact>]
        member x.``2 2 Shape 1  should give 2 2 Shape 1 1 1 1`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;1L;1L;1L|] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual)     
        [<Fact>]
        member x.``2 2 Shape 1 2 3 4  should give 2 2 Shape 1 2 3 4`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual)   
        [<Fact>]
        member x.``(2 2 Shape 2) Shape 1  should give 2 2 2 Shape 1 1 1 1 1 1 1 1`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|2L;2L;2L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|1L;1L;1L;1L;1L;1L;1L;1L|] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``2 2 Shape Iota 2 2 should give 2 2 2 Shape 0 1 2 3 0 1 2 3`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;0L;1L;2L;3L|] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual)   
            
        [<Fact>]
        member x.``Dyadic Shape with floats`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeFloatArray [|1.0;2.0;3.0;4.0|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeFloatArray [|1.0;2.0;3.0;4.0|] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual)        
        [<Fact>]
        member x.``Dyadic Shape with booleans`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|4|]; JValue = JTypeBooleanArray [|true;false;false;true|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2;2|]; JValue = JTypeBooleanArray [|true;false;false;true|] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Dyadic Shape with unicode`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|4|]; JValue = JTypeUnicodeArray [|'a';'b';'c';'d'|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2;2|]; JValue = JTypeUnicodeArray [|'a';'b';'c';'d'|] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Dyadic Shape with boxed`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let box1 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let box2 = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeBoxedArray [|box1;box2;box2;box1|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeBoxedArray [|box1;box2;box2;box1|] ;}
            let actual = JShape x y
            Assert.Equal(expected,actual) 

    type ``Test JShapeOF:`` () =                   
        [<Fact>]
        member x.``ShapeOf scalar`` () =
            //ShapeOf 5 should give Iota 0
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JShapeOf y
            Assert.Equal(expected,actual)  
             
        [<Fact>]
        member x.``ShapeOf Iota 0`` () =
            //ShapeOf Iota 0 should give 0
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let actual = JShapeOf y
            Assert.Equal(expected,actual)    
             
        [<Fact>]
        member x.``ShapeOf other nouns`` () =
            //ShapeOf 2 2 Shape 1 2 3 4 should give 2 2
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;2L|] ;}
            let actual = JShapeOf y
            Assert.Equal(expected,actual)                                 
                                  
    type ``Test JSignum:`` () =                   
        [<Fact>]
        member x.``Signum of integer`` () =
            //Signum -5 should give -1
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            let actual = JSignum y
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``Signum of float`` () =
            //Signum -5.0 should give -1
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-5.0|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            let actual = JSignum y
            Assert.Equal(expected,actual)   

    type ``Test JFloor:`` () =                   
        [<Fact>]
        member x.``Floor`` () =
            //Floor -5.5 should give -6
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-5.5|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-6L|] ;}
            let actual = JFloor y
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``Floor should give domain error if the absolute value  of the argument is larger than 2 raised to 51`` () =
            let f () =
                //Floor 
                let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-4503599627370496.0|] ;}
                let actual = JFloor y
                ()
            Assert.Throws<JExceptionDomainError>(f)  

    type ``Test JCeiling:`` () =                   
        [<Fact>]
        member x.``Ceiling`` () =
            //Ceiling -5.5 should give -5
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-5.5|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-5L|] ;}
            let actual = JCeiling y
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``Ceiling should give domain error if the absolute value  of the argument is larger than 2 raised to 51`` () =
            let f () =
                //Floor 
                let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-4503599627370496.0|] ;}
                let actual = JCeiling y
                ()
            Assert.Throws<JExceptionDomainError>(f)                            
            
    type ``Test JNegate:`` () =                   
        [<Fact>]
        member x.``Negate of integer`` () =
            //Signum -5 should give 5
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let actual = JNegate y
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``Negate of float`` () =
            //Signum -5.0 should give 5.0
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-5.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|5.0|] ;}
            let actual = JNegate y
            Assert.Equal(expected,actual)   

    type ``Test JMagnitude:`` () =                   
        [<Fact>]
        member x.``Magnitude of integer`` () =
            //Magnitude -5 should give 5
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let actual = JMagnitude y
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``Magnitude of float`` () =
            //Reciprocal  -5.0 should give 5.0
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-5.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|5.0|] ;}
            let actual = JMagnitude y
            Assert.Equal(expected,actual)                

    type ``Test JReciprocal:`` () =                   
        [<Fact>]
        member x.``Reciprocal of integer`` () =
            //Reciprocal 5 should give 0.2
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|0.2|] ;}
            let actual = JReciprocal y
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``Reciprocal of float`` () =
            //Reciprocal  5.0 should give 0.2
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|5.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|0.2|] ;}
            let actual = JReciprocal y
            Assert.Equal(expected,actual) 

    type ``Test JNot:`` () =                   
        [<Fact>]
        member x.``Not`` () =
            //Not true should give false
            let y = {JType = JTBType.JTypeBoolean; JShape= [||]; JValue = JTypeBooleanArray [|true|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [||]; JValue = JTypeBooleanArray [|false|] ;}
            let actual = JNot y
            Assert.Equal(expected,actual)              

    type ``Test JBti:`` () =                   
        [<Fact>]
        member x.``BooleanToInteger of true`` () =
            //BooleanToInteger of true should give 1
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeBooleanArray [|true|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let actual = JBti y
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``BooleanToInteger of false`` () =
            //BooleanToInteger of true should give 1
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeBooleanArray [|false|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let actual = JBti y
            Assert.Equal(expected,actual)   
            
    type ``Test JAdd:`` () =                   
        [<Fact>]
        member x.``Add integers`` () =
            //1 + 1 = 2
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let actual = JAdd x y
            Assert.Equal(expected,actual)   
        [<Fact>]
        member x.``Add floats`` () =
            //1 + 1 = 2
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|1.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|1.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.0|] ;}
            let actual = JAdd x y
            Assert.Equal(expected,actual)   


    type ``Test JSubtract:`` () =                   
        [<Fact>]
        member x.``Subtract integers`` () =
            //1 - 1 = 0
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let actual = JSubtract x y
            Assert.Equal(expected,actual)   
        [<Fact>]
        member x.``Subtract floats`` () =
            //1 - 1 = 0
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|1.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|1.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|0.0|] ;}
            let actual = JSubtract x y
            Assert.Equal(expected,actual)  
  
      type ``Test JTimes:`` () =                   
        [<Fact>]
        member x.``Multiply integers`` () =
            //5 times 2 should give 10
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|10L|] ;}
            let actual = JTimes x y
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``Multiply floats`` () =
            //5.0 times 2 should give a float, 10
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|5.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|10.0|] ;}
            let actual = JTimes x y
            Assert.Equal(expected,actual)  


    type ``Test JDivide:`` () =                   
        [<Fact>]
        member x.``Divide of integers`` () =
            //5 divided by 2 should give a float, 2.5
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.5|] ;}
            let actual = JDivide x y
            Assert.Equal(expected,actual)    
        [<Fact>]
        member x.``Divide of floats and integer`` () =
            //5.0 divided by 2 should give a float, 2.5
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|5.0|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.5|] ;}
            let actual = JDivide x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Divide of integer and float`` () =
            //5 divided by 2.0 should give a float, 2.5
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.5|] ;}
            let actual = JDivide x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Divide of two floats`` () =
            //5.0 divided by 2.0 should give a float, 2.5
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|5.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.5|] ;}
            let actual = JDivide x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``Division by zero`` () =
            //Division by zero should give infinity
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|5.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|0.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|infinity|] ;}
            let actual = JDivide x y
            Assert.Equal(expected,actual)  

      type ``Test JPower:`` () =                   
        [<Fact>]
        member x.``Power`` () =
            //2 raised to 2 should give 4.0
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|4.0|] ;}
            let actual = JPower x y
            Assert.Equal(expected,actual)    


    type ``Test JMin:`` () =                   
        [<Fact>]
        member x.``Min of integers`` () =
            //-1 min -5 = -5
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-5L|] ;}
            let actual = JMin x y
            Assert.Equal(expected,actual)   
        [<Fact>]
        member x.``Min of floats`` () =
            //-1.1 min -1.2 = -1.2
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-1.1|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-1.2|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-1.2|] ;}
            let actual = JMin x y
            Assert.Equal(expected,actual)  

    type ``Test JMax:`` () =                   
        [<Fact>]
        member x.``Max of integers`` () =
            //-1 max -5 = -1
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            let actual = JMax x y
            Assert.Equal(expected,actual)   
        [<Fact>]
        member x.``Max of floats`` () =
            //-1.1 min -1.2 = -1.1
            let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-1.1|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-1.2|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-1.1|] ;}
            let actual = JMax x y
            Assert.Equal(expected,actual)  

    type ``Test JAnd:`` () =                   
        [<Fact>]
        member x.``And`` () =
            //true true And true false = true false
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;true|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JAnd x y
            Assert.Equal(expected,actual)   

    type ``Test JOr:`` () =                   
        [<Fact>]
        member x.``Or`` () =
            //true false Or false false = true false
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|false;false|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JOr x y
            Assert.Equal(expected,actual) 

    type ``Test JEqual:`` () =                   
        [<Fact>]
        member x.``See Test JScalarFunctionDyadic`` () = 
            Assert.True(true)      
                        
    type ``Test JNotEqual:`` () =       
        [<Fact>]
        member x.``Integer`` () =
            //2 1 NotEqual 1 1 = true false
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JNotEqual x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Float`` () =
            //2.0 1.0 NotEqual 1.0 1.0 = true false
            let x = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.0;1.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|1.0;1.0|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JNotEqual x y
            Assert.Equal(expected,actual)                             
        [<Fact>]
        member x.``Boolean`` () =
            //true false NotEqual false false = true false
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|false;false|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JNotEqual x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Unicode`` () =
            //"ab" NotEqual "bb" = true false
            let x = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'b';'b'|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JNotEqual x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Boxed`` () =
            //((Box "a"), Box b" ) NotEqual (Box "b"),Box "b" = true false
            let boxa = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'a'|] ;}
            let boxb = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'b'|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|boxa;boxb|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|boxb;boxb|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JNotEqual x y
            Assert.Equal(expected,actual) 

    type ``Test JLessThan:`` () =       
        [<Fact>]
        member x.``Integer`` () =
            //1 1 LessThan 2 1 = true false 
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;1L|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JLessThan x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Float`` () =
            //1.0 1.0 NotEqual 2.0 1.0 = true false
            let x = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|1.0;1.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.0;1.0|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JLessThan x y
            Assert.Equal(expected,actual)                             
        [<Fact>]
        member x.``Boolean`` () =
            //false false LessThan true false = true false
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|false;false|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JLessThan x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Unicode`` () =
            //"ab" LessThan "bb" = true false
            let x = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'b';'b'|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JLessThan x y
            Assert.Equal(expected,actual) 

    type ``Test JLargerThan:`` () =       
        [<Fact>]
        member x.``Integer`` () =
            //2 1 LargerThan 1 1 = true false 
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JLargerThan x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Float`` () =
            //2.0 1.0 LargerThan 1.0 1.0 = true false
            let x = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.0;1.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|1.0;1.0|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JLargerThan x y
            Assert.Equal(expected,actual)                             
        [<Fact>]
        member x.``Boolean`` () =
            //true false LargerThan false false = true false
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|false;false|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JLargerThan x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Unicode`` () =
            //"bb" LargerThan "ab" = true false
            let x = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'b';'b'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JLargerThan x y
            Assert.Equal(expected,actual) 

    type ``Test JLessOrEqual:`` () =       
        [<Fact>]
        member x.``Integer`` () =
            //1 1 2 LessOrEqual 2 1 1 = true true false 
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;1L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;1L;1L|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|true;true;false|] ;}
            let actual = JLessOrEqual x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Float`` () =
            //1.0 1.0 2.0 LessOrEqual 2.0 1.0 1.0 = true true false
            let x = {JType = JTBType.JTypeFloat; JShape= [|3|]; JValue = JTypeFloatArray [|1.0;1.0;2.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|3|]; JValue = JTypeFloatArray [|2.0;1.0;1.0|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|true;true;false|] ;}
            let actual = JLessOrEqual x y
            Assert.Equal(expected,actual)                             
        [<Fact>]
        member x.``Boolean`` () =
            //false false true LessOrEqual true false false = true true false
            let x = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|false;false;true|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|true;false;false|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|true;true;false|] ;}
            let actual = JLessOrEqual x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Unicode`` () =
            //"aab" LessOrEqual "baa" = true true false
            let x = {JType = JTBType.JTypeUnicode; JShape= [|3|]; JValue = JTypeUnicodeArray [|'a';'a';'b'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|3|]; JValue = JTypeUnicodeArray [|'b';'a';'a'|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|true;true;false|] ;}
            let actual = JLessOrEqual x y
            Assert.Equal(expected,actual) 

    type ``Test JLargerOrEqual:`` () =       
        [<Fact>]
        member x.``Integer`` () =
            //1 1 2 LargerOrEqual 2 1 1 = false true true 
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;1L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;1L;1L|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|false;true;true|] ;}
            let actual = JLargerOrEqual x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Float`` () =
            //1.0 1.0 2.0 LargerOrEqual 2.0 1.0 1.0 = false true true
            let x = {JType = JTBType.JTypeFloat; JShape= [|3|]; JValue = JTypeFloatArray [|1.0;1.0;2.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|3|]; JValue = JTypeFloatArray [|2.0;1.0;1.0|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|false;true;true|] ;}
            let actual = JLargerOrEqual x y
            Assert.Equal(expected,actual)                             
        [<Fact>]
        member x.``Boolean`` () =
            //false false true LargerOrEqual true false false = false true true
            let x = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|false;false;true|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|true;false;false|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|false;true;true|] ;}
            let actual = JLargerOrEqual x y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Unicode`` () =
            //"aab" LargerOrEqual "baa" = false true true
            let x = {JType = JTBType.JTypeUnicode; JShape= [|3|]; JValue = JTypeUnicodeArray [|'a';'a';'b'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|3|]; JValue = JTypeUnicodeArray [|'b';'a';'a'|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|false;true;true|] ;}
            let actual = JLargerOrEqual x y
            Assert.Equal(expected,actual) 
 




    type ``Test JFold:`` () =   
        [<Fact>]
        member x.``If right argument is scalar you should get rank error`` () =
            //0 Plus Fold 3 should give rank error
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
                let actual = JFold x y JAdd
                ()
            Assert.Throws<JExceptionRankError>(f)                      
        [<Fact>]
        member x.``Sum of integers`` () =
            //0 Plus Fold 1 2 3 should give 6 
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|6L|] ;}
            let actual = JFold x y JAdd
            Assert.Equal(expected,actual)
        [<Fact>]
        member x.``If right argument is vector and left argument is not scalar you should get length error`` () =
            //0 0 Plus Fold 1 2 3 should give length error
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;0L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
                let actual = JFold x y JAdd
                ()
            Assert.Throws<JExceptionLengthError>(f)  
        [<Fact>]
        member x.``If right argument is empty array, if ShapeOF left argument is equal to 1 Drop ShapeOf right argument you should return left argument`` () =
            //(1 Shape 6) Plus Fold 0 1 Shape i.0 should give 1 Shape 6
            let x = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|6L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0;1|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|6L|] ;}
            let actual = JFold x y JAdd
            Assert.Equal(expected,actual)
        [<Fact>]
        member x.``If right argument is empty array, result should be x`` () =
            //( 1 Shape 6) Plus Fold 0 2 Shape i.0 should give 1 Shape 6
            let x = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|6L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0;2|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|6L|] ;}
            let actual = JFold x y JAdd
            Assert.Equal(expected,actual)
 

        [<Fact>]
        member x.``Sum integer array of rank greater than 1`` () =
            //0 0 Plus Fold 2 2 Shape 1 2 3 4 should give 4 6
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|4L;6L|] ;}
            let actual = JFold x y JAdd
            Assert.Equal(expected,actual)
        [<Fact>]
        member x.``Sum integer array of rank  3`` () =
            //( 2 2 Shape 0) Plus Fold i. 2 2 2 should give 2 2 Shape 4 6 8 10
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;0L;0L;0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|4L;6L;8L;10L|] ;}
            let actual = JFold x y JAdd
            Assert.Equal(expected,actual)
        [<Fact>]
        member x.``Left argument of rank 1`` () =
            //0 0 Plus Fold 2 2 Shape 1 2 3 4 should give 4 6
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|4L;6L|] ;}
            let actual = JFold x y JAdd
            Assert.Equal(expected,actual)

    type ``Test GetPartOfNounAsNoun:`` () =   
        [<Fact>]
        member x.``Get part of integer noun`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let actual = GetPartOfNounAsNoun y 1 2
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Get part of float noun`` () =
            let y = {JType = JTBType.JTypeFloat; JShape= [|4|]; JValue = JTypeFloatArray [|1.0;2.0;3.0;4.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.0;3.0|] ;}
            let actual = GetPartOfNounAsNoun y 1 2
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Get part of boolean noun`` () =
            let y = {JType = JTBType.JTypeBoolean; JShape= [|4|]; JValue = JTypeBooleanArray [|true;false;false;true|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|false;false|] ;}
            let actual = GetPartOfNounAsNoun y 1 2
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Get part of unicode noun`` () =
            let y = {JType = JTBType.JTypeUnicode; JShape= [|4|]; JValue = JTypeUnicodeArray [|'a';'b';'c';'d'|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'b';'c'|] ;}
            let actual = GetPartOfNounAsNoun y 1 2
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``Get part of boxed noun`` () =
            let box1 = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeBooleanArray [|false;true;true;true|] ;}
            let box2 = {JType = JTBType.JTypeFloat; JShape= [|2;2|]; JValue = JTypeFloatArray [|1.0;2.0;3.0;4.0|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|4|]; JValue = JTypeBoxedArray [|box1;box2;box1;box2|] ;}
            let expected = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|box2;box1|] ;}
            let actual = GetPartOfNounAsNoun y 1 2
            Assert.Equal(expected,actual) 
                                           
                                           
    type ``Test JScan:`` () =   

        [<Fact>]
        member x.``0 + Scan Iota 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JScan x y JAdd
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 + Scan 0 should give rank error`` () =
            let f ()  =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
                let actual = JScan x y JAdd
                ()
            Assert.Throws<JExceptionRankError>(f)  

        [<Fact>]
        member x.``0 + Scan (1 Shape 0) should give 1 Shape 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|0L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|0L|] ;}
            let actual = JScan x y JAdd
            Assert.Equal(expected,actual)  

    
        [<Fact>]
        member x.``0 + Scan 1 2 3 should give 1 3 6`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;3L;6L|] ;}
            let actual = JScan x y JAdd
            Assert.Equal(expected,actual)  
               
        [<Fact>]
        member x.``0 + Scan 2 2 Shape 1 2 3 4 should give rank error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
                let actual = JScan x y JAdd
                ()
            Assert.Throws<JExceptionRankError>(f) 

        [<Fact>]
        member x.``0 0 + Scan 2 2 Shape 1 2 3 4 should give 2 2 Shape 1 2 4 6`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;4L;6L|] ;}
            let actual = JScan x y JAdd
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 ( LeftNoun + 0 + Fold RightNoun ) Scan 2 2 Shape 1 2 3 4 should give 2 Shape 3 10`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;10L|] ;}
            let f x y = JAdd x ( JFold ZeroNounConstant y JAdd )
            let actual = JScan x y f
            Assert.Equal(expected,actual) 
            
           
              
            
    type ``Test JCatenate:`` () =   

        [<Fact>]
        member x.``(Iota 0) Catenate Iota 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)    

        [<Fact>]
        member x.``(Iota 0) Catenate 0 should give 1 Shape 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|0L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 0) Catenate 0 1 should give 2 Shape 0 1`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 1 Catenate Iota 0 should give 2 Shape 0 1`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 0) Catenate Iota 2 2 should give 3 2 Shape 0 0 0 1 2 3`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;2|]; JValue = JTypeIntegerArray [|0L;0L;0L;1L;2L;3L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 2 2 ) Catenate Iota 0 should give 3 2 Shape 0 1 2 3 0 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;0L;0L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 Catenate Iota 0 should give 1 Shape 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|0L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 Catenate 1 should give 2 Shape 0 1`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)     

        [<Fact>]
        member x.``0 1 Catenate 2 should give 3 Shape 0 1 2`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|0L;1L;2L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)        

        [<Fact>]
        member x.``0 Catenate 1 2 should give 3 Shape 0 1 2`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|0L;1L;2L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)   

        [<Fact>]
        member x.``0 1 Catenate 2 3 should give 4 Shape 0 1 2 3`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)   
                
        [<Fact>]            
        member x.``0 1 Catenate 2 2 Shape 2 3 4 5 should give 3 2 Shape 0 1 2 3 4 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|2L;3L;4L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual) 
            
        [<Fact>]
        member x.``(2 2 Shape 0 1 2 3 ) Catenate 1 2 Shape 4 5 should give 3 2 Shape 0 1 2 3 4 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|1;2|]; JValue = JTypeIntegerArray [|4L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)                

        [<Fact>]
        member x.``(2 2 Shape 0 1 2 3 ) Catenate 4 5 should give 3 2 Shape 0 1 2 3 4 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|4L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)  
          
        [<Fact>]       
        member x.``(2 2 Shape 0 1 2 3 ) Catenate 4 should give 3 2 Shape 0 1 2 3 4 4`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;4L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)   

        [<Fact>]
        member x.``0 Catenate (2 2 Shape 1 2 3 4 ) should give 3 2 Shape 0 0 1 2 3 4`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;2|]; JValue = JTypeIntegerArray [|0L;0L;1L;2L;3L;4L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)    

        [<Fact>]
        member x.``(2 2 Shape 0 1 2 3 ) Catenate 4 5 6 should give 3 3 Shape 0 1 0 2 3 0 4 5 6`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|4L;5L;6L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;3|]; JValue = JTypeIntegerArray [|0L;1L;0L;2L;3L;0L;4L;5L;6L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)     

        [<Fact>]
        member x.``0 1 2 Catenate (2 2 Shape 3 4 5 6 ) should give 3 3 Shape 0 1 2 3 4 0 5 6 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|4L;5L;6L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;3|]; JValue = JTypeIntegerArray [|0L;1L;0L;2L;3L;0L;4L;5L;6L|] ;}
            let actual = JCatenate x y
            Assert.Equal(expected,actual)    

        [<Fact>]
        member x.``Catenation of different types should give domain error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
                let y = {JType = JTBType.JTypeFloat; JShape= [|3|]; JValue = JTypeFloatArray [|4.0;5.0;6.0|] ;}
                let actual = JCatenate x y
                ()
            Assert.Throws<JExceptionDomainError>(f)  
               
    type ``Test JFrom:`` () =  

        [<Fact>]
        member x.``(Iota 0) From Iota 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``(Iota 0) From 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``0 From Iota 0 should give Index Error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
                let actual = JFrom x y
                ()
            Assert.Throws<JExceptionIndexError>(f)  

        [<Fact>]
        member x.``(2 0 Shape Iota 0) From Iota 0 should give 2 0 Shape Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual) 


        [<Fact>]
        member x.``2 From Iota 5 should give 2`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``-1 From Iota 5 should give Index Error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
                let actual = JFrom x y
                ()
            Assert.Throws<JExceptionIndexError>(f)   
        [<Fact>]
        member x.``2 4 From Iota 5 should give 2 4`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``2 5 From Iota 5 should give Index Error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;5L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
                let actual = JFrom x y
                ()
            Assert.Throws<JExceptionIndexError>(f)  
        [<Fact>]
        member x.``1 From Iota 2 2 should give 2 3`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``-1 From Iota 2 2 should give index error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
                let actual = JFrom x y
                ()
            Assert.Throws<JExceptionIndexError>(f)
        [<Fact>]
        member x.``2 4 From Iota 5 2 should give 2 2 Shape 4 5 8 9`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|4L;5L;8L;9L|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  
        [<Fact>]
        member x.``2 5 From Iota 5 2 should give Index Error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;5L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
                let actual = JFrom x y
                ()
            Assert.Throws<JExceptionIndexError>(f) 
        [<Fact>]
        member x.``(1 2 Shape 2 4) From Iota 5 should give 1 2 Shape 2 4`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|1;2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1;2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``(2 0 Shape Iota 0) From Iota 5 should give 2 0 Shape Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2;0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``(1 2 Shape 2 4) From Iota 5 2 should give 1 2 2 Shape 4 5 8 9`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|1;2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1;2;2|]; JValue = JTypeIntegerArray [|4L;5L;8L;9L|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``(0 2 Shape Iota 0) From Iota 5 2 should give 0 2 2 Shape Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0;2|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0;2;2|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``(Iota 5) From Iota 5 2 should give Iota 5 2 `` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``0 From Iota 2 2 2 should give 2 2 Shape 0 1 2 3 `` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``Float`` () =
            //2 4 From 0.0 1.0 2.0 3.0 4.0 should give 2.0 4.0
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|5|]; JValue = JTypeFloatArray [|0.0;1.0;2.0;3.0;4.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|2.0;4.0|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``Boolean`` () =
            //2 4 From false false true false true should give true true
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|5|]; JValue = JTypeBooleanArray [|false;false;true;false;true|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;true|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``Unicode`` () =
            //2 4 From "abcde" should give "ce"
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|5|]; JValue = JTypeUnicodeArray [|'a';'b';'c';'d';'e'|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'c';'e'|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``Boxed`` () =
            //2 4 From "a" "b" "c" "d" "e" should give "c" "e" (boxed values)
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let boxa = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'a'|] ;}
            let boxb = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'b'|] ;}
            let boxc = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'c'|] ;}
            let boxd = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'d'|] ;}
            let boxe = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'e'|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|5|]; JValue = JTypeBoxedArray [|boxa;boxb;boxc;boxd;boxe|] ;}
            let expected = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|boxc;boxe|] ;}
            let actual = JFrom x y
            Assert.Equal(expected,actual) 

            
    type ``Test JAmend:`` () =  

       
    
        [<Fact>]
        member x.``(( Box 2 ),Box -1 ) Amend Iota 5 should give 0 1 -1 3 4`` () =
            let i = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let p = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;-1L;3L;4L|] ;}
            let actual = JAmend x y
            Assert.Equal(expected,actual) 


        [<Fact>]
        member x.``(( Box 2 ), Box  -1 -2 ) Amend Iota 5 should give rank error`` () =
            let f () =
                let i = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
                let p = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|-1L;-2L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
                let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
                let actual = JAmend x y
                ()
            Assert.Throws<JExceptionRankError>(f) 

        [<Fact>]
        member x.``(( Box 2 4 ),Box -1 -2) Amend Iota 5 should give 0 1 -1 3 -2`` () =
            let i = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let p = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|-1L;-2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;-1L;3L;-2L|] ;}
            let actual = JAmend x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``(( Box Iota 0 ),Box Iota 0) Amend Iota 5 should give 0 1 2 3 4`` () =
            let i = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let p = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
            let actual = JAmend x y
            Assert.Equal(expected,actual)  


        [<Fact>]
        member x.``(( Box 2 4 ),Box -1 Amend Iota 5 should give rank error`` () =
            let f () =
                let i = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
                let p = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|-1L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
                let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
                let actual = JAmend x y
                ()
            Assert.Throws<JExceptionRankError>(f) 

        [<Fact>]
        member x.``(( Box 2 4 ),Box -1 -2 -3 Amend Iota 5 should give length error`` () =
            let f () =
                let i = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
                let p = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|-1L;-2L;-3L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L|] ;}
                let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
                let actual = JAmend x y
                ()
            Assert.Throws<JExceptionLengthError>(f) 

        [<Fact>]
        member x.``(( Box 2 4 ),Box 2 2 Shape -1 -2 -3 -4) Amend Box Iota 5 2 should give 5 2 $ 0 1 2 3 -1 -2 6 7 -3 -4`` () =
            let i = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let p = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|-1L;-2L;-3L;-4L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;-1L;-2L;6L;7L;-3L;-4L|] ;}
            let actual = JAmend x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``(( Box Iota 0 ),Box 0 2 Shape Iota 0) Amend Box Iota 5 2 should give 5 2 $ 0 1 2 3 4 5 6 7 8 9`` () =
            let i = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let p = {JType = JTBType.JTypeInteger; JShape= [|0;2|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
            let actual = JAmend x y
            Assert.Equal(expected,actual)  

        [<Fact>]
        member x.``(( Box 2 4 ),Box  2 Shape -1 -2 ) Amend Box Iota 5 2 should give rank error`` () =
            let f () =
                let i = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
                let p = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|-1L;-2L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
                let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
                let actual = JAmend x y
                ()
            Assert.Throws<JExceptionRankError>(f)   

        [<Fact>]
        member x.``(( Box 2 4 ),Box  2 3 Shape -1 -2 -3 -4 -5 -6 ) Amend Box Iota 5 2 should give length error`` () =
            let f () =
                let i = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
                let p = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|-1L;-2L;-3L;-4L;-5L;-6L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|5;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L|] ;}
                let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
                let actual = JAmend x y
                ()
            Assert.Throws<JExceptionLengthError>(f)  

        [<Fact>]
        member x.``(( Box 2 ),Box -1.0 ) Amend 0.0 1.0 2.0 3.0 4.0 should give 0.0 1.0 -1.0 3.0 4.0`` () =
            let i = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let p = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-1.0|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|5|]; JValue = JTypeFloatArray [|0.0;1.0;2.0;3.0;4.0|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [|5|]; JValue = JTypeFloatArray [|0.0;1.0;-1.0;3.0;4.0|] ;}
            let actual = JAmend x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(( Box 2 ),Box true) Amend false false false false false should give false false true false false`` () =
            let i = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let p = {JType = JTBType.JTypeBoolean; JShape= [||]; JValue = JTypeBooleanArray [|true|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|5|]; JValue = JTypeBooleanArray [|false;false;false;false;false|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|5|]; JValue = JTypeBooleanArray [|false;false;true;false;false|] ;}
            let actual = JAmend x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(( Box 2 ),Box "x" ) Amend "abcde"  should give "abxde"`` () =
            let i = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let p = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'x'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|5|]; JValue = JTypeUnicodeArray [|'a';'b';'c';'d';'e'|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|5|]; JValue = JTypeUnicodeArray [|'a';'b';'x';'d';'e'|] ;}
            let actual = JAmend x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(( Box 2 ),Box Box "x" ) Amend (Box "a") , (Box "b") ,(Box "c") ,(Box "d") ,Box "e" , should give (Box "a") , (Box "b") ,(Box "x") ,(Box "d") ,(Box "e")`` () =
            let i = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let a = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'a'|] ;}
            let boxa = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|a|] ;}
            let b = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'b'|] ;}
            let boxb = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|b|] ;}
            let c = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'c'|] ;}
            let boxc = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|c|] ;}
            let d = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'d'|] ;}
            let boxd = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|d|] ;}
            let e = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'e'|] ;}
            let boxe = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|e|] ;}
            let x = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'x'|] ;}
            let boxx = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|x|] ;}
            let p = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|boxx|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|5|]; JValue = JTypeBoxedArray [|boxa;boxb;boxc;boxd;boxe|] ;}
            let x = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|i;p|] ;}
            let expected = {JType = JTBType.JTypeBoxed; JShape= [|5|]; JValue = JTypeBoxedArray [|boxa;boxb;boxx;boxd;boxe|] ;}
            let actual = JAmend x y
            Assert.Equal(expected,actual) 

    type ``Test JGradeUp:`` () =  

        [<Fact>]
        member x.``GradeUp 3 should give rank error`` () =
            let f () =
                let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
                let actual = JGradeUp y
                ()
            Assert.Throws<JExceptionRankError>(f)         
    
        [<Fact>]
        member x.``GradeUp 3 1 2 should give 1 2 0`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|3L;1L;2L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;0L|] ;}
            let actual = JGradeUp y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``GradeUp Iota 0 should give Iota 0`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JGradeUp y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``GradeUp 3 1 Shape 3 1 2 should give 1 2 0`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|3;1|]; JValue = JTypeIntegerArray [|3L;1L;2L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;0L|] ;}
            let actual = JGradeUp y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``GradeUp 3 0 Shape Iota 0 should give 0 1 2`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|3;0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|0L;1L;2L|] ;}
            let actual = JGradeUp y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``GradeUp 0 0 Shape Iota 0 should give Iota 0`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|0;0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JGradeUp y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``GradeUp 3_0 1_0 2_0 should give 1 2 0`` () =
            let y = {JType = JTBType.JTypeFloat; JShape= [|3|]; JValue = JTypeFloatArray [|3.0;1.0;2.0|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;0L|] ;}
            let actual = JGradeUp y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``GradeUp true false should give 1 0`` () =
            let y = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let actual = JGradeUp y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``GradeUp "cab" should give 1 2 0`` () =
            let y = {JType = JTBType.JTypeUnicode; JShape= [|3|]; JValue = JTypeUnicodeArray [|'c';'a';'b'|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;0L|] ;}
            let actual = JGradeUp y
            Assert.Equal(expected,actual) 

    type ``Test selectIndices:`` () = 
        [<Fact>]
        member x.``selectIndices true false should give 0`` () =
            let q = [|true;false|]
            let actual = selectIndices q
            let expected = [|0|]
            Assert.Equal<JTypeInt array>(expected,actual) 

        [<Fact>]
        member x.``selectIndices empty should give empty`` () =
            let q = [||]
            let actual = selectIndices q
            let expected = [||]
            Assert.Equal<JTypeInt array>(expected,actual) 

    type ``Test replicateIndices:`` () = 
        [<Fact>]
        member x.``replicateIndices 2 3 0 should give 0 0 1 1 1`` () =
            let a = [|2;3;0|]
            let actual = replicateIndices a
            let expected = [|0;0;1;1;1|]
            Assert.Equal<JTypeInt array>(expected,actual) 

        [<Fact>]
        member x.``replicateIndices empty should give empty`` () =
            let a = [||]
            let actual = replicateIndices a
            let expected = [||]
            Assert.Equal<JTypeInt array>(expected,actual) 

    type ``Test JSelect:`` () = 

        [<Fact>]
        member x.``true Select 0 should give 0`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [||]; JValue = JTypeBooleanArray [|true|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``false Select 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [||]; JValue = JTypeBooleanArray [|false|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 


        [<Fact>]
        member x.``0 Select 0 should give Domain error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
                let actual = JSelect x y
                ()
            Assert.Throws<JExceptionDomainError>(f) 

        [<Fact>]
        member x.``true false Select 4 5 should give 1 Shape 4`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|4L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|4L|] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``Empty boolean Select Iota 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|0|]; JValue = JTypeBooleanArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``true false Select 1 Shape 1 should give Length error`` () =
            let f () =
                let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|1L|] ;}
                let actual = JSelect x y
                ()
            Assert.Throws<JExceptionLengthError>(f) 

        [<Fact>]
        member x.``true false Select Iota 2 2 should give 1 2 Shape 0 1`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1;2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``Empty boolean Select Iota 0 2 should give 0 2 Shape Iota 0`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|0|]; JValue = JTypeBooleanArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0;2|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0;2|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``true false Select Iota 1 2 should give Length error`` () =
            let f () =
                let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|1;2|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
                let actual = JSelect x y
                ()
            Assert.Throws<JExceptionLengthError>(f)

        [<Fact>]
        member x.``( 2 2 Shape true false) Select Iota 2 2 1 should give Rank error`` () =
            let f () =
                let x = {JType = JTBType.JTypeBoolean; JShape= [|2;2|]; JValue = JTypeBooleanArray [|true;false;true;false|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|2;2;1|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
                let actual = JSelect x y
                ()
            Assert.Throws<JExceptionRankError>(f)

        [<Fact>]
        member x.``true false Select 4_0 5_0 should give 1 Shape 4_0`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|4.0;5.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [|1|]; JValue = JTypeFloatArray [|4.0|] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``true false Select false true should give 1 Shape false`` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|false;true|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|1|]; JValue = JTypeBooleanArray [|false|] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``true false Select "ab" should give 1 Shape "a" `` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|1|]; JValue = JTypeUnicodeArray [|'a'|] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``true false Select (Box "a") , Box "b" should give 1 Shape Box "a" `` () =
            let x = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let a = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'a'|] ;}
            let boxa = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|a|] ;}
            let b = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'b'|] ;}
            let boxb = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|b|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|boxa;boxb|] ;}
            let expected = {JType = JTBType.JTypeBoxed; JShape= [|1|]; JValue = JTypeBoxedArray [|boxa|] ;}
            let actual = JSelect x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.`` false true true false Select Rank 1 1 Iota 2 4 should give 2 2 Shape 1 2 5 6 `` () =
        // { false true true false /:: '/ 1 1 / |i. 2 4 } should give 2 2 Shape 1 2 5 6
            let x = {JType = JTBType.JTypeBoolean; JShape= [|4|]; JValue = JTypeBooleanArray [|false;true;true;false|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;4|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let rank = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;1L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;2L;5L;6L|] ;}
            let actual = JRankDyadic x y JSelect rank
            Assert.Equal(expected,actual) 



    type ``Test JReplicate:`` () = 

        [<Fact>]
        member x.``2 Replicate 4 5 should give  4 4 5 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|4L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|4L;4L;5L;5L|] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``1 2 Replicate 4 should give  4 4 4`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|4L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|4L;4L;4L|] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``2 2 0 Replicate 4 5 6 should give  4 4 5 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|4L;5L;6L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|4L;4L;5L;5L|] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 0) Replicate Iota 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``2 2 0 Replicate 4 5 6 7 should give Length error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;0L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|4L;5L;6L;7L|] ;}
                let actual = JReplicate x y
                ()
            Assert.Throws<JExceptionLengthError>(f)

        [<Fact>]
        member x.``2 Replicate 1 2 Shape 4 5 should give 2 2 Shape 4 5 4 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|1;2|]; JValue = JTypeIntegerArray [|4L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|4L;5L;4L;5L|] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``2 2 0 Replicate 3 1 Shape 4 5 6 should give 4 1 Shape 4 4 5 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3;1|]; JValue = JTypeIntegerArray [|4L;5L;6L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|4;1|]; JValue = JTypeIntegerArray [|4L;4L;5L;5L|] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 0) Replicate 0 1 Shape Iota 0 should give 0 1 Shape Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0;1|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0;1|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``2 2 0 Replicate 4 1 Shape 4 5 6 7 should give Length error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;0L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|4;1|]; JValue = JTypeIntegerArray [|4L;5L;6L;7L|] ;}
                let actual = JReplicate x y
                ()
            Assert.Throws<JExceptionLengthError>(f)

        [<Fact>]
        member x.``2_0 Replicate 4 5 should give  Doamain error`` () =
            let f () =
                let x = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|2.0|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|4L;5L|] ;}
                let actual = JReplicate x y
                ()
            Assert.Throws<JExceptionDomainError>(f) 

        [<Fact>]
        member x.``2 2 0 Replicate 4_0 5_0 6_0 should give  4_0 4_0 5_0 5_0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;0L|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|3|]; JValue = JTypeFloatArray [|4.0;5.0;6.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [|4|]; JValue = JTypeFloatArray [|4.0;4.0;5.0;5.0|] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``2 2 0 Replicate true false true should give  true true false false`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;0L|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|3|]; JValue = JTypeBooleanArray [|true;false;true|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|4|]; JValue = JTypeBooleanArray [|true;true;false;false|] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``2 2 0 Replicate "abc" should give "aabb" `` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;0L|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|3|]; JValue = JTypeUnicodeArray [|'a';'b';'c'|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|4|]; JValue = JTypeUnicodeArray [|'a';'a';'b';'b'|] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``2 2 0 Replicate (Box "a") , (Box "b") , Box "c" should give (Box "a"),(Box "a"),(Box "b"), Box "b" `` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;0L|] ;}
            let a = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'a'|] ;}
            let boxa = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|a|] ;}
            let b = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'b'|] ;}
            let boxb = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|b|] ;}
            let c = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'c'|] ;}
            let boxc = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|c|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|3|]; JValue = JTypeBoxedArray [|boxa;boxb;boxc|] ;}
            let expected = {JType = JTBType.JTypeBoxed; JShape= [|4|]; JValue = JTypeBoxedArray [|boxa;boxa;boxb;boxb|] ;}
            let actual = JReplicate x y
            Assert.Equal(expected,actual) 

    type ``Test JRavel:`` () = 

        [<Fact>]
        member x.``Ravel 5 should give 1 Shape 5`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|1|]; JValue = JTypeIntegerArray [|5L|] ;}
            let actual = JRavel y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``Ravel Iota 2 2 should give 4 Shape 0 1 2 3`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let actual = JRavel y
            Assert.Equal(expected,actual) 

    type ``Test JTally:`` () = 

        [<Fact>]
        member x.``Tally 5 should give 1`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let actual = JTally y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``Tally Iota 2 2 should give 2`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let actual = JTally y
            Assert.Equal(expected,actual) 


    type ``Test JIotaDyadic:`` () = 

        [<Fact>]
        member x.``0 1 2 3 JIota 2 3 should give  2 3`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;3L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 1 2 3 JIota 2 5 should give  2 4`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;4L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 1 2 3 JIota 2 2 5 should give  2 2 4`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;4L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 1 2 2 3 JIota 2 5 should give  2 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;5L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 1 2 2 3 JIota 2 2 5 should give  2 2 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|2L;2L;5L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 1 2 2 3 JIota 2 1 Shape 2 5 should give 2 1 Shape 2 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;1|]; JValue = JTypeIntegerArray [|2L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2;1|]; JValue = JTypeIntegerArray [|2L;5L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 1 2 2 3 JIota Iota 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|5|]; JValue = JTypeIntegerArray [|0L;1L;2L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 0) JIota 2 5 should give 0 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|2L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|0L;0L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 0) JIota Iota 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 1 2 3 JIota 2 should give  2`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``2 JIota 2 should give  0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``3 JIota 2 should give  1`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|3L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|2L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``"abc" JIota "b" should give  1`` () =
            let x = {JType = JTBType.JTypeUnicode; JShape= [|3|]; JValue = JTypeUnicodeArray [|'a';'b';'c'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'b'|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 4 2) JIota Iota -4 2 should give  3 2 1 0 `` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|4;2|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|4;2|]; JValue = JTypeIntegerArray [|6L;7L;4L;5L;2L;3L;0L;1L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|3L;2L;1L;0L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(4 2 Shape "abcdefgh" ) JIota (4 2 Shape "ghefcdab" ) should give  3 2 1 0 `` () =
            let x = {JType = JTBType.JTypeUnicode; JShape= [|4;2|]; JValue = JTypeUnicodeArray [|'a';'b';'c';'d';'e';'f';'g';'h'|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|4;2|]; JValue = JTypeUnicodeArray [|'g';'h';'e';'f';'c';'d';'a';'b'|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|4|]; JValue = JTypeIntegerArray [|3L;2L;1L;0L|] ;}
            let actual = JIotaDyadic x y
            Assert.Equal(expected,actual) 






    type ``Test GradeUp:`` () = 

        [<Fact>]
        member x.``GradeUp(FSharp version) 5 4 3 2 1 should give 4 3 2 1 0  `` () =
            let (expected :JTypeInt array) = [|4;3;2;1;0|]
            let compare s e = GradeUpCompare s e [|5;4;3;2;1|]
            let (actual: JTypeInt array) = GradeUp 5 compare
            Assert.Equal<JTypeInt array>(expected,actual) 

        [<Fact>]
        member x.``GradeUp(FSharp version) 3 3 3 should give 0 1 2 `` () =
            let (expected :JTypeInt array) = [|0;1;2|]
            let compare s e = GradeUpCompare s e [|3;3;3|]
            let (actual: JTypeInt array) = GradeUp 3 compare
            Assert.Equal<JTypeInt array>(expected,actual) 

        [<Fact>]
        member x.``GradeUp(FSharp version) empty should give empty  `` () =
            let (expected :JTypeInt array) = [||]
            let compare s e = GradeUpCompare s e [||]
            let (actual: JTypeInt array) = GradeUp 0 compare
            Assert.Equal<JTypeInt array>(expected,actual) 

                        
    type ``Test JTranspose:`` () = 

        [<Fact>]
        member x.``1 0 JTranspose Iota 2 3 should give 3 2 Shape 0 3 1 4 2 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;2|]; JValue = JTypeIntegerArray [|0L;3L;1L;4L;2L;5L|] ;}
            let actual = JTranspose x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``1 0 2 JTranspose Iota 2 3 should give length error`` () =
            let f  () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;0L;2L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L|] ;}
                let actual = JTranspose x y
                ()
            Assert.Throws<JExceptionLengthError>(f) 

        [<Fact>]
        member x.``1 2 JTranspose Iota 2 3 should give rank error`` () =
            let f  () =
                let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L|] ;}
                let actual = JTranspose x y
                ()
            Assert.Throws<JExceptionRankError>(f) 

        [<Fact>]
        member x.``1_0 2_0 JTranspose Iota 2 3 should give domain error`` () =
            let f  () =
                let x = {JType = JTBType.JTypeFloat; JShape= [|2|]; JValue = JTypeFloatArray [|1.0;2.0|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [|2;3|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L|] ;}
                let actual = JTranspose x y
                ()
            Assert.Throws<JExceptionDomainError>(f) 

        [<Fact>]
        member x.``1 2 0 JTranspose Iota 2 3 4 should give 3 4 2 Shape 0 12 1 13 2 14 3 15 4 16 5 17 6 18 7 19 8 20 9 21 10 22 11 23 `` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|3|]; JValue = JTypeIntegerArray [|1L;2L;0L|] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;3;4|]; JValue = JTypeIntegerArray [|0L;1L;2L;3L;4L;5L;6L;7L;8L;9L;10L;11L;12L;13L;14L;15L;16L;17L;18L;19L;20L;21L;22L;23L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|3;4;2|]; JValue = JTypeIntegerArray [|0L;12L;1L;13L;2L;14L;3L;15L;4L;16L;5L;17L;6L;18L;7L;19L;8L;20L;9L;21L;10L;22L;11L;23L|] ;}
            let actual = JTranspose x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 0) JTranspose Iota 0 should give Iota 0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let actual = JTranspose x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``(Iota 0) JTranspose 5 should give 5`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let expected = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
            let actual = JTranspose x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``0 JTranspose 5 should give length error`` () =
            let f () =
                let x = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
                let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|5L|] ;}
                let actual = JTranspose x y
                ()
            Assert.Throws<JExceptionLengthError>(f) 

        [<Fact>]
        member x.``1 0 JTranspose 1 2 Shape 1_0 2_0 should give 2 1 Shape 1_0 2_0`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let y = {JType = JTBType.JTypeFloat; JShape= [|1;2|]; JValue = JTypeFloatArray [|1.0;2.0|] ;}
            let expected = {JType = JTBType.JTypeFloat; JShape= [|2;1|]; JValue = JTypeFloatArray [|1.0;2.0|] ;}
            let actual = JTranspose x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``1 0 JTranspose 1 2 Shape true false should give 2 1 Shape true false`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let y = {JType = JTBType.JTypeBoolean; JShape= [|1;2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let expected = {JType = JTBType.JTypeBoolean; JShape= [|2;1|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let actual = JTranspose x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``1 0 JTranspose 1 2 Shape "a" "b" should give 2 1 Shape "a" "b"`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let y = {JType = JTBType.JTypeUnicode; JShape= [|1;2|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2;1|]; JValue = JTypeUnicodeArray [|'a';'b'|] ;}
            let actual = JTranspose x y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``1 0 JTranspose 1 2 Shape (Box "a"), Box "b" should give 2 1 Shape (Box "a") , Box "b"`` () =
            let x = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;0L|] ;}
            let a = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'a'|] ;}
            let boxa = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|a|] ;}
            let b = {JType = JTBType.JTypeUnicode; JShape= [||]; JValue = JTypeUnicodeArray [|'b'|] ;}
            let boxb = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|b|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|1;2|]; JValue = JTypeBoxedArray [|boxa;boxb|] ;}
            let expected = {JType = JTBType.JTypeBoxed; JShape= [|2;1|]; JValue = JTypeBoxedArray [|boxa;boxb|] ;}
            let actual = JTranspose x y
            Assert.Equal(expected,actual) 

    type ``Test JDefaultFormat:`` () = 

        [<Fact>]
        member x.``JDefaultFormat 1 should give " 1"`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2|]; JValue = JTypeUnicodeArray [|' ';'1'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat 1 23 should give 1 5 Shape " 1 23"`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;23L|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|1;5|]; JValue = JTypeUnicodeArray [|' ';'1';' ';'2';'3'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 
        [<Fact>]
        member x.``JDefaultFormat 2 2 Shape 1 23 123 0 should give 2 7 Shape "  1 23" and " 123   0"`` () =
            let y = {JType = JTBType.JTypeInteger; JShape= [|2;2|]; JValue = JTypeIntegerArray [|1L;23L;123L;0L|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2;7|]; JValue = JTypeUnicodeArray [|' ';' ';' ';'1';' ';'2';'3';' ';'1';'2';'3';' ';' ';'0'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat -1234567_1234567e-300 should give " -1_23457e-294"`` () =
            let y = {JType = JTBType.JTypeFloat; JShape= [||]; JValue = JTypeFloatArray [|-1234567.1234567e-300|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|14|]; JValue = JTypeUnicodeArray [|' ';'-';'1';'.';'2';'3';'4';'5';'7';'e';'-';'2';'9';'4'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat true false should give 1 11 Shape " true false"`` () =
            let y = {JType = JTBType.JTypeBoolean; JShape= [|2|]; JValue = JTypeBooleanArray [|true;false|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|1;11|]; JValue = JTypeUnicodeArray [|' ';'t';'r';'u';'e';' ';'f';'a';'l';'s';'e'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat "Erling" should give "Erling"`` () =
            let y = {JType = JTBType.JTypeUnicode; JShape= [|5|]; JValue = JTypeUnicodeArray [|'E';'r';'l';'i';'n';'g'|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|5|]; JValue = JTypeUnicodeArray [|'E';'r';'l';'i';'n';'g'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat (Box 0) Catenate Box 1 should give 1 4 Shape " 0 1"`` () =
            let a = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let b = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|a;b|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|1;4|]; JValue = JTypeUnicodeArray [|' ';'0';' ';'1'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat 2 1 Shape (Box 0) Catenate Box 1 should give 2 2 Shape " 0 1"`` () =
            let a = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|0L|] ;}
            let b = {JType = JTBType.JTypeInteger; JShape= [||]; JValue = JTypeIntegerArray [|1L|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|2;1|]; JValue = JTypeBoxedArray [|a;b|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2;2|]; JValue = JTypeUnicodeArray [|' ';'0';' ';'1'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat Box 2 1 Shape 0 1 should give 2 2 Shape " 0 1"`` () =
            let a = {JType = JTBType.JTypeInteger; JShape= [|2;1|]; JValue = JTypeIntegerArray [|0L;1L|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [||]; JValue = JTypeBoxedArray [|a|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2;2|]; JValue = JTypeUnicodeArray [|' ';'0';' ';'1'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat (Box Iota 0 ) , Box Iota 0 should give empty boolean vector"`` () =
            let a = {JType = JTBType.JTypeInteger; JShape= [|0|]; JValue = JTypeIntegerArray [||] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|a;a|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|1;0|]; JValue = JTypeUnicodeArray [||] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat (Box 1 2 ) , Box 3 4 should give 1 8 Shape " 1 2 3 4"`` () =
            let a = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let b = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;4L|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|2|]; JValue = JTypeBoxedArray [|a;b|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|1;8|]; JValue = JTypeUnicodeArray [|' ';'1';' ';'2';' ';'3';' ';'4'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

        [<Fact>]
        member x.``JDefaultFormat 2 1 Shape (Box 1 2 ) , Box 3 4 should give 2 4 Shape " 1 2 3 4"`` () =
            let a = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|1L;2L|] ;}
            let b = {JType = JTBType.JTypeInteger; JShape= [|2|]; JValue = JTypeIntegerArray [|3L;4L|] ;}
            let y = {JType = JTBType.JTypeBoxed; JShape= [|2;1|]; JValue = JTypeBoxedArray [|a;b|] ;}
            let expected = {JType = JTBType.JTypeUnicode; JShape= [|2;4|]; JValue = JTypeUnicodeArray [|' ';'1';' ';'2';' ';'3';' ';'4'|] ;}
            let actual = JDefaultFormat y
            Assert.Equal(expected,actual) 

                                                                                                                                                                                                                         