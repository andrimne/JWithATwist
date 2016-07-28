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

open Microsoft.FSharp.Collections
open System
open Checked

module Base =

    type JTBType =
    | JTypeEmpty = 0
    | JTypeBoolean = 1
    | JTypeInteger = 4
    | JTypeFloat = 8
    | JTypeUnicode = 131072
    | JTypeBoxed = 32

    type JTypeBoolean = bool
    type JTypeInteger = int64
    type JTypeInt = int
    type JTypeFloat = double
    type JTypeUnicode = Char
    type JNoun = 
        {
            JType: JTBType
            JShape: JTypeInt[]
            JValue: JValueDU
        }
    and JValueDU =
    |JTypeBooleanArray of JTypeBoolean[]
    |JTypeIntegerArray of JTypeInteger[]
    |JTypeFloatArray of JTypeFloat[]
    |JTypeUnicodeArray of JTypeUnicode[]
    |JTypeBoxedArray of JNoun[]

    type JTypeBoxed = JNoun


    type JVerbMonadic = JNoun -> JNoun
    type JVerbDyadic = JNoun -> JNoun -> JNoun


    exception JExceptionRankError
    exception JExceptionDomainError
    exception JExceptionSystemError
    exception JExceptionValueError
    exception JExceptionLengthError
    exception JExceptionIndexError
    exception JExceptionStackFull
    exception JExceptionMemoryFull
    exception JExceptionSyntaxError
    exception ExceptionNotYetImplemented

    type JTypeBasicFunctionMonadic = JTypeBasicFunctionMonadicDU -> JTypeBasicFunctionMonadicDU
    and JTypeBasicFunctionMonadicDU =
    |JBasicFunctionMonadicBoolean of (JTypeBoolean -> JTypeBoolean)
    |JBasicFunctionMonadicBooleanToInteger of (JTypeBoolean -> JTypeInteger)
    |JBasicFunctionMonadicInteger of (JTypeInteger -> JTypeInteger)
    |JBasicFunctionMonadicFloat of (JTypeFloat -> JTypeFloat)
    |JBasicFunctionMonadicFloatToInteger of (JTypeFloat -> JTypeInteger)
    |JBasicFunctionMonadicUnicode of (JTypeUnicode -> JTypeUnicode)

    type JTypeBasicFunctioDyadic = JTypeBasicFunctionDyadicDU -> JTypeBasicFunctionDyadicDU
    and JTypeBasicFunctionDyadicDU =
    |JBasicFunctionDyadicBoolean of (JTypeBoolean -> JTypeBoolean -> JTypeBoolean)
    |JBasicFunctionDyadicInteger of (JTypeInteger -> JTypeInteger -> JTypeInteger)
    |JBasicFunctionDyadicIntegerToBoolean of (JTypeInteger -> JTypeInteger -> JTypeBoolean)
    |JBasicFunctionDyadicFloat of (JTypeFloat -> JTypeFloat -> JTypeFloat)
    |JBasicFunctionDyadicFloatToBoolean of (JTypeFloat -> JTypeFloat -> JTypeBoolean)
    |JBasicFunctionDyadicUnicode of (JTypeUnicode -> JTypeUnicode -> JTypeUnicode)
    |JBasicFunctionDyadicUnicodeToBoolean of (JTypeUnicode -> JTypeUnicode -> JTypeBoolean)
    |JBasicFunctionDyadicBoxedToBoolean of (JTypeBoxed -> JTypeBoxed -> JTypeBoolean)



    let  ArrayTake i (fill:'T) (array:'T[]): 'T[] =
        let takeDirectionForward = i>=0
        let absi = abs i
        let takeLength = min absi array.Length
        let fillLength = max 0 (absi - array.Length)
        let resultBeforeFill =
            match takeLength with 
            |0 -> 
                [||]
            |_ ->
                match takeDirectionForward with
                |true -> Array.sub array 0 takeLength
                |false -> Array.sub array (array.Length - takeLength) takeLength
        match fillLength with
        |0 -> 
            resultBeforeFill
        |_ ->
            let fillArray = Array.init fillLength (fun x -> fill)
            match takeDirectionForward with
            |true -> Array.append resultBeforeFill fillArray
            |false -> Array.append fillArray resultBeforeFill 

    let inline RepeatArray n a =
        Array.init n (fun i -> Array.get a (i % a.Length))

    let inline JTypeIntegerToJTypeFloat (jTypeIntegerArray:JTypeInteger []) : JTypeFloat [] =
        Array.map double jTypeIntegerArray

    let inline JTypeIntegerToJTypeInt (jTypeInteger:JTypeInteger) : JTypeInt =
        (int) jTypeInteger

    let inline JTypeIntToJTypeInteger (jTypeInt:JTypeInt) : JTypeInteger =
        (int64) jTypeInt

    let IotaZeroConstant = {JType=JTBType.JTypeInteger;JShape=[|0|];JValue=JTypeIntegerArray [||]}
    let OneNounConstant = {JType = JTBType.JTypeInteger ; JShape = [||]; JValue = JTypeIntegerArray [|1L|]; }
    let ZeroNounConstant = {JType = JTBType.JTypeInteger ; JShape = [||]; JValue = JTypeIntegerArray [|0L|]; }
    let JAce = {JType=JTBType.JTypeBoxed;JShape=[||];JValue=JTypeBoxedArray [|IotaZeroConstant|]}
    let MaxValueWithULPEqualToOne = 2.0 ** 51.0

    let JScalarFunctionMonadic (uBFM:JTypeBasicFunctionMonadicDU) xNoun : JNoun = 
        let inline execute (f:^a -> ^b) (y:^a) = 
            try
                f y
            with
            |_ ->
                reraise ()
        match xNoun.JValue,uBFM with
        |JTypeBooleanArray xValue,JBasicFunctionMonadicBoolean f->
            let rValue = Array.map (execute f) xValue
            {JType=JTBType.JTypeBoolean;JShape=xNoun.JShape;JValue=JTypeBooleanArray rValue}
        |JTypeBooleanArray xValue,JBasicFunctionMonadicBooleanToInteger f->
            let rValue = Array.map (execute f) xValue
            {JType=JTBType.JTypeInteger;JShape=xNoun.JShape;JValue=JTypeIntegerArray rValue}
        |JTypeIntegerArray xValue,JBasicFunctionMonadicInteger f ->
            let rValue = Array.map (execute f) xValue
            {JType=JTBType.JTypeInteger;JShape=xNoun.JShape;JValue=JTypeIntegerArray rValue}
        |JTypeFloatArray xValue,JBasicFunctionMonadicFloat f ->
            let rValue = Array.map (execute f) xValue
            {JType=JTBType.JTypeFloat;JShape=xNoun.JShape;JValue=JTypeFloatArray rValue}
        |JTypeFloatArray xValue,JBasicFunctionMonadicFloatToInteger f ->
            let rValue = Array.map (execute f) xValue
            {JType=JTBType.JTypeInteger;JShape=xNoun.JShape;JValue=JTypeIntegerArray rValue}
        |JTypeIntegerArray xValue,JBasicFunctionMonadicFloat f ->
            let rValue = Array.map (execute f) (JTypeIntegerToJTypeFloat xValue)
            {JType=JTBType.JTypeFloat;JShape=xNoun.JShape;JValue=JTypeFloatArray rValue}
        |JTypeUnicodeArray xValue,JBasicFunctionMonadicUnicode f ->
            let rValue = Array.map (execute f) xValue
            {JType=JTBType.JTypeUnicode;JShape=xNoun.JShape;JValue=JTypeUnicodeArray rValue}
        |_ -> raise JExceptionDomainError
    
    let JSignum yNoun =
        match yNoun.JType with
        |JTBType.JTypeInteger _ -> 
            let JSignumBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicInteger (fun x -> (int64) (sign x))
            JScalarFunctionMonadic JSignumBasic yNoun
        |JTBType.JTypeFloat _ ->
            let JSignumBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicFloatToInteger (fun x -> (int64) (sign x))
            JScalarFunctionMonadic JSignumBasic yNoun
        |_ ->
            raise JExceptionDomainError

    let JFloor yNoun =
        match yNoun.JType with
        |JTBType.JTypeFloat _ ->
            let inline f x =
                if (abs x) <= MaxValueWithULPEqualToOne then
                    (int64) (floor x)
                else
                    raise JExceptionDomainError
            let JFloorBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicFloatToInteger f
            JScalarFunctionMonadic JFloorBasic yNoun
        |_ ->
            raise JExceptionDomainError

    let JCeiling yNoun =
        match yNoun.JType with
        |JTBType.JTypeFloat _ ->
            let inline f x =
                if (abs x) <= MaxValueWithULPEqualToOne then
                    (int64) (ceil x)
                else
                    raise JExceptionDomainError
            let JCeilingBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicFloatToInteger f
            JScalarFunctionMonadic JCeilingBasic yNoun
        |_ ->
            raise JExceptionDomainError


    let JNegate yNoun =
        match yNoun.JType with
        |JTBType.JTypeInteger _ -> 
            let JNegateBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicInteger (fun x -> - x)
            JScalarFunctionMonadic JNegateBasic yNoun
        |JTBType.JTypeFloat _ ->
            let JNegateFloatBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicFloat (fun x -> - x)
            JScalarFunctionMonadic JNegateFloatBasic yNoun
        |_ ->
            raise JExceptionDomainError

    let JMagnitude yNoun =
        match yNoun.JType with
        |JTBType.JTypeInteger _ -> 
            let JMagnitudeBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicInteger (fun x -> abs x)
            JScalarFunctionMonadic JMagnitudeBasic yNoun
        |JTBType.JTypeFloat _ ->
            let JMagnitudeFloatBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicFloat (fun x -> abs x)
            JScalarFunctionMonadic JMagnitudeFloatBasic yNoun
        |_ ->
            raise JExceptionDomainError

    let JReciprocal xNoun =
        let JReciprocalBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicFloat (fun x -> 1.0/x)
        JScalarFunctionMonadic JReciprocalBasic xNoun

    let JNot xNoun =
        let JNotBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicBoolean (fun x -> not x)
        JScalarFunctionMonadic JNotBasic xNoun

    let JBti xNoun =
        let JBtiBasic : JTypeBasicFunctionMonadicDU = JBasicFunctionMonadicBooleanToInteger (fun q -> if q then 1L else 0L)
        JScalarFunctionMonadic JBtiBasic xNoun

    let JScalarFunctionDyadic (uBFD:JTypeBasicFunctionDyadicDU) xNoun yNoun : JNoun = 
        let inline func f xValue  (yValue:^a array) rType unionCase  =
            let inline execute (f:^a -> ^b -> ^c) (x:^a) (y:^b) = 
                try
                    f x y
                with
                | :? System.OverflowException -> 
                    raise JExceptionValueError
                |_ ->
                    reraise ()
            match xNoun.JShape,yNoun.JShape with
            //x and y are both scalars
            |xShape,yShape when xShape.Length=0 && yShape.Length=0  -> 
                let rValue =  (Array.map2 (execute f) xValue yValue)
                {JType=rType;JShape=xShape;JValue=unionCase rValue}
            //x is scalar, y is n-dimensional array
            |xShape,yShape when xShape.Length=0 && yShape.Length<>0-> 
                let xScalarValue = unbox (xValue.GetValue 0)
                let rValue = Array.map (fun x -> (execute f) xScalarValue x) yValue
                {JType=rType;JShape=yShape;JValue=unionCase rValue}
            //x is n-dimensional array, y is scalar
            |xShape,yShape when xShape.Length<>0 && yShape.Length=0-> 
                let yScalarValue = unbox (yValue.GetValue 0)
                let rValue = Array.map (fun x -> (execute f) x yScalarValue) xValue
                {JType=rType;JShape=xShape;JValue = unionCase rValue}
            //x and y are n-dimensional arrays of equal dimensions
            |xShape,yShape when xShape=yShape -> 
                let rValue = Array.map2 (execute f) xValue yValue
                {JType=rType;JShape=xShape;JValue=unionCase rValue}
            //The shape of x is the same as the shape of the first part of y - Agreement
            |xShape,yShape when (xShape.Length < yShape.Length) && xShape = (ArrayTake xShape.Length 0 yShape) -> 
                let rest = Array.sub yShape xShape.Length (yShape.Length- xShape.Length)
                let frameSize = Array.fold (*) 1 rest
                let g ix y =
                    let x = unbox (xValue.GetValue (ix/frameSize))
                    execute f x y 
                let rValue = Array.mapi g yValue
                {JType=rType;JShape=yShape;JValue = unionCase rValue}
            //The shape of the first part of x is the same as the shape of y - Agreement
            |xShape,yShape when (xShape.Length > yShape.Length) && (ArrayTake yShape.Length 0 xShape) = yShape -> 
                let rest = Array.sub xShape yShape.Length (xShape.Length- yShape.Length)
                let frameSize = Array.fold (*) 1 rest
                let g ix x =
                    let y = unbox (yValue.GetValue (ix/frameSize))
                    execute f x y 
                let rValue = Array.mapi g xValue
                {JType=rType;JShape=xShape;JValue = unionCase rValue}
            //x and y are n-dimensional arrays of different dimensions
            |_,_ -> 
                raise JExceptionLengthError
        match xNoun.JValue,yNoun.JValue,uBFD with
        |JTypeBooleanArray xValue,JTypeBooleanArray yValue, JBasicFunctionDyadicBoolean f ->
            func f xValue yValue JTBType.JTypeBoolean JTypeBooleanArray
        |JTypeIntegerArray xValue,JTypeIntegerArray yValue, JBasicFunctionDyadicInteger f ->
            func f xValue yValue JTBType.JTypeInteger JTypeIntegerArray 
        |JTypeIntegerArray xValue,JTypeIntegerArray yValue, JBasicFunctionDyadicFloat f ->
            func f (JTypeIntegerToJTypeFloat xValue) (JTypeIntegerToJTypeFloat yValue) JTBType.JTypeFloat JTypeFloatArray 
        |JTypeIntegerArray xValue,JTypeIntegerArray yValue, JBasicFunctionDyadicIntegerToBoolean f ->
            func f xValue yValue JTBType.JTypeBoolean JTypeBooleanArray 
        |JTypeFloatArray xValue,JTypeFloatArray yValue, JBasicFunctionDyadicFloat f ->
            func f xValue yValue JTBType.JTypeFloat JTypeFloatArray
        |JTypeFloatArray xValue,JTypeIntegerArray yValue, JBasicFunctionDyadicFloat f ->
            func f xValue (JTypeIntegerToJTypeFloat yValue) JTBType.JTypeFloat JTypeFloatArray
        |JTypeIntegerArray xValue,JTypeFloatArray yValue, JBasicFunctionDyadicFloat f ->
            func f (JTypeIntegerToJTypeFloat xValue) yValue JTBType.JTypeFloat JTypeFloatArray
        |JTypeFloatArray xValue,JTypeFloatArray yValue, JBasicFunctionDyadicFloatToBoolean f ->
            func f xValue yValue JTBType.JTypeBoolean JTypeBooleanArray
        |JTypeUnicodeArray xValue,JTypeUnicodeArray yValue, JBasicFunctionDyadicUnicodeToBoolean f ->
            func f xValue yValue JTBType.JTypeBoolean JTypeBooleanArray
        |JTypeBoxedArray xValue,JTypeBoxedArray yValue, JBasicFunctionDyadicBoxedToBoolean f ->
            func f xValue yValue JTBType.JTypeBoolean JTypeBooleanArray
        |_->
            raise JExceptionDomainError


    let JAdd xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JAddBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicInteger (+)
            JScalarFunctionDyadic JAddBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JAddBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloat (+)
            JScalarFunctionDyadic JAddBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JSubtract xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JSubtractBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicInteger (-)
            JScalarFunctionDyadic JSubtractBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JSubtractBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloat (-)
            JScalarFunctionDyadic JSubtractBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JTimes xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JTimesBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicInteger (*)
            JScalarFunctionDyadic JTimesBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JTimesBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloat (*)
            JScalarFunctionDyadic JTimesBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JDivide xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JDivideBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloat (/)
            JScalarFunctionDyadic JDivideBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JPower xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JPowerBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloat (fun x y -> x ** y)
            JScalarFunctionDyadic JPowerBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError



    let JMin xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JMinBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicInteger (min)
            JScalarFunctionDyadic JMinBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JMinBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloat (min)
            JScalarFunctionDyadic JMinBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JMax xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JMaxBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicInteger (max)
            JScalarFunctionDyadic JMaxBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JMaxBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloat (max)
            JScalarFunctionDyadic JMaxBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError
                        
    let JAnd xNoun yNoun =
        let JAndBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoolean (&&)
        JScalarFunctionDyadic JAndBasic xNoun yNoun

    let JOr xNoun yNoun =
        let JOrBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoolean (||)
        JScalarFunctionDyadic JOrBasic xNoun yNoun

    let JEqual xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicIntegerToBoolean (=)
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloatToBoolean (=)
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |JTBType.JTypeUnicode _,JTBType.JTypeUnicode -> 
            let compareUnicode (x:JTypeUnicode) (y:JTypeUnicode) =
                let sx = System.String [|x|]
                let sy = System.String [|y|]
                let a = System.String.Compare(sx, sy, System.StringComparison.CurrentCultureIgnoreCase)
                a = 0
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicUnicodeToBoolean compareUnicode
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |JTBType.JTypeBoolean _,JTBType.JTypeBoolean -> 
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoolean (=)
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |JTBType.JTypeBoxed _,JTBType.JTypeBoxed -> 
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoxedToBoolean (=)
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JNotEqual xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JNotEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicIntegerToBoolean (<>)
            JScalarFunctionDyadic JNotEqualBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JNotEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloatToBoolean (<>)
            JScalarFunctionDyadic JNotEqualBasic xNoun yNoun
        |JTBType.JTypeUnicode _,JTBType.JTypeUnicode -> 
            let compareUnicode (x:JTypeUnicode) (y:JTypeUnicode) =
                let sx = System.String [|x|]
                let sy = System.String [|y|]
                let a = System.String.Compare(sx, sy, System.StringComparison.CurrentCultureIgnoreCase)
                a <> 0
            let JNotEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicUnicodeToBoolean compareUnicode
            JScalarFunctionDyadic JNotEqualBasic xNoun yNoun
        |JTBType.JTypeBoolean _,JTBType.JTypeBoolean -> 
            let JNotEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoolean (<>)
            JScalarFunctionDyadic JNotEqualBasic xNoun yNoun
        |JTBType.JTypeBoxed _,JTBType.JTypeBoxed -> 
            let JNotEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoxedToBoolean (<>)
            JScalarFunctionDyadic JNotEqualBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JLessThan xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicIntegerToBoolean (<)
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloatToBoolean (<)
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |JTBType.JTypeUnicode _,JTBType.JTypeUnicode -> 
            let compareUnicode (x:JTypeUnicode) (y:JTypeUnicode) =
                let sx = System.String [|x|]
                let sy = System.String [|y|]
                let a = System.String.Compare(sx, sy, System.StringComparison.CurrentCultureIgnoreCase)
                a < 0
            let JLessThanBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicUnicodeToBoolean compareUnicode
            JScalarFunctionDyadic JLessThanBasic xNoun yNoun
        |JTBType.JTypeBoolean _,JTBType.JTypeBoolean -> 
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoolean (<)
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JLargerThan xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicIntegerToBoolean (>)
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloatToBoolean (>)
            JScalarFunctionDyadic JEqualBasic xNoun yNoun
        |JTBType.JTypeUnicode _,JTBType.JTypeUnicode -> 
            let compareUnicode (x:JTypeUnicode) (y:JTypeUnicode) =
                let sx = System.String [|x|]
                let sy = System.String [|y|]
                let a = System.String.Compare(sx, sy, System.StringComparison.CurrentCultureIgnoreCase)
                a > 0
            let JLargerThanBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicUnicodeToBoolean compareUnicode
            JScalarFunctionDyadic JLargerThanBasic xNoun yNoun
        |JTBType.JTypeBoolean _,JTBType.JTypeBoolean -> 
            let JLargerThanBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoolean (>)
            JScalarFunctionDyadic JLargerThanBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JLessOrEqual xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JLessOrEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicIntegerToBoolean (<=)
            JScalarFunctionDyadic JLessOrEqualBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JLessOrEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloatToBoolean (<=)
            JScalarFunctionDyadic JLessOrEqualBasic xNoun yNoun
        |JTBType.JTypeUnicode _,JTBType.JTypeUnicode -> 
            let compareUnicode (x:JTypeUnicode) (y:JTypeUnicode) =
                let sx = System.String [|x|]
                let sy = System.String [|y|]
                let a = System.String.Compare(sx, sy, System.StringComparison.CurrentCultureIgnoreCase)
                a <= 0
            let JLessOrEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicUnicodeToBoolean compareUnicode
            JScalarFunctionDyadic JLessOrEqualBasic xNoun yNoun
        |JTBType.JTypeBoolean _,JTBType.JTypeBoolean -> 
            let JLessOrEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoolean (<=)
            JScalarFunctionDyadic JLessOrEqualBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError

    let JLargerOrEqual xNoun yNoun =
        match xNoun.JType,yNoun.JType with
        |JTBType.JTypeInteger _,JTBType.JTypeInteger -> 
            let JLargerOrEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicIntegerToBoolean (>=)
            JScalarFunctionDyadic JLargerOrEqualBasic xNoun yNoun
        |(JTBType.JTypeInteger _|JTBType.JTypeFloat _),(JTBType.JTypeInteger|JTBType.JTypeFloat _) ->
            let JLargerOrEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicFloatToBoolean (>=)
            JScalarFunctionDyadic JLargerOrEqualBasic xNoun yNoun
        |JTBType.JTypeUnicode _,JTBType.JTypeUnicode -> 
            let compareUnicode (x:JTypeUnicode) (y:JTypeUnicode) =
                let sx = System.String [|x|]
                let sy = System.String [|y|]
                let a = System.String.Compare(sx, sy, System.StringComparison.CurrentCultureIgnoreCase)
                a >= 0
            let JLargerOrEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicUnicodeToBoolean compareUnicode
            JScalarFunctionDyadic JLargerOrEqualBasic xNoun yNoun
        |JTBType.JTypeBoolean _,JTBType.JTypeBoolean -> 
            let JLargerOrEqualBasic : JTypeBasicFunctionDyadicDU = JBasicFunctionDyadicBoolean (>=)
            JScalarFunctionDyadic JLargerOrEqualBasic xNoun yNoun
        |_ ->
            raise JExceptionDomainError


    let rec JTake (xNoun:JNoun) (yNoun:JNoun) =
        match xNoun.JType,xNoun.JShape,xNoun.JValue with
        |xType, xShape,JTypeIntegerArray xValue ->
            let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
            let xTuple:JTBType*JTypeInt[]*JTypeInt[] = (xType,xShape,xValueInt)
            //Recursive helper function. Called for each type combination below.
            let rec TakeDyadicGeneric (xTuple:JTBType*JTypeInt[]*JTypeInt []) (yTuple:JTBType*JTypeInt array*'a array)  unionCase fill  =
                let (xType,xShape,xValue)=xTuple
                let (yType,yShape,yValue)=yTuple
                let rType = yType
                match xShape,yShape with
                |xShape,yShape when xValue.Length = 0 ->
                //x is empty vector, y is any noun.
                //Return y
                    yTuple
                |xShape,yShape when xShape.Length = 0 || xValue.Length = 1  -> 
                //x is scalar or one long vector
                    let x = unbox (xValue.GetValue 0)
                    match yShape.Length with
                    |0 ->
                    //x is scalar or one long vector and y is scalar
                    //Take zero or more from head or tail
                        let rValue = ArrayTake x fill yValue
                        let rShape = [|abs x|]
                        (rType,rShape,rValue)
                    |_ -> 
                    //x is scalar  or one long vector and y is n-dimensional array.
                    //Take a number of (n-1)-cells from the head or tail.
                        let yShapeItem = Array.sub yShape 1 (yShape.Length-1)
                        let frameSize = Array.fold (fun acc x -> acc * x) 1 yShapeItem
                        let rValue= ArrayTake (x*frameSize) fill yValue 
                        let rShape = 
                            Array.sub yShape 1 (yShape.Length-1)
                            |>Array.append  [|abs x|]
                        (rType,rShape,rValue)
                |xShape,yShape when xValue.Length <= yShape.Length || yShape.Length = 0  ->
                //x is non-empty vector. The length of x is less or equal to the rank of y or y is scalar
                    let x = unbox (xValue.GetValue 0)
                    match xValue.Length with
                    |1 ->
                    //x is non-empty vector. The length of x and the rank of y is equal and both 1 or y is scalar
                    //Take a number of cells from the head or tail
                        let rValue= ArrayTake x fill yValue 
                        let rShape = [|abs x|]
                        (rType,rShape,rValue)
                    |_ ->
                    //x is non-empty vector. The length of x and the rank of y is equal and greater than 1 or y is scalar
                    //Take a number of (n-1)-cells from the head or tail.
                    //Call TakeDyadicGeneric recursively on the (n-1)-cells
                        let xValueItem = Array.sub xValue 1 (xValue.Length-1)
                        let xShapeItem = [|xValueItem.Length|]
                        let xTupleItem = (xType,xShapeItem,xValueItem)
                        let yShapeModified =
                        //If y is scalar, treat it as a one long vector
                            match yShape.Length with
                            |0 ->
                                [|1|]
                            |_->
                                yShape  
                        let yShapeItem = Array.sub yShapeModified 1 (yShapeModified.Length-1)
                        let frameSize = Array.fold (fun acc x -> acc * x) 1 yShapeItem
                        let remainingFrames = ArrayTake (x*frameSize) fill yValue
                        let iter acc ix =
                            let yValueItem = Array.sub remainingFrames (ix*frameSize) frameSize
                            let yTupleItem = (yType,yShapeItem,yValueItem)
                            let (_,_,rValueItem) = TakeDyadicGeneric xTupleItem yTupleItem unionCase fill
                            Array.append acc rValueItem
                        let rValue = Array.fold iter [||] [|0 .. ((abs x)-1)|]
                        let rShape = 
                            let part1 = Array.map abs xValue
                            let part2 = 
                                match yShape.Length with
                                    |0 ->
                                    //y is scalar.
                                        [||]
                                    |_ ->
                                        Array.sub yShapeModified (xValue.Length-1) (yShapeModified.Length - xValue.Length)
                            Array.append part1 part2
                        (rType,rShape,rValue)
                |_ -> 
                    raise JExceptionRankError
            match yNoun.JType,yNoun.JShape,yNoun.JValue with
            |yType, yShape,JTypeBoxedArray yValue ->
                let (rType,rShape,rValue) = TakeDyadicGeneric xTuple (yType,yShape,yValue) JTypeBoxedArray ZeroNounConstant
                {JType=rType;JShape=rShape;JValue=JTypeBoxedArray rValue}
            |yType, yShape,JTypeBooleanArray yValue ->
                let (rType,rShape,rValue) = TakeDyadicGeneric xTuple (yType,yShape,yValue) JTypeBooleanArray false
                {JType=rType;JShape=rShape;JValue=JTypeBooleanArray rValue}
            |yType, yShape,JTypeIntegerArray yValue ->
                let (rType,rShape,rValue) = TakeDyadicGeneric xTuple (yType,yShape,yValue) JTypeIntegerArray 0L
                {JType=rType;JShape=rShape;JValue=JTypeIntegerArray rValue}
            |yType, yShape,JTypeFloatArray yValue ->
                let (rType,rShape,rValue) = TakeDyadicGeneric xTuple (yType,yShape,yValue) JTypeFloatArray 0.0
                {JType=rType;JShape=rShape;JValue=JTypeFloatArray rValue}
            |yType, yShape,JTypeUnicodeArray yValue ->
                let (rType,rShape,rValue) = TakeDyadicGeneric xTuple (yType,yShape,yValue) JTypeUnicodeArray ' '
                {JType=rType;JShape=rShape;JValue=JTypeUnicodeArray rValue}

        |_ -> 
            raise  JExceptionDomainError


    let rec RankMonadicBox (uVerb:JVerbMonadic) rank xNoun: JNoun =
        //Applies uVerb to k-cells of xNoun.
        //Creates a boxed array of nouns with the results.
        //These results can also be boxed.
        //The second layer of nouns are unboxed and catenated in RankMonadicOpen
        //Rank is the min of function rank and argument rank.
        let xShape=xNoun.JShape
        //Rank in the call to the J verb is the min of functionRank and x rank
        let xRank = xShape.Length
        //If there are no items innerShape contains zero
        //frameSize is then zero and xNounItem containts an empty vector
        let innerShape = Array.sub xShape (xRank-rank) rank
        let frameSize = Array.fold (*) 1 innerShape
        let outerShape = Array.sub xShape 0 (xRank-rank)
        let nValues = max 1 (Array.fold (*) 1 outerShape)
        //This function is generic and called for the different
        //types of the argument
        let accumulateResults xType xValue unionCase =
            //This function is executed inside the array fold
            let rValue = Array.zeroCreate nValues
            let frameIterator ix =
                let xValueItem = Array.sub xValue (ix*frameSize) frameSize
                let xNounItem = {JType=xType;JShape=innerShape;JValue=unionCase xValueItem} 
                let rNounItem = uVerb xNounItem
                rValue.[ix] <- rNounItem
            //Accumulate the values to the boxed array of boxed arrays.
            for ix = 0 to nValues - 1 do
                frameIterator ix
            {JType=JTBType.JTypeBoxed;JShape=outerShape;JValue=JTypeBoxedArray rValue}
        //Handle the different argument types
        match xNoun.JType,xNoun.JValue with
        |xType,JTypeBoxedArray xValue ->
            accumulateResults xType xValue JTypeBoxedArray
        |xType,JTypeBooleanArray xValue ->
            accumulateResults xType xValue JTypeBooleanArray
        |xType,JTypeIntegerArray xValue ->
            accumulateResults xType xValue JTypeIntegerArray
        |xType,JTypeFloatArray xValue ->
            accumulateResults xType xValue JTypeFloatArray
        |xType,JTypeUnicodeArray xValue ->
            accumulateResults xType xValue JTypeUnicodeArray


    let rec RankDyadicBox (uVerb:JVerbDyadic) rank xNoun yNoun: JNoun =
        //Applies uVerb to k-cells of xNoun and yNoun.
        //Creates a boxed array of nouns with the results.
        //These results can also be boxed.
        //The second layer of nouns are unboxed and catenated in RankMonadicOpen
        //Rank is the min of function rank and argument rank.
        let xShape,yShape=xNoun.JShape,yNoun.JShape
        //Rank in the call to the J verb is the min of functionRank and x rank
        let xRank,yRank = xShape.Length,yShape.Length
        //If there are no items innerShape contains zero
        //frameSize is then zero and xNounItem containts an empty vector
        let xOperationRank,yOperationRank =
            match rank with
            |[|xOperationRank;yOperationRank|] ->
                xOperationRank,yOperationRank
            |_ ->
                raise JExceptionLengthError
         //x       
        let xInnerShape = Array.sub xShape (xRank-xOperationRank) xOperationRank
        let xFrameSize = Array.fold (*) 1 xInnerShape
        let xOuterShape = Array.sub xShape 0 (xRank-xOperationRank)
        let xNValues = max 1 (Array.fold (*) 1 xOuterShape)
        //y
        let yInnerShape = Array.sub yShape (yRank-yOperationRank) yOperationRank
        let yFrameSize = Array.fold (*) 1 yInnerShape
        let yOuterShape = Array.sub yShape 0 (yRank-yOperationRank)
        let yNValues = max 1 (Array.fold (*) 1 yOuterShape)
        //
        let nValues,outerShape =
            match xOuterShape,yOuterShape with
            |x,y when x.Length < y.Length ->
                yNValues,yOuterShape
            |x,y when x.Length >= y.Length ->
                xNValues,xOuterShape
            |_ ->
                raise JExceptionSystemError
        match xNValues,yNValues with
        |xNValues,yNValues when xNValues=yNValues || xNValues = 1 || yNValues = 1 ->
            let accumulateResults xTuple yTuple  outherShape xUnionCase yUnionCase =
                //This function is executed inside the array fold
                let (xType,xShape,xValue) = xTuple
                let (yType,yShape,yValue) = yTuple
                let rValue = Array.zeroCreate nValues
                match xNValues,yNValues with
                |1,_ ->
                    let xValueItem = Array.sub xValue 0 xFrameSize
                    let xNounItem = {JType=xType;JShape=xInnerShape;JValue=xUnionCase xValueItem} 
                    let frameIterator i xNounItem =
                        let yIndex = i % yNValues
                        let yValueItem = Array.sub yValue (yIndex*yFrameSize) yFrameSize
                        let yNounItem = {JType=yType;JShape=yInnerShape;JValue=yUnionCase yValueItem} 
                        let rNounItem = uVerb xNounItem yNounItem
                        rValue.[i] <- rNounItem
                    //Accumulate the values to the boxed array of boxed arrays.
                    for i = 0 to nValues - 1 do
                        frameIterator i xNounItem
                    {JType=JTBType.JTypeBoxed;JShape=outerShape;JValue=JTypeBoxedArray rValue}
                |_,1 ->
                    let yValueItem = Array.sub yValue 0 yFrameSize
                    let yNounItem = {JType=yType;JShape=yInnerShape;JValue=yUnionCase yValueItem} 
                    let frameIterator i yNounItem =
                        let xIndex = i % xNValues
                        let xValueItem = Array.sub xValue (xIndex*xFrameSize) xFrameSize
                        let xNounItem = {JType=xType;JShape=xInnerShape;JValue=xUnionCase xValueItem} 
                        let rNounItem = uVerb xNounItem yNounItem
                        rValue.[i] <- rNounItem
                    //Accumulate the values to the boxed array of boxed arrays.
                    for i = 0 to nValues - 1 do
                        frameIterator i yNounItem
                    {JType=JTBType.JTypeBoxed;JShape=outerShape;JValue=JTypeBoxedArray rValue}
                |_ ->
                    let frameIterator i =
                        let xIndex = i % xNValues
                        let xValueItem = Array.sub xValue (xIndex*xFrameSize) xFrameSize
                        let xNounItem = {JType=xType;JShape=xInnerShape;JValue=xUnionCase xValueItem} 
                        let yIndex = i % yNValues
                        let yValueItem = Array.sub yValue (yIndex*yFrameSize) yFrameSize
                        let yNounItem = {JType=yType;JShape=yInnerShape;JValue=yUnionCase yValueItem} 
                        let rNounItem = uVerb xNounItem yNounItem
                        rValue.[i] <- rNounItem
                    //Accumulate the values to the boxed array of boxed arrays.
                    for i = 0 to nValues - 1 do
                        frameIterator i
                    {JType=JTBType.JTypeBoxed;JShape=outerShape;JValue=JTypeBoxedArray rValue}
            //Handle the different argument types
            let yCombinator (xType,xShape,xValue)  outerShape xUnionType =
                match yNoun.JType,yNoun.JShape,yNoun.JValue with
                |yType,yShape,JTypeBoxedArray yValue ->
                     accumulateResults (xType,xShape,xValue) (yType,yShape,yValue)  outerShape xUnionType JTypeBoxedArray
                |yType,yShape,JTypeBooleanArray yValue ->
                     accumulateResults (xType,xShape,xValue) (yType,yShape,yValue)  outerShape xUnionType JTypeBooleanArray
                |yType,yShape,JTypeIntegerArray yValue ->
                     accumulateResults (xType,xShape,xValue) (yType,yShape,yValue)  outerShape xUnionType JTypeIntegerArray
                |yType,yShape,JTypeFloatArray yValue ->
                     accumulateResults (xType,xShape,xValue) (yType,yShape,yValue)  outerShape xUnionType JTypeFloatArray
                |yType,yShape,JTypeUnicodeArray yValue ->
                     accumulateResults (xType,xShape,xValue) (yType,yShape,yValue)  outerShape xUnionType JTypeUnicodeArray
            match xNoun.JType,xNoun.JShape,xNoun.JValue with
            //Boxed
            |xType,xShape,JTypeBoxedArray xValue ->
                yCombinator (xType,xShape,xValue)  outerShape JTypeBoxedArray
            //Boolean
            |xType,xShape,JTypeBooleanArray xValue ->
                yCombinator (xType,xShape,xValue) outerShape JTypeBooleanArray
            //Integer
            |xType,xShape,JTypeIntegerArray xValue ->
                yCombinator (xType,xShape,xValue)  outerShape JTypeIntegerArray
            //Float
            |xType,xShape,JTypeFloatArray xValue->
                yCombinator (xType,xShape,xValue)  outerShape JTypeFloatArray
            //Unicode
            |xType,xShape,JTypeUnicodeArray xValue ->
                yCombinator (xType,xShape,xValue)  outerShape JTypeUnicodeArray
        |_->
            raise JExceptionLengthError
            




    let RankMonadicOpen boxesOfNoun: JNoun =
        //The function unboxes and catenates boxed nouns
        match boxesOfNoun.JValue with
        |JTypeBoxedArray boxesOfNounValue ->
        //Calculate the max of each shape dimension in the resulting array of JTB.
            let resultRank = Array.fold (fun acc x -> max acc x.JShape.Length) 0 boxesOfNounValue
            let initArray = Array.create resultRank 0
            let maxResultShape acc x =
                    Array.map2 (max) (acc:JTypeInt[]) (ArrayTake resultRank 0 x.JShape)
            let maxShape = Array.fold maxResultShape initArray boxesOfNounValue
            let takeNounRValue = Array.map JTypeIntToJTypeInteger maxShape
            let takeNoun = {JType=JTBType.JTypeInteger;JShape=[|resultRank|];JValue=JTypeIntegerArray takeNounRValue}
        //The resulting frameSize. Empty maxShape gives a frameSize of 1, scalar.
        //Zero in one dimension gives a frameSize of 0.
            let rFrameSize = 
                maxShape
                |>Array.fold (fun acc x -> acc*x) 1
        //Binary OR of enum values to get the resulting type mixture
            let rType = Array.fold (fun acc x -> acc ||| x.JType ) JTBType.JTypeEmpty boxesOfNounValue
        //The resulting shape
            let rShape = Array.append boxesOfNoun.JShape maxShape
        //The result array length
            let rLength = 
                match rShape with
                |[||] ->
                    1
                |_ ->
                    Array.fold (*) 1 rShape
        //Helper function to write the results to the result array
            let inline setRange (rValue:^a array) (rIx: JTypeInt ref) (rValueItem:^a array) =
                let max = rValueItem.Length
                let mutable i = 0
                while i < max do
                    rValue.[!rIx] <- rValueItem.[i]
                    i <- i + 1
                    rIx := !rIx + 1
        //Pack up each JTB in the array of JTB
            match rType with
            //Boxed
            |JTBType.JTypeBoxed ->
                let (rValue:JTypeBoxed array) = Array.zeroCreate rLength
                let  rIx = ref 0
                let resultValue x =
                    let sItem = ArrayTake (-maxShape.Length) 1 x.JShape
                    let xItem = {JType=x.JType;JShape=sItem;JValue=x.JValue}
                    let rItem =
                        if xItem.JShape = maxShape then
                            xItem
                        else
                            JTake takeNoun xItem
                    match rItem.JValue with
                    |JTypeBoxedArray rValueItem -> 
                        setRange rValue rIx rValueItem
                    |_ ->
                        raise JExceptionDomainError
                Array.iter resultValue  boxesOfNounValue
                {JType=JTBType.JTypeBoxed;JShape=rShape;JValue=JTypeBoxedArray rValue}
            //Boolean
            |JTBType.JTypeBoolean ->
                let (rValue:JTypeBoolean array) = Array.zeroCreate rLength
                let  rIx = ref 0
                let resultValue x =
                    let sItem = ArrayTake (-maxShape.Length) 1 x.JShape
                    let xItem = {JType=x.JType;JShape=sItem;JValue=x.JValue}
                    let rItem = JTake takeNoun xItem
                    match rItem.JValue with
                    |JTypeBooleanArray rValueItem -> 
                        setRange rValue rIx rValueItem
                    |_ ->
                        raise JExceptionDomainError
                Array.iter resultValue  boxesOfNounValue
                {JType=rType;JShape=rShape;JValue=JTypeBooleanArray rValue}
            //Integer
            |JTBType.JTypeInteger ->
                let (rValue:JTypeInteger array) = Array.zeroCreate rLength
                let  rIx = ref 0
                let resultValue x =
                    let sItem = ArrayTake (-maxShape.Length) 1 x.JShape
                    let xItem = {JType=x.JType;JShape=sItem;JValue=x.JValue}
                    let rItem = JTake takeNoun xItem
                    match rItem.JValue with
                    |JTypeIntegerArray rValueItem -> 
                        setRange rValue rIx rValueItem
                    |_ ->
                        raise JExceptionDomainError
                Array.iter resultValue  boxesOfNounValue
                {JType=rType;JShape=rShape;JValue=JTypeIntegerArray rValue}
            //A mixture of integer and float is accumulated into a float array
            |n when n = (JTBType.JTypeInteger|||JTBType.JTypeFloat )->
                let (rValue:JTypeFloat array) = Array.zeroCreate rLength
                let  rIx = ref 0
                let resultValue x =
                    let sItem = ArrayTake (-maxShape.Length) 1 x.JShape
                    let xItem = {JType=x.JType;JShape=sItem;JValue=x.JValue}
                    let rItem = JTake takeNoun xItem
                    match rItem.JValue with
                    |JTypeIntegerArray rValueItem -> 
                        let rValueNew = JTypeIntegerToJTypeFloat rValueItem
                        setRange rValue rIx rValueNew
                    |JTypeFloatArray rValueItem -> 
                        setRange rValue rIx rValueItem
                    |_ ->
                        raise JExceptionDomainError
                Array.iter resultValue  boxesOfNounValue
                {JType=JTBType.JTypeFloat;JShape=rShape;JValue=JTypeFloatArray rValue}
            //Float
            |JTBType.JTypeFloat ->
                let (rValue:JTypeFloat array) = Array.zeroCreate rLength
                let  rIx = ref 0
                let resultValue x =
                    let sItem = ArrayTake (-maxShape.Length) 1 x.JShape
                    let xItem = {JType=x.JType;JShape=sItem;JValue=x.JValue}
                    let rItem = JTake takeNoun xItem
                    match rItem.JValue with
                    |JTypeFloatArray rValueItem -> 
                        setRange rValue rIx rValueItem
                    |_ ->
                        raise JExceptionDomainError
                Array.iter resultValue  boxesOfNounValue
                {JType=rType;JShape=rShape;JValue=JTypeFloatArray rValue}
            //Unicode
            |JTBType.JTypeUnicode ->
                let (rValue:JTypeUnicode array) = Array.zeroCreate rLength
                let  rIx = ref 0
                let resultValue x =
                    let sItem = ArrayTake (-maxShape.Length) 1 x.JShape
                    let xItem = {JType=x.JType;JShape=sItem;JValue=x.JValue}
                    let rItem = JTake takeNoun xItem
                    match rItem.JValue with
                    |JTypeUnicodeArray rValueItem -> 
                        setRange rValue rIx rValueItem
                    |_ ->
                        raise JExceptionDomainError
                Array.iter resultValue  boxesOfNounValue
                {JType=rType;JShape=rShape;JValue=JTypeUnicodeArray rValue}
            |_ ->
                raise JExceptionDomainError
        |_ ->
            //The array should always be boxed
            raise JExceptionSystemError

    let rec RankMonadicEmptyArraySpecialHandling uVerb rank xNoun outerShape (innerShape:JTypeInt array): JNoun =
        //If outershape contains zero, innershape does not contain zero or is empty.
        //Call the verb with a default element of shape innerShape
        //to get result shape and type
        //Append the resulting shape to outerShape
        let xShape=xNoun.JShape
        let applyVerb uVerb xType unionCase fill : JNoun=
            let rLength = 
                if innerShape.Length = 0 then
                    1
                else
                    Array.fold (*) 1 innerShape
            let xValueItem = Array.create rLength fill
            let defaultNoun = {JType=xType;JShape=innerShape;JValue= unionCase xValueItem}
            let {JType=rType;JShape=rShapeItem;JValue=rValue} = uVerb defaultNoun
            let rValue =
                match rValue with
                |JTypeBoxedArray _ ->
                    JTypeBoxedArray [||]
                |JTypeBooleanArray _ ->
                    JTypeBooleanArray [||]
                |JTypeIntegerArray _ ->
                    JTypeIntegerArray [||]
                |JTypeFloatArray _ ->
                    JTypeFloatArray [||]
                |JTypeUnicodeArray _ ->
                    JTypeUnicodeArray [||]
            let rShape =                  
                Array.append outerShape rShapeItem
            {JType=rType;JShape=rShape;JValue=rValue}
        match xNoun.JType,xNoun.JValue with
        |xType,JTypeBoxedArray _ ->
            applyVerb uVerb xType JTypeBoxedArray ZeroNounConstant
        |xType,JTypeBooleanArray _ ->
            applyVerb uVerb xType  JTypeBooleanArray false
        |xType,JTypeIntegerArray _ ->
            applyVerb uVerb xType JTypeIntegerArray 0L
        |xType,JTypeFloatArray _ ->
            applyVerb uVerb  xType JTypeFloatArray 0.0
        |xType,JTypeUnicodeArray _ ->
            applyVerb uVerb xType JTypeUnicodeArray ' '



    let rec RankDyadicEmptyArraySpecialHandling xSpecialHandling ySpecialHandling uVerb rank xNoun yNoun outerShape xInnerShape yInnerShape: JNoun =
        let xRank,yRank =
            match rank with
            |[|xRank;yRank|] 
                -> xRank,yRank
            |_ ->
                raise JExceptionSystemError
        let prepareArgument specialHandling argumentTuple rank (innerShape:JTypeInt array) unionCase fill =
            let (aType,aShape,aValue) = argumentTuple
            match specialHandling with 
            |true
                ->
                let aLength = 
                    if innerShape.Length = 0 then
                        1
                    else
                        Array.fold (*) 1 innerShape
                let aValueItem = Array.create aLength fill
                {JType=aType;JShape=innerShape;JValue= unionCase aValueItem}
            |false
                ->
                let rShape = Array.sub aShape (aShape.Length-rank) rank
                {JType=aType;JShape=rShape;JValue=unionCase aValue}
        let applyVerb uVerb xNoun yNoun  outerShape: JNoun=
            let {JType=rType;JShape=rShapeItem;JValue=rValueItem} = uVerb xNoun yNoun
            let rValue =
                match rValueItem with
                |JTypeBoxedArray _ ->
                    JTypeBoxedArray [||]
                |JTypeBooleanArray _ ->
                    JTypeBooleanArray [||]
                |JTypeIntegerArray _ ->
                    JTypeIntegerArray [||]
                |JTypeFloatArray _ ->
                    JTypeFloatArray [||]
                |JTypeUnicodeArray _ ->
                    JTypeUnicodeArray [||]
            let rShape =                  
                Array.append outerShape rShapeItem
            {JType=rType;JShape=rShape;JValue=rValue}
        let yCombinator xNounItem =
            match yNoun.JType,yNoun.JShape,yNoun.JValue with
            |yType,yShape,JTypeBoxedArray yValue ->
                let yNounItem = prepareArgument ySpecialHandling (yType,yShape,yValue) yRank yInnerShape JTypeBoxedArray IotaZeroConstant
                applyVerb uVerb xNounItem yNounItem  outerShape
            |yType,yShape,JTypeBooleanArray yValue ->
                let yNounItem = prepareArgument ySpecialHandling (yType,yShape,yValue) yRank yInnerShape JTypeBooleanArray false
                applyVerb uVerb xNounItem yNounItem outerShape
            |yType,yShape,JTypeIntegerArray yValue ->
                let yNounItem = prepareArgument ySpecialHandling (yType,yShape,yValue) yRank yInnerShape JTypeIntegerArray 0L
                applyVerb uVerb xNounItem yNounItem  outerShape
            |yType,yShape,JTypeFloatArray yValue ->
                let yNounItem = prepareArgument ySpecialHandling (yType,yShape,yValue) yRank yInnerShape JTypeFloatArray 0.0
                applyVerb uVerb xNounItem yNounItem  outerShape
            |yType,yShape,JTypeUnicodeArray yValue ->
                let yNounItem = prepareArgument ySpecialHandling (yType,yShape,yValue) yRank yInnerShape JTypeUnicodeArray ' '
                applyVerb uVerb xNounItem yNounItem  outerShape
        match xNoun.JType,xNoun.JShape,xNoun.JValue with
        //Boxed
        |xType,xShape,JTypeBoxedArray xValue ->
            let xNounItem = prepareArgument xSpecialHandling (xType,xShape,xValue) xRank xInnerShape JTypeBoxedArray IotaZeroConstant
            yCombinator xNounItem
        //Boolean
        |xType,xShape,JTypeBooleanArray xValue ->
            let xNounItem = prepareArgument xSpecialHandling (xType,xShape,xValue) xRank xInnerShape JTypeBooleanArray false 
            yCombinator xNounItem
        //Integer
        |xType,xShape,JTypeIntegerArray xValue ->
            let xNounItem = prepareArgument xSpecialHandling (xType,xShape,xValue) xRank xInnerShape JTypeIntegerArray 0L
            yCombinator xNounItem
        //Float
        |xType,xShape,JTypeFloatArray xValue ->
            let xNounItem = prepareArgument xSpecialHandling (xType,xShape,xValue) xRank xInnerShape JTypeFloatArray 0.0
            yCombinator xNounItem
        //Unicode
        |xType,xShape,JTypeUnicodeArray xValue ->
            let xNounItem = prepareArgument xSpecialHandling (xType,xShape,xValue) xRank xInnerShape JTypeUnicodeArray ' '
            yCombinator xNounItem
      

    let rec RankMonadic uVerb functionRank  xNoun : JNoun =
        let xShape=xNoun.JShape
        //Rank in the call to the J verb is the min of functionRank and x rank
        let xRank = xShape.Length
        let rank = min xRank functionRank
        //If outershape contains zero, there is nothing to operate on.
        //This is no problem if innershape contains zero. Then the corresponding empty array is used in the call.
        //If outershape contains zero and innershape does not, we need something to operate on to
        //determine type and shape of the empty result. We then generate a default object of the correct 
        //shape and type to operate on.
        let xShape = xNoun.JShape
        let innerShape = Array.sub xShape (xRank-rank) rank
        let outerShape = Array.sub xShape 0 (xRank-rank)
        match innerShape, outerShape with
        |innerShape, outerShape when (0=Array.fold (*) 1 outerShape) &&  (( 0<>Array.fold (*) 1 innerShape) || innerShape=[||]) ->
            RankMonadicEmptyArraySpecialHandling uVerb rank xNoun outerShape innerShape
        |_ -> 
            let boxesOfNoun = RankMonadicBox  uVerb rank xNoun
            RankMonadicOpen boxesOfNoun
            
            
    let rec RankDyadic uVerb functionRank xNoun yNoun: JNoun =
        let xShape,yShape=xNoun.JShape,yNoun.JShape
        //Rank in the call to the J verb is the min of functionRank and x rank
        let xyRank = [|xShape.Length;yShape.Length|]
        let rank = min xyRank functionRank
        // TODO: Duplicated code in RankDydic and RankDyadicBox because of planned restructuring.
        let xShape,yShape=xNoun.JShape,yNoun.JShape
        let xRank,yRank = xShape.Length,yShape.Length
        let xOperationRank,yOperationRank =
            match rank with
            |[|xOperationRank;yOperationRank|] ->
                xOperationRank,yOperationRank
            |_ ->
                raise JExceptionLengthError
         //x       
        let xInnerShape = Array.sub xShape (xRank-xOperationRank) xOperationRank
        //let xFrameSize = Array.fold (*) 1 xInnerShape
        let xOuterShape = Array.sub xShape 0 (xRank-xOperationRank)
        let xNValues = max 1 (Array.fold (*) 1 xOuterShape)
        //y
        let yInnerShape = Array.sub yShape (yRank-yOperationRank) yOperationRank
        //let yFrameSize = Array.fold (*) 1 yInnerShape
        let yOuterShape = Array.sub yShape 0 (yRank-yOperationRank)
        let yNValues = max 1 (Array.fold (*) 1 yOuterShape)
        //
        let nValues,outerShape =
            match xOuterShape,yOuterShape with
            |x,y when x.Length < y.Length ->
                yNValues,yOuterShape
            |x,y when x.Length >= y.Length ->
                xNValues,xOuterShape
            |_ ->
                raise JExceptionSystemError
        match xNValues,yNValues with
        |xNValues,yNValues when xNValues=yNValues || xNValues = 1 || yNValues = 1 ->
            //If outershape contains zero, there is nothing to operate on.
            //This is no problem if innershape contains zero. Then the corresponding empty array is used in the call.
            //If outershape contains zero and innershape does not, we need something to operate on to
            //determine type and shape of the empty result. We then generate a default object of the correct 
            //shape and type to operate on.
            let xSpecialHandling = ( 0<>Array.fold (*) 1 xInnerShape) || xInnerShape=[||]
            let ySpecialHandling = ( 0<>Array.fold (*) 1 yInnerShape) || yInnerShape=[||]
            match xInnerShape,yInnerShape,outerShape with
            |xInnerShape,yInnerShape,outerShape when (0=Array.fold (*) 1 outerShape) && ( xSpecialHandling || ySpecialHandling) ->
                RankDyadicEmptyArraySpecialHandling xSpecialHandling ySpecialHandling uVerb rank xNoun yNoun outerShape xInnerShape yInnerShape
            |_ -> 
                let boxesOfNoun = RankDyadicBox uVerb rank xNoun yNoun
                RankMonadicOpen boxesOfNoun
        |_->
            raise JExceptionLengthError

            


    let rec JRankMonadic yNoun uVerb  nNoun: JNoun  =
        //The Rank adverb u Rank n y (according to the J documentation this is a verb)
            match nNoun.JValue with
            |JTypeIntegerArray nValue -> 
                let nValueInt = Array.map JTypeIntegerToJTypeInt nValue
                match nNoun.JShape,yNoun.JShape with
                |nShape,yShape when nShape.Length = 0 -> 
                    let inRank = unbox (nValueInt.GetValue 0)
                    let rank = 
                        match inRank with
                        |n when n<0 ->
                            max 0 (yShape.Length + inRank)
                        |_ -> 
                            min yShape.Length inRank
                    RankMonadic  uVerb rank yNoun
                |_ -> 
                    raise JExceptionRankError
            |_ -> 
                raise JExceptionDomainError

    let rec JRankDyadic  xNoun yNoun uVerb nNoun: JNoun  =
        //The Rank adverb x u Rank n y (according to the J documentation this is a verb)
            match nNoun.JValue with
            |JTypeIntegerArray nValue -> 
                let nValueInt = Array.map JTypeIntegerToJTypeInt nValue
                match nNoun.JShape,xNoun.JShape,yNoun.JShape with
                |nShape,xShape,yShape when nShape.Length = 1 && nValue.Length = 2 -> 
                    let xyRank = [|xShape.Length;yShape.Length|]
                    let rankIter nRankItem xyRankItem = 
                        match nRankItem with
                        |n when n<0 ->
                            max 0 (xyRankItem + nRankItem)
                        |_ -> 
                            min xyRankItem nRankItem
                    let rank = Array.map2 rankIter nValueInt xyRank
                    RankDyadic uVerb rank xNoun yNoun
                |_ -> 
                    raise JExceptionRankError
            |_ -> 
                raise JExceptionDomainError


    let GenIota (yValue:JTypeInt array):JTypeInt array =
        //Implementation of the Iota-function in the program language J.
        //It generates sequences of consecutive non-negative integers sorted in different ways.
        //Right argument: Array of integers, one for each dimension in the result.
        //The absolute value of each integer is the size of the result in this dimension.
        //The sign of each integer is the sort order in this dimension.
        //The result is a one-dimensional array, the shape is stored separately.
        //The first cell in the vector has an index of all zeroes, the last an index
        //of the max index value in all dimensions.
        //As an example Iota 2 -2 would give 1 0 3 2
        //The function is complex because it is optimized.
        //It is an important building block of the J language.
        let yValueList = Array.toList yValue
        //The shape of the result.
        let rShape = List.map (abs) yValueList
        //Max index in each dimension.
        let maxIndex = List.map (fun x -> x-1) rShape
        //The total length and the framesize in each dimension.
        match (List.scanBack (*) rShape 1) with
        |rLength::_ when rLength=0 ->
            //One of the values is 0
            [||]
        |[1] ->
            //No values, empty vector
            [|0|]
        |rLength::frameSize -> 
            //The dimensions sorted in reverse order
            let reversedSort = List.map (fun x -> x < 0) yValueList
            //Conversion from index in the one-dimensional array to index in the
            //n-dimensional array.
            let rec DimIndex (fS:JTypeInt list) ix =
                match fS with
                |[] ->
                    raise JExceptionSystemError
                |[hd] ->
                    [ix]        
                |hd::tl ->
                    let quot = ix / hd
                    let rem = ix - quot * hd
                    quot::DimIndex tl rem
            //The value of a certain index in a certain dimension
            let inline DimIndexValue rS fS mI dI =
                if rS then
                    fS*(mI-dI)
                else
                    fS*dI
            //Calculate the min value of the row(in acc)
            //Fill this row with consecutive integers
            //sorted in ascending or descending order 
            //Move index to the next row.
            let rec FillArray acc rS fS mI dI (rArray:JTypeInt array) (ix:JTypeInt ref) =
                match rS,fS,mI,dI with
                |[rShd],[fShd],[mIhd],[dIhd] -> 
                    let mutable j = !ix
                    let dimIndexValue = DimIndexValue rShd fShd mIhd dIhd
                    let mutable value = acc + dimIndexValue
                    let step = if rShd then -1 else 1
                    while j <= (!ix + mIhd) do
                        rArray.SetValue(value,j)
                        value <- value + step
                        j <- j + 1
                    ix := !ix + mIhd + 1
                |rShd::rStl,fShd::fStl,mIhd::mItl,dIhd::dItl ->
                    let dimIndexValue = DimIndexValue rShd fShd mIhd dIhd
                    FillArray (acc+dimIndexValue) rStl fStl mItl dItl rArray ix
                |_ ->
                    raise JExceptionSystemError
            //Create empty array for the result.
            let rArray = Array.zeroCreate rLength
            let ix = ref 0 
            while !ix < rArray.Length do
                //Calculate the n-dimensional index.
                let dimIndex = DimIndex frameSize !ix
                //Fill the result array
                FillArray 0 reversedSort frameSize maxIndex dimIndex rArray ix
            rArray 
        |_->
            raise JExceptionSystemError
                            
    let rec JIotaMonadic xNoun : JNoun =
        match xNoun.JShape.Length with
        |xRank when xRank <= 1 ->
            match xNoun.JValue with
            |JTypeIntegerArray xValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                let rValueInt = GenIota xValueInt
                let rValue = Array.map JTypeIntToJTypeInteger rValueInt
                let rShape = Array.map (abs) xValueInt
                {JType=JTBType.JTypeInteger;JShape=rShape;JValue=JTypeIntegerArray rValue}
            |_ -> raise JExceptionDomainError
        |_ ->
            RankMonadic JIotaMonadic 1 xNoun


    let JBox (xNoun:JNoun) : JNoun =
        {JType = JTBType.JTypeBoxed; JShape = [||]; JValue = JTypeBoxedArray [|xNoun|] ; }

    let rec JOpen xNoun : JNoun =
        match xNoun.JValue with
        |JTypeBoxedArray xValue ->
            match xNoun.JShape with
            |xShape when 0 = Array.fold (*) 1 xShape ->
                {JType = JTBType.JTypeInteger; JShape = xShape; JValue = JTypeIntegerArray [||] ; }
            |[||] ->
                unbox (xValue.GetValue 0)
            |_ ->
                RankMonadic JOpen 0 xNoun
        |_ ->
            xNoun

            (*
    let rec JShape (xNoun:JNoun) (yNoun:JNoun) : JNoun =
        match xNoun.JShape with
        |([||]|[|_|])->
            let shapeFunction xValue yType yShape yValue unionCase fill =
                match xValue,yShape with
                |xValue,_ when 0 = Array.fold (*) 1 xValue ->
                    {JType = yType; JShape = xValue; JValue = unionCase [||] ; }
                |_,yShape when 0 = Array.fold (*) 1 yShape ->
                    raise JExceptionLengthError
                |[||],_ ->
                    let rValue = ArrayTake 1 fill yValue
                    {JType = yType; JShape = xValue; JValue = unionCase rValue ; }
                |_ ->
                    let rValue = RepeatArray (Array.fold (*) 1 xValue) yValue
                    {JType = yType; JShape = xValue; JValue = unionCase rValue ; }
            match xNoun.JValue,yNoun.JType,yNoun.JShape,yNoun.JValue with
            //Integer
            |JTypeIntegerArray xValue,yType,yShape,JTypeIntegerArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt  yType yShape yValue JTypeIntegerArray 0L
            //Float
            |JTypeIntegerArray xValue,yType,yShape,JTypeFloatArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt yType yShape yValue JTypeFloatArray 0.0
            //Boolean
            |JTypeIntegerArray xValue,yType,yShape,JTypeBooleanArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt yType yShape yValue  JTypeBooleanArray false
            //Unicode
            |JTypeIntegerArray xValue,yType,yShape,JTypeUnicodeArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt yType yShape yValue  JTypeUnicodeArray ' '
            //Boxed
            |JTypeIntegerArray xValue,yType,yShape,JTypeBoxedArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt yType yShape yValue JTypeBoxedArray ZeroNounConstant
            |_ ->
                raise JExceptionSystemError
        |_ ->
            let rank = [|1;yNoun.JShape.Length|]
            RankDyadic JShape rank xNoun yNoun

            *)
    let rec JShape (xNoun:JNoun) (yNoun:JNoun) : JNoun =
        match xNoun.JShape with
        |([||]|[|_|])->
            let shapeFunction xValue yType yShape yValue unionCase fill =
                match xValue,yShape with
                |xValue,_ when 0 = Array.fold (*) 1 xValue ->
                    {JType = yType; JShape = xValue; JValue = unionCase [||] ; }
                |_,yShape when 0 = Array.fold (*) 1 yShape ->
                    raise JExceptionLengthError
                |[||],_ ->
                    let rValue = ArrayTake 1 fill yValue
                    {JType = yType; JShape = xValue; JValue = unionCase rValue ; }
                |_,([|_|]|[||]) ->
                    let rValue = RepeatArray (Array.fold (*) 1 xValue) yValue
                    {JType = yType; JShape = xValue; JValue = unionCase rValue ; }
                |_ ->
                    let rShape = Array.append xValue (Array.sub yShape 1 (yShape.Length - 1))
                    let rValue = RepeatArray (Array.fold (*) 1 rShape) yValue
                    {JType = yType; JShape = rShape; JValue = unionCase rValue ; }
            match xNoun.JValue,yNoun.JType,yNoun.JShape,yNoun.JValue with
            //Integer
            |JTypeIntegerArray xValue,yType,yShape,JTypeIntegerArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt  yType yShape yValue JTypeIntegerArray 0L
            //Float
            |JTypeIntegerArray xValue,yType,yShape,JTypeFloatArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt yType yShape yValue JTypeFloatArray 0.0
            //Boolean
            |JTypeIntegerArray xValue,yType,yShape,JTypeBooleanArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt yType yShape yValue  JTypeBooleanArray false
            //Unicode
            |JTypeIntegerArray xValue,yType,yShape,JTypeUnicodeArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt yType yShape yValue  JTypeUnicodeArray ' '
            //Boxed
            |JTypeIntegerArray xValue,yType,yShape,JTypeBoxedArray yValue ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                shapeFunction xValueInt yType yShape yValue JTypeBoxedArray ZeroNounConstant
            |_ ->
                raise JExceptionSystemError
        |_ ->
            let rank = [|1;yNoun.JShape.Length|]
            RankDyadic JShape rank xNoun yNoun


                                        
                       

    let JShapeOf (yNoun:JNoun) : JNoun =
        let yShape = Array.map JTypeIntToJTypeInteger yNoun.JShape
        let rShape =
            match yShape with
            |[||] ->
                [|0|]
            |[|n|] ->
                [||]
            |_ ->
                [|yShape.Length|]
        {JType = JTBType.JTypeInteger; JShape = rShape; JValue = JTypeIntegerArray yShape ; } 

    let JRavel (yNoun:JNoun) :JNoun =
        match yNoun.JType,yNoun.JShape,yNoun.JValue with
        |yType,[||],yValueDU  ->
            let rShape = [|1|]
            {JType = yType; JShape = rShape; JValue = yValueDU ; }
        |yType,yShape,yValueDU ->
            let rShape = [|Array.fold (*) 1 yShape|]
            {JType = yType; JShape = rShape; JValue = yValueDU ; }

    let JTally (yNoun:JNoun) :JNoun =
        match yNoun.JType,yNoun.JShape,yNoun.JValue with
        |yType,[||],yValueDU  ->
            {JType = JTBType.JTypeInteger; JShape = [||]; JValue = JTypeIntegerArray [|1L|] ; }
        |yType,yShape,yValueDU ->
            let rTally = JTypeIntToJTypeInteger ( yShape.[0] )
            {JType = JTBType.JTypeInteger; JShape = [||]; JValue = JTypeIntegerArray [|rTally|] ; }

    let GetPartOfNounAsNoun (yNoun:JNoun) start number =
        match yNoun.JValue with
        |JTypeIntegerArray yValue ->
            let rValue = Array.sub yValue start number
            {JType = JTBType.JTypeInteger; JShape = [|number|]; JValue = JTypeIntegerArray rValue; }
        |JTypeFloatArray yValue ->
            let rValue = Array.sub yValue start number
            {JType = JTBType.JTypeFloat; JShape = [|number|]; JValue = JTypeFloatArray rValue; }
        |JTypeBooleanArray yValue ->
            let rValue = Array.sub yValue start number
            {JType = JTBType.JTypeBoolean; JShape = [|number|]; JValue = JTypeBooleanArray rValue; }
        |JTypeUnicodeArray yValue ->
            let rValue = Array.sub yValue start number
            {JType = JTBType.JTypeUnicode; JShape = [|number|]; JValue = JTypeUnicodeArray rValue; }
        |JTypeBoxedArray yValue ->
            let rValue = Array.sub yValue start number
            {JType = JTBType.JTypeBoxed; JShape = [|number|]; JValue = JTypeBoxedArray rValue; }

    let GetItemOfNounAsNoun (yNoun:JNoun) (item:int) =
        match yNoun.JValue with
        |JTypeIntegerArray yValue ->
            let rValue = [|unbox (yValue.GetValue item)|]
            {JType = JTBType.JTypeInteger; JShape = [||]; JValue = JTypeIntegerArray rValue; }
        |JTypeFloatArray yValue ->
            let rValue =  [|unbox (yValue.GetValue item)|]
            {JType = JTBType.JTypeFloat; JShape = [||]; JValue = JTypeFloatArray rValue; }
        |JTypeBooleanArray yValue ->
            let rValue =  [|unbox (yValue.GetValue item)|]
            {JType = JTBType.JTypeBoolean; JShape = [||]; JValue = JTypeBooleanArray rValue; }
        |JTypeUnicodeArray yValue ->
            let rValue =  [|unbox (yValue.GetValue item)|]
            {JType = JTBType.JTypeUnicode; JShape = [||]; JValue = JTypeUnicodeArray rValue; }
        |JTypeBoxedArray yValue ->
            let rValue = [|unbox (yValue.GetValue item)|]
            {JType = JTBType.JTypeBoxed; JShape = [||]; JValue = JTypeBoxedArray rValue; }


    let SetMutableScalarNounAsItemOfNoun (xNoun:JNoun) (yNoun:JNoun) (item:int) =
        match xNoun.JValue,yNoun.JValue with
        |JTypeIntegerArray xValue,JTypeIntegerArray yValue ->
            let xItem = xValue.GetValue 0
            yValue.SetValue( xItem, item)
        |JTypeFloatArray xValue,JTypeFloatArray yValue ->
            let xItem = xValue.GetValue 0
            yValue.SetValue( xItem, item)
        |JTypeBooleanArray xValue, JTypeBooleanArray yValue ->
            let xItem = xValue.GetValue 0
            yValue.SetValue( xItem, item)
        |JTypeUnicodeArray xValue,JTypeUnicodeArray yValue ->
            let xItem = xValue.GetValue 0
            yValue.SetValue( xItem, item)
        |JTypeBoxedArray xValue,JTypeBoxedArray yValue ->
            let xItem = xValue.GetValue 0
            yValue.SetValue( xItem, item)
        |_ ->
            raise JExceptionSystemError

    let SetMutableNounAsPartOfNoun (xNoun:JNoun) (yNoun:JNoun) index =
        let length = Array.fold (*) 1 xNoun.JShape
        let setMutablePart (xValue:'a array) (yValue:'a array) =
            let mutable i = 0
            while i < length do
                let xItem = xValue.GetValue i
                yValue.SetValue( xItem, (index + i))
                i <- i + 1
        match xNoun.JValue,yNoun.JValue with
        |JTypeIntegerArray xValue,JTypeIntegerArray yValue ->
            setMutablePart xValue yValue
        |JTypeFloatArray xValue,JTypeFloatArray yValue ->
            setMutablePart xValue yValue
        |JTypeBooleanArray xValue,JTypeBooleanArray yValue ->
            setMutablePart xValue yValue
        |JTypeUnicodeArray xValue,JTypeUnicodeArray yValue ->
            setMutablePart xValue yValue
        |JTypeBoxedArray xValue,JTypeBoxedArray yValue ->
            setMutablePart xValue yValue
        |_ ->
            raise JExceptionSystemError


    let ZeroCreateNounFromNoun (xNoun:JNoun) (shape:JTypeInt []) =
        let length = Array.fold (*) 1 shape
        match xNoun.JValue with
        |JTypeIntegerArray _ ->
            let rValue=Array.zeroCreate length
            {JType = JTBType.JTypeInteger; JShape = shape; JValue = JTypeIntegerArray rValue; }
        |JTypeFloatArray _ ->
            let rValue=Array.zeroCreate length
            {JType = JTBType.JTypeFloat; JShape = shape; JValue = JTypeFloatArray rValue; }
        |JTypeBooleanArray _ ->
            let rValue=Array.zeroCreate length
            {JType = JTBType.JTypeBoolean; JShape = shape; JValue = JTypeBooleanArray rValue; }
        |JTypeUnicodeArray _ ->
            let rValue=Array.zeroCreate length
            {JType = JTBType.JTypeUnicode; JShape = shape; JValue = JTypeUnicodeArray rValue; }
        |JTypeBoxedArray _ ->
            let rValue=Array.zeroCreate length
            {JType = JTBType.JTypeBoxed; JShape = shape; JValue = JTypeBoxedArray rValue; }



    let rec JScan (xNoun:JNoun) (yNoun:JNoun) (uVerb:JVerbDyadic) : JNoun =
        match xNoun.JShape,yNoun.JShape with
        |xShape,yShape ->
            match yShape with
            |[||] ->
                raise JExceptionRankError
            |[|yTally|] ->
                let rShape = Array.append [|yTally|] xShape
                let rFrameSize = Array.fold (*) 1 xShape
                let rNoun = ZeroCreateNounFromNoun xNoun rShape
                let startIndex = Array.init yTally id
                let f acc ix =
                    let yNounItem = GetItemOfNounAsNoun yNoun ix
                    let rNounItem = uVerb acc yNounItem
                    if rNounItem.JShape = xNoun.JShape then
                        SetMutableNounAsPartOfNoun rNounItem rNoun (ix*rFrameSize)
                        rNounItem
                    else
                        if rNounItem.JShape.Length <> xNoun.JShape.Length then
                            raise JExceptionRankError
                        else
                            raise JExceptionLengthError
                Array.fold f xNoun startIndex |>ignore
                rNoun
            |yShape ->
                let yShapeTail = Array.sub yShape 1 (yShape.Length - 1)
                let yTally = unbox (Array.get yShape 0)
                let yFrameSize = Array.fold (*) 1 yShapeTail
                let rFrameSize = Array.fold (*) 1 xShape
                let startIndex = Array.init yTally id
                let rShape = Array.append [|yTally|] xShape
                let rNoun = ZeroCreateNounFromNoun xNoun rShape
                let f acc ix =
                    let yNounItem = GetPartOfNounAsNoun yNoun (ix*yFrameSize) yFrameSize
                    let rNounItem = uVerb acc yNounItem
                    if rNounItem.JShape = xNoun.JShape then
                        SetMutableNounAsPartOfNoun rNounItem rNoun (ix*rFrameSize)
                        rNounItem
                    else
                        if rNounItem.JShape.Length <> xNoun.JShape.Length then
                            raise JExceptionRankError
                        else
                            raise JExceptionLengthError
                Array.fold f xNoun startIndex|>ignore
                rNoun




    let rec JDrop (xNoun:JNoun) (yNoun:JNoun) =
        let yToTakeFunction xToDrop yLength =
            match xToDrop with
            |xToDrop when xToDrop >= 0 ->
                - (max 0 (yLength - xToDrop))
            |xToDrop when xToDrop < 0 ->
                max 0 (yLength + xToDrop)
            |_ ->
                raise JExceptionSystemError
        match xNoun.JShape,xNoun.JValue,yNoun.JShape with
        |xShape,JTypeIntegerArray xValue,yShape ->
            let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
            match xShape,xValueInt with
            |[||],[|xToDrop|] ->
                match yShape with
                [||] ->
                    {JType = JTBType.JTypeInteger; JShape = xShape; JValue = JTypeIntegerArray [||] ; }                    
                |[|yLength|] ->
                    let yToTake = JTypeIntToJTypeInteger (yToTakeFunction xToDrop yLength)
                    let yToTakeNoun = {JType = JTBType.JTypeInteger; JShape = [||]; JValue = JTypeIntegerArray [|yToTake|] ; }
                    JTake yToTakeNoun yNoun
                |_ ->
                    let yLength = unbox (yShape.GetValue 0)
                    let yToTake = JTypeIntToJTypeInteger (yToTakeFunction xToDrop yLength)
                    let yToTakeNoun = {JType = JTBType.JTypeInteger; JShape = [||]; JValue = JTypeIntegerArray [|yToTake|] ; }
                    JTake yToTakeNoun yNoun
            |[|0|],_ ->
                yNoun
            |[|xLength|],_ ->
                match yShape with
                |[||] ->
                    {JType = JTBType.JTypeInteger; JShape = xShape; JValue = JTypeIntegerArray [||] ; }
                |_->
                    let yShapeLength = yShape.Length
                    if xLength > yShapeLength then
                        raise JExceptionLengthError
                    else
                        let yShapeCut = Array.sub yShape 0 xLength
                        let yToTake = Array.map JTypeIntToJTypeInteger (Array.map2 yToTakeFunction xValueInt yShapeCut)
                        let yToTakeNoun = {JType = JTBType.JTypeInteger; JShape = [|xLength|]; JValue = JTypeIntegerArray yToTake ; }
                        JTake yToTakeNoun yNoun
            |_ ->
                raise JExceptionLengthError
        |_ ->
            raise JExceptionDomainError



    let rec JFold (xNoun:JNoun) (yNoun:JNoun) (uVerb:JVerbDyadic) : JNoun =
        match xNoun.JShape,yNoun.JShape with
        |xShape,yShape ->
            match yShape with
            |[||] ->
                raise JExceptionRankError
            |[|n|] ->
                match xShape with
                |[||] ->
                    let startIndex = Array.init n id
                    let f acc ix =
                        let yNounItem = GetItemOfNounAsNoun yNoun ix
                        uVerb acc yNounItem
                    Array.fold f xNoun startIndex
                |_ ->
                    raise JExceptionLengthError
            |yShape when 0 = Array.fold (*) 1 yShape ->
                xNoun
            |yShape ->
                let yShapeTail = Array.sub yShape 1 (yShape.Length - 1)
                let yShapeTailJInteger = Array.map JTypeIntToJTypeInteger yShapeTail
                let yShapeTailNoun = {JType = JTBType.JTypeInteger; JShape = [||]; JValue = JTypeIntegerArray yShapeTailJInteger ; }
                let nValues = unbox (Array.get yShape 0)
                let frameSize = Array.fold (+) 0 yShapeTail
                let startIndex = Array.init nValues (fun x -> x*frameSize)
                let f acc ix =
                    let yNounItem = GetPartOfNounAsNoun yNoun ix frameSize
                    uVerb acc (JShape yShapeTailNoun yNounItem)
                Array.fold f xNoun startIndex
            
    let JCatenate xNoun yNoun: JNoun =
        let catenate xNoun yNoun:JNoun =   
            let f (xType,(xShape:JTypeInt array),xValue) (yType,(yShape:JTypeInt array),yValue) unionCase =
                match xShape,yShape with
                |([||],[||] | [||],[|_|] | [|_|],[||] | [|_|],[|_|] ) ->
                    let rType = xType
                    let rValue = Array.append xValue yValue
                    let rShape = [|rValue.Length|]
                    {JType = rType; JShape = rShape; JValue = unionCase rValue ; }
                |_,_ ->
                    let rType = xType
                    let length = (unbox (xShape.GetValue 0)) + (unbox (yShape.GetValue 0))
                    let rest = Array.sub xShape 1 (xShape.Length - 1)
                    let rShape = Array.append [|length|] rest
                    let rValue = Array.append xValue yValue   
                    {JType = rType; JShape = rShape; JValue = unionCase rValue ; } 
            //Handle the different argument types
            match xNoun.JType,xNoun.JShape,xNoun.JValue,yNoun.JType,yNoun.JShape,yNoun.JValue with
            //Boxed
            |xType,xShape,JTypeBoxedArray xValue,yType,yShape,JTypeBoxedArray yValue ->
                f (xType,xShape,xValue) (yType,yShape,yValue)  JTypeBoxedArray
            //Boolean
            |xType,xShape,JTypeBooleanArray xValue,yType,yShape,JTypeBooleanArray yValue ->
                f (xType,xShape,xValue) (yType,yShape,yValue)  JTypeBooleanArray
            //Integer
            |xType,xShape,JTypeIntegerArray xValue,yType,yShape,JTypeIntegerArray yValue ->
                f (xType,xShape,xValue) (yType,yShape,yValue)  JTypeIntegerArray
            //Float
            |xType,xShape,JTypeFloatArray xValue,yType,yShape,JTypeFloatArray yValue ->
                f (xType,xShape,xValue) (yType,yShape,yValue)  JTypeFloatArray
            //Unicode
            |xType,xShape,JTypeUnicodeArray xValue,yType,yShape,JTypeUnicodeArray yValue ->
                f (xType,xShape,xValue) (yType,yShape,yValue)  JTypeUnicodeArray
            |_->
                raise JExceptionDomainError
        match xNoun.JShape,yNoun.JShape with
        |([||],[||] | [||],[|_|] | [|_|],[||] | [|_|],[|_|] ) ->
            catenate xNoun yNoun
        |[||],_ ->
            let rest = JDrop OneNounConstant (JShapeOf yNoun)
            let shape = catenate OneNounConstant rest
            let x = JShape shape xNoun
            catenate x yNoun
        |_,[||] ->
            let rest = JDrop OneNounConstant (JShapeOf xNoun)
            let shape = catenate OneNounConstant rest
            let y = JShape shape yNoun
            catenate xNoun y
        |xShape,yShape when xShape.Length = yShape.Length ->
            let rShapeLength = xShape.Length
            let rest = Array.tail (Array.map2 (max) xShape yShape)
            let xDim0 = unbox (xShape.GetValue 0)
            let yDim0 = unbox (yShape.GetValue 0)
            let xShapeItem = Array.map JTypeIntToJTypeInteger (Array. append [|xDim0|] rest)
            let yShapeItem = Array.map JTypeIntToJTypeInteger (Array. append [|yDim0|] rest)
            let xShapeItemNoun = {JType = JTBType.JTypeInteger; JShape = [|rShapeLength|]; JValue = JTypeIntegerArray xShapeItem ; }
            let yShapeItemNoun = {JType = JTBType.JTypeInteger; JShape = [|rShapeLength|]; JValue = JTypeIntegerArray yShapeItem ; }
            let xNounItem = JTake xShapeItemNoun xNoun
            let yNounItem = JTake yShapeItemNoun yNoun
            catenate xNounItem yNounItem
        |xShape,yShape when xShape.Length < yShape.Length ->
            let rShapeLength = max xShape.Length yShape.Length
            let x = ArrayTake (-rShapeLength) 0 xShape
            let y = ArrayTake (-rShapeLength) 0 yShape
            let rest = Array.tail (Array.map2 (max) x y)
            let xDim0 = 1
            let yDim0 = unbox (yShape.GetValue 0)
            let x = Array.map JTypeIntToJTypeInteger (ArrayTake (-rShapeLength) 1 xShape)
            let xShapeItemNoun = {JType = JTBType.JTypeInteger; JShape = [|rShapeLength|]; JValue = JTypeIntegerArray x ; }
            let xNounItemReshaped = JShape xShapeItemNoun xNoun
            let xShapeItem = Array.map JTypeIntToJTypeInteger (Array. append [|xDim0|] rest)
            let yShapeItem = Array.map JTypeIntToJTypeInteger (Array. append [|yDim0|] rest)
            let xShapeItemNoun = {JType = JTBType.JTypeInteger; JShape = [|rShapeLength|]; JValue = JTypeIntegerArray xShapeItem ; }
            let yShapeItemNoun = {JType = JTBType.JTypeInteger; JShape = [|rShapeLength|]; JValue = JTypeIntegerArray yShapeItem ; }
            let xNounItem = JTake xShapeItemNoun xNounItemReshaped
            let yNounItem = JTake yShapeItemNoun yNoun
            catenate xNounItem yNounItem
        |xShape,yShape when xShape.Length > yShape.Length ->
            let rShapeLength = max xShape.Length yShape.Length
            let x = ArrayTake (-rShapeLength) 0 xShape
            let y = ArrayTake (-rShapeLength) 0 yShape
            let rest = Array.tail (Array.map2 (max) x y)
            let xDim0 = unbox (xShape.GetValue 0)
            let yDim0 = 1
            let y = Array.map JTypeIntToJTypeInteger (ArrayTake (-rShapeLength) 1 yShape)
            let yShapeItemNoun = {JType = JTBType.JTypeInteger; JShape = [|rShapeLength|]; JValue = JTypeIntegerArray y ; }
            let yNounItemReshaped = JShape yShapeItemNoun yNoun
            let xShapeItem = Array.map JTypeIntToJTypeInteger (Array. append [|xDim0|] rest)
            let yShapeItem = Array.map JTypeIntToJTypeInteger (Array. append [|yDim0|] rest)
            let xShapeItemNoun = {JType = JTBType.JTypeInteger; JShape = [|rShapeLength|]; JValue = JTypeIntegerArray xShapeItem ; }
            let yShapeItemNoun = {JType = JTBType.JTypeInteger; JShape = [|rShapeLength|]; JValue = JTypeIntegerArray yShapeItem ; }
            let xNounItem = JTake xShapeItemNoun xNoun
            let yNounItem = JTake yShapeItemNoun yNounItemReshaped
            catenate xNounItem yNounItem  
        |_,_ ->
            raise JExceptionSystemError            


    let rec JFrom xNoun yNoun: JNoun =
        match xNoun.JType,xNoun.JShape,xNoun.JValue with
        |xType,xShape,JTypeIntegerArray xValue ->
            let inline from (xType,xShape,(xValue:JTypeInteger array)) (yType,yShape,(yValue:^a array)) unionCase : JNoun =
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                match xShape,yShape with
                |[||],([|0|] | [||]) ->
                    raise JExceptionIndexError
                |[||],[|yLength|] ->
                    let (yIndex:JTypeInt) = unbox (xValueInt.GetValue 0)
                    match yIndex with
                    |yIndex when 0 <= yIndex && yIndex < yLength ->
                        let rValue = unbox( yValue.GetValue yIndex)
                        {JType = yType; JShape = [||]; JValue = unionCase [|rValue|]; }
                    |_->
                        raise JExceptionIndexError   
                |[||],yShape when 0 = Array.fold (*) 1 yShape ->   
                    let (yIndex:JTypeInt) = unbox (xValueInt.GetValue 0)
                    let yLength = unbox( yShape.GetValue 0)
                    match yIndex with
                    |yIndex when 0 <= yIndex && yIndex < yLength ->
                        let rShape = Array.sub yShape 1 (yShape.Length - 1) 
                        {JType = yType; JShape = rShape; JValue = unionCase [||]; }   
                    |_->
                        raise JExceptionIndexError  
                |[||],_ ->
                    let (yIndex:JTypeInt) = unbox (xValueInt.GetValue 0)
                    let yLength = unbox( yShape.GetValue 0)
                    match yIndex with
                    |yIndex when 0 <= yIndex && yIndex < yLength ->
                        let yShapeTail = Array.sub  yShape 1 (yShape.Length-1)
                        let frameSize = Array.fold (*) 1 yShapeTail
                        let rValue = Array.sub yValue (yIndex*frameSize) frameSize
                        {JType = yType ; JShape = yShapeTail; JValue = unionCase rValue; }
                    |_->
                        raise JExceptionIndexError 
                |xShape,[||] when 0 = Array.fold (*) 1 xShape->
                    {JType = yType; JShape = xShape; JValue = unionCase [||]; }
                |xShape,_ when 0 = Array.fold (*) 1 xShape->
                    let yShapeTail = Array.sub  yShape 1 (yShape.Length-1)
                    let rShape = Array.append xShape yShapeTail
                    {JType = yType; JShape = rShape; JValue = unionCase [||]; }
                |[|xLength|],([|0|] | [||]) ->
                    raise JExceptionDomainError
                |[|xLength|],[|yLength|] ->
                    let yIndex = xValueInt
                    match yIndex with
                    |yIndex when Array.fold (fun acc yIx -> acc && 0 <= yIx && yIx < yLength) true yIndex ->
                        let rValue = Array.zeroCreate yIndex.Length
                        let f (rIx:JTypeInt) (yIx:JTypeInt) =
                            let y = yValue.GetValue yIx
                            rValue.SetValue( y, rIx)
                        Array.iteri f yIndex
                        let yShapeTail = Array.sub  yShape 1 (yShape.Length-1)
                        let rShape = Array.append [|xLength|] yShapeTail
                        {JType = yType; JShape = rShape; JValue = unionCase rValue; }
                    |_->
                        raise JExceptionIndexError  
                |[|xLength|],yShape when 0 = Array.fold (*) 1 yShape ->   
                    let yIndex = xValueInt
                    let yLength = unbox( yShape.GetValue 0)
                    match yIndex with
                    |yIndex when Array.fold (fun acc yIx -> acc && 0 <= yIx && yIx < yLength) true yIndex ->
                        let yShapeTail = Array.sub yShape 1 (yShape.Length - 1) 
                        let rShape = Array.append [|xLength|] yShapeTail
                        {JType = yType; JShape = rShape; JValue = unionCase [||]; }   
                    |_->
                        raise JExceptionIndexError  
                |[|xLength|],_ ->
                    let yIndex = xValueInt
                    let yLength = unbox( yShape.GetValue 0)
                    match yIndex with
                    |yIndex when Array.fold (fun acc yIx -> acc && 0 <= yIx && yIx < yLength) true yIndex ->
                        let yShapeTail = Array.sub  yShape 1 (yShape.Length-1)
                        let frameSize = Array.fold (*) 1 yShapeTail
                        let rValue = Array.zeroCreate (yIndex.Length*frameSize)
                        let f (rFrameIx:JTypeInt) (yFrameIx:JTypeInt) =
                            let rIxStart = rFrameIx*frameSize
                            let yIxStart = yFrameIx*frameSize
                            let mutable i = 0
                            while i < frameSize do
                                let xItem = yValue.GetValue (yIxStart+i)
                                rValue.SetValue( xItem, (rIxStart + i))
                                i <- i + 1
                        Array.iteri f yIndex
                        let rShape = Array.append [|xLength|] yShapeTail
                        {JType = yType ; JShape = rShape; JValue = unionCase rValue; }
                    |_->
                        raise JExceptionIndexError 
                |_,_ ->
                    let xNounItemShape = [|Array.fold (*) 1 xShape|]
                    let xNounItem = {JType = xType ; JShape = xNounItemShape; JValue = JTypeIntegerArray xValue; }
                    let rNounItem = JFrom xNounItem yNoun
                    let rNounShapeTail = Array.sub  rNounItem.JShape 1 (rNounItem.JShape.Length-1)
                    let rShape = Array.append xShape rNounShapeTail
                    {JType = yType ; JShape = rShape; JValue = rNounItem.JValue; }
                |_ ->
                    raise JExceptionSystemError        
            match yNoun.JType,yNoun.JShape,yNoun.JValue with
            |yType,yShape,JTypeIntegerArray yValue ->
                from (xType,xShape,xValue) (yType,yShape,yValue) JTypeIntegerArray
            |yType,yShape,JTypeFloatArray yValue ->
                from (xType,xShape,xValue) (yType,yShape,yValue) JTypeFloatArray
            |yType,yShape,JTypeBooleanArray yValue ->
                from (xType,xShape,xValue) (yType,yShape,yValue) JTypeBooleanArray
            |yType,yShape,JTypeUnicodeArray yValue ->
                from (xType,xShape,xValue) (yType,yShape,yValue) JTypeUnicodeArray
            |yType,yShape,JTypeBoxedArray yValue ->
                from (xType,xShape,xValue) (yType,yShape,yValue) JTypeBoxedArray
        |_ ->
            raise JExceptionDomainError

    let rec JAmend xNoun yNoun: JNoun =
        match xNoun with
        |{JValue=JTypeBoxedArray [|iNoun;pNoun|]} ->
            match iNoun.JType,iNoun.JShape,iNoun.JValue with
            |iType,iShape,JTypeIntegerArray iValue ->
                let inline amend (iType,iShape,(iValue:JTypeInteger array)) (pType,(pShape:JTypeInt array),(pValue:^a array)) (yType,yShape,(yValue:^a array)) unionCase : JNoun =
                    let iValueInt = Array.map JTypeIntegerToJTypeInt iValue
                    match iShape,yShape with
                    |[||],([|0|] | [||]) ->
                        raise JExceptionIndexError
                    |[||],[|yLength|] ->
                        let yIndex =  unbox (iValueInt.GetValue 0)
                        match yIndex with
                        |yIndex when 0 <= yIndex && yIndex < yLength ->
                            let rValue = Array.copy yValue
                            match pShape with
                            |[||] ->
                                let y = unbox( pValue.GetValue 0)
                                rValue.SetValue( y, yIndex)
                                {JType = yType; JShape = yShape; JValue = unionCase rValue; }
                            |_->
                                raise JExceptionRankError  
                        |_->
                            raise JExceptionIndexError   
                    |[||],yShape when 0 = Array.fold (*) 1 yShape ->   
                        let yIndex = unbox (iValueInt.GetValue 0)    
                        let yLength = unbox( yShape.GetValue 0)
                        match yIndex with
                        |yIndex when 0 <= yIndex && yIndex < yLength ->
                            match pShape with
                            |pShape when pShape = Array.sub yShape 1 (yShape.Length - 1 ) ->
                                yNoun 
                            |_ ->
                                raise JExceptionIndexError   
                        |_->
                            raise JExceptionIndexError  
                    |[||],_ ->
                        let yIndex =  unbox (iValueInt.GetValue 0)
                        let yLength = unbox( yShape.GetValue 0)
                        match yIndex with
                        |yIndex when 0 <= yIndex && yIndex < yLength ->
                            let yShapeTail = Array.sub  yShape 1 (yShape.Length-1)
                            let frameSize = Array.fold (*) 1 yShapeTail
                            let rValue = Array.copy yValue
                            match pShape with
                            |[|n|] when n = frameSize ->
                                let yIxStart = yIndex*frameSize
                                let mutable i = 0
                                while i < frameSize do
                                    let pItem = pValue.GetValue i
                                    rValue.SetValue( pItem, (yIxStart + i))
                                    i <- i + 1
                                {JType = yType ; JShape = yShape; JValue = unionCase rValue; }
                            |_ ->
                                raise JExceptionLengthError 
                        |_->
                            raise JExceptionIndexError 
                    |[|iLength|],[||] when 0 = Array.fold (*) 1 iShape ->
                        match pShape with
                        |pShape when pShape = Array.append [|iLength|] (Array.sub yShape 1 (yShape.Length - 1 )) ->
                            yNoun 
                        |_ ->
                            raise JExceptionIndexError 
                    |[|iLength|],_ when 0 = Array.fold (*) 1 iShape->
                        match pShape with
                        |pShape when pShape = Array.append [|iLength|] (Array.sub yShape 1 (yShape.Length - 1 )) ->
                            yNoun 
                        |_ ->
                            raise JExceptionIndexError 
                    |[|iLength|],([|0|] | [||]) ->
                        raise JExceptionDomainError
                    |[|iLength|],[|yLength|] ->
                        let yIndex = iValueInt
                        match yIndex with
                        |yIndex when Array.fold (fun acc yIx -> acc && 0 <= yIx && yIx < yLength) true yIndex ->
                            match pShape with
                            |[|n|] when n = iLength ->
                                let rValue = Array.copy yValue
                                let f (pIx:JTypeInt) (yIx:JTypeInt) =
                                    let y = pValue.GetValue pIx
                                    rValue.SetValue( y, yIx)
                                Array.iteri f yIndex
                                {JType = yType; JShape = yShape; JValue = unionCase rValue; }
                            |[|_|] ->
                                raise JExceptionLengthError 
                            |_->
                                raise JExceptionRankError  
                        |_->
                            raise JExceptionIndexError  
                    |[|iLength|],yShape when 0 = Array.fold (*) 1 yShape ->   
                        let yIndex = iValueInt   
                        let yLength = unbox( yShape.GetValue 0)
                        match yIndex with
                        |yIndex when Array.fold (fun acc yIx -> acc && 0 <= yIx && yIx < yLength) true yIndex ->
                            match pShape with
                            |pShape when pShape = Array.append [|iLength|] (Array.sub yShape 1 (yShape.Length - 1 )) ->
                                yNoun 
                            |_ ->
                                raise JExceptionIndexError 
                        |_->
                            raise JExceptionIndexError  
                    |[|iLength|],_ ->
                        let yIndex = iValueInt
                        let yLength = unbox( yShape.GetValue 0)
                        match yIndex with
                        |yIndex when Array.fold (fun acc yIx -> acc && 0 <= yIx && yIx < yLength) true yIndex ->
                            let yShapeTail = Array.sub  yShape 1 (yShape.Length-1)
                            let frameSize = Array.fold (*) 1 yShapeTail
                            match pShape with
                            |pShape when pShape.Length <> yShape.Length ->
                                raise JExceptionRankError
                            |pShape when pShape = Array.append [|iLength|] (Array.sub yShape 1 (yShape.Length - 1 )) ->
                                let rValue = Array.copy yValue
                                let f (pFrameIx:JTypeInt) (rFrameIx:JTypeInt) =
                                    let pIxStart = pFrameIx*frameSize
                                    let rIxStart = rFrameIx*frameSize
                                    let mutable i = 0
                                    while i < frameSize do
                                        let xItem = pValue.GetValue (pIxStart+i)
                                        rValue.SetValue( xItem, (rIxStart + i))
                                        i <- i + 1
                                Array.iteri f yIndex
                                {JType = yType ; JShape = yShape; JValue = unionCase rValue; }
                            |_->
                                raise JExceptionLengthError
                        |_->
                            raise JExceptionIndexError 
                    |_ ->
                        raise JExceptionSystemError  
                match pNoun.JType,pNoun.JShape,pNoun.JValue,yNoun.JType,yNoun.JShape,yNoun.JValue with
                |pType,pShape,JTypeIntegerArray pValue,yType,yShape,JTypeIntegerArray yValue ->
                    amend (iType,iShape,iValue) (pType,pShape,pValue)  (yType,yShape,yValue) JTypeIntegerArray
                |pType,pShape,JTypeFloatArray pValue,yType,yShape,JTypeFloatArray yValue ->
                    amend (iType,iShape,iValue) (pType,pShape,pValue) (yType,yShape,yValue) JTypeFloatArray
                |pType,pShape,JTypeBooleanArray pValue,yType,yShape,JTypeBooleanArray yValue ->
                    amend (iType,iShape,iValue) (pType,pShape,pValue) (yType,yShape,yValue) JTypeBooleanArray
                |pType,pShape,JTypeUnicodeArray pValue,yType,yShape,JTypeUnicodeArray yValue ->
                    amend (iType,iShape,iValue) (pType,pShape,pValue) (yType,yShape,yValue) JTypeUnicodeArray
                |pType,pShape,JTypeBoxedArray pValue,yType,yShape,JTypeBoxedArray yValue ->
                    amend (iType,iShape,iValue) (pType,pShape,pValue) (yType,yShape,yValue) JTypeBoxedArray
                |_ ->
                    raise JExceptionDomainError
            |_ ->
                raise JExceptionDomainError
        |_ ->
            raise JExceptionDomainError



    let inline GradeUpCompare s e (array:^a array) =
        array.[s] <= array.[e]


    let GradeUpCompareUnicode s e (array:JTypeUnicode array) =
        let s = System.String [|array.[s]|]
        let e = System.String [|array.[e]|]
        let a = System.String.Compare(s, e, System.StringComparison.CurrentCultureIgnoreCase)
        a <= 0

    let inline GradeUpCompareArray s e frameSize (fill:^a) (array:^a array)  =
        let mutable is = s * frameSize
        let ismax = is + frameSize
        let mutable ie = e * frameSize
        let mutable continueLoop = true
        let mutable result = true
        let mutable ss = fill
        let mutable ee = fill
        while continueLoop do
            if is < ismax then
                ss <- array.[is]
                ee <- array.[ie]
                if ss = ee then
                    is <- is + 1
                    ie <- ie + 1
                else
                    if ss > ee then
                        result <- false
                        continueLoop <- false
                    else
                        continueLoop <- false
            else
                continueLoop <- false
        result


    let GradeUpCompareUnicodeArray s e frameSize (array:JTypeUnicode array) =
        let is = s * frameSize
        let ss = Array.sub array is frameSize
        let sss = System.String ss
        let ie = e * frameSize
        let ee = Array.sub array ie frameSize
        let eee = System.String ee
        let a = System.String.Compare(sss, eee, System.StringComparison.CurrentCultureIgnoreCase)
        a <= 0

    let inline GradeUp (tally:int) (gradeUpCompare: int -> int -> bool) :int array  =
        //This function is very optimized. Handle with care!
        //It is a mergesort. It is sorting a sort vector, not the data.
        //The sort vector is split in two parts, which are handled recursively
        //When only one or two remains, if one, it is set as result, if two they are
        //sorted and set as result.
        //On the way out of recursion the sorted parts are merged.
        //The function takes three times longer than the Microsoft quicksort
        //This might well be world class, since this is a stable sort.
        //It is determinate, takes about the same time for all same size arguments.
        //The size requirements are the data to be sorted + 2 times the sort vector.
        //The sort vector and the scratch vector are switched.
        //On one level one is sort vector the other scratch on next level roles are switched
        //realindex points to the same sort vector.
        //The array you read from can be the same as the array you write to on the way
        //down in the recursion. You have to read both s and e before writing any of them!
        //Seriously trickprogrammed function, so again, take care.
        match tally with 
        |0 -> 
            [||]
        |_ ->
            let index = Array.init tally id
            let scratch = Array.zeroCreate tally
            let rec sort (index:int array)  (scratch:int array) (realindex:int array) (iStart:int) (iEnd:int) =
                let  mutable s = 0
                let  mutable e = 0
                let mutable n = iEnd - iStart
                match n with
                |0 -> 
                    scratch.[iStart] <- realindex.[iStart]
                |1 -> 
                    s <- realindex.[iStart]
                    e <- realindex.[iEnd]
                    if (gradeUpCompare s e) then
                        scratch.[iStart] <- s
                        scratch.[iEnd] <- e
                    else 
                        scratch.[iStart] <- e
                        scratch.[iEnd] <- s
                |_ ->
                    let half = n / 2
                    let split = iStart + half
                    //scratch and index are switched here
                    sort scratch  index realindex iStart split
                    sort scratch  index realindex (split + 1) iEnd
                    let mutable i = iStart 
                    let mutable index1 = iStart
                    let mutable index2 = split + 1
                    while index1 <= split && index2 <= iEnd do
                        s <- index.[index1]
                        e <- index.[index2]
                        if (gradeUpCompare s e) then
                            scratch.[i] <- s  
                            index1 <- index1 + 1
                        else
                            scratch.[i] <- e
                            index2 <- index2 + 1
                        i <- i + 1  
                    while index1 <= split do
                        scratch.[i] <- index.[index1]
                        index1 <- index1 + 1
                        i <- i + 1
                    while index2 <= iEnd do
                        scratch.[i] <- index.[index2]
                        index2 <- index2 + 1
                        i <- i + 1
            sort index scratch index 0 ( tally - 1 )
            scratch
          
    let JGradeUp yNoun: JNoun =
        match yNoun.JType,yNoun.JShape,yNoun.JValue with
        |yType,yShape,JTypeIntegerArray yValue ->
            match yShape with 
            |[||]->
                raise JExceptionRankError
            |yShape when 0 = unbox (yShape.GetValue 0) ->
                {JType = JTBType.JTypeInteger ; JShape = [|0|]; JValue = JTypeIntegerArray [||]; }
            |yShape when 0 = Array.fold (*) 1 yShape  ->
                let yLength = unbox (yShape.GetValue 0)
                let rShape = [|yLength|]
                let rValue = [|0L..(JTypeIntToJTypeInteger (yLength-1))|]
                {JType = JTBType.JTypeInteger ; JShape = rShape; JValue = JTypeIntegerArray rValue; }
            |[|yLength|] ->
                let compare s e = GradeUpCompare s e yValue  
                let rValue = Array.map JTypeIntToJTypeInteger (GradeUp yLength compare)
                {JType = JTBType.JTypeInteger ; JShape = yShape; JValue = JTypeIntegerArray rValue; }
            |_ ->
                let yShapeTail = Array.sub yShape 1 (yShape.Length-1)
                let frameSize = Array.fold (*) 1 yShapeTail
                let nValues = unbox (yShape.GetValue 0)
                let compareArray s e = GradeUpCompareArray s e frameSize 0L yValue
                let rValue = Array.map JTypeIntToJTypeInteger (GradeUp nValues compareArray)
                {JType = JTBType.JTypeInteger ; JShape = [|nValues|]; JValue = JTypeIntegerArray rValue; }
        |yType,yShape,JTypeFloatArray yValue ->
            match yShape with 
            |[||]->
                raise JExceptionRankError
            |yShape when 0 = unbox (yShape.GetValue 0) ->
                {JType = JTBType.JTypeInteger ; JShape = [|0|]; JValue = JTypeIntegerArray [||]; }
            |yShape when 0 = Array.fold (*) 1 yShape  ->
                let yLength = unbox (yShape.GetValue 0)
                let rShape = [|yLength|]
                let rValue = [|0L..(JTypeIntToJTypeInteger(yLength-1))|]
                {JType = JTBType.JTypeInteger ; JShape = rShape; JValue = JTypeIntegerArray rValue; }
            |[|yLength|] ->
                let compare s e = GradeUpCompare s e yValue  
                let rValue = Array.map JTypeIntToJTypeInteger (GradeUp yLength compare)
                {JType = JTBType.JTypeInteger ; JShape = yShape; JValue = JTypeIntegerArray rValue; }
            |_ ->
                let yShapeTail = Array.sub yShape 1 (yShape.Length-1)
                let frameSize = Array.fold (*) 1 yShapeTail
                let nValues = unbox (yShape.GetValue 0)
                let compareArray s e = GradeUpCompareArray s e frameSize 0.0 yValue
                let rValue = Array.map JTypeIntToJTypeInteger (GradeUp nValues compareArray)
                {JType = JTBType.JTypeInteger ; JShape = [|nValues|]; JValue = JTypeIntegerArray rValue; }
        |yType,yShape,JTypeBooleanArray yValue ->
            match yShape with 
            |[||]->
                raise JExceptionRankError
            |yShape when 0 = unbox (yShape.GetValue 0) ->
                {JType = JTBType.JTypeInteger ; JShape = [|0|]; JValue = JTypeIntegerArray [||]; }
            |yShape when 0 = Array.fold (*) 1 yShape  ->
                let yLength = unbox (yShape.GetValue 0)
                let rShape = [|yLength|]
                let rValue = [|0L..(JTypeIntToJTypeInteger(yLength-1))|]
                {JType = JTBType.JTypeInteger ; JShape = rShape; JValue = JTypeIntegerArray rValue; }
            |[|yLength|] ->
                let compare s e = GradeUpCompare s e yValue  
                let rValue = Array.map JTypeIntToJTypeInteger (GradeUp yLength compare)
                {JType = JTBType.JTypeInteger ; JShape = yShape; JValue = JTypeIntegerArray rValue; }
            |_ ->
                let yShapeTail = Array.sub yShape 1 (yShape.Length-1)
                let frameSize = Array.fold (*) 1 yShapeTail
                let nValues = unbox (yShape.GetValue 0)
                let compareArray s e = GradeUpCompareArray s e frameSize false yValue
                let rValue = Array.map JTypeIntToJTypeInteger (GradeUp nValues compareArray)
                {JType = JTBType.JTypeInteger ; JShape = [|nValues|]; JValue = JTypeIntegerArray rValue; }
        |yType,yShape,JTypeUnicodeArray yValue ->
            match yShape with 
            |[||]->
                raise JExceptionRankError
            |yShape when 0 = unbox (yShape.GetValue 0) ->
                {JType = JTBType.JTypeInteger ; JShape = [|0|]; JValue = JTypeIntegerArray [||]; }
            |yShape when 0 = Array.fold (*) 1 yShape  ->
                let yLength = unbox (yShape.GetValue 0)
                let rShape = [|yLength|]
                let rValue = [|0L..(JTypeIntToJTypeInteger(yLength-1))|]
                {JType = JTBType.JTypeInteger ; JShape = rShape; JValue = JTypeIntegerArray rValue; }
            |[|yLength|] ->
                let compare s e = GradeUpCompareUnicode s e yValue  
                let rValue = Array.map JTypeIntToJTypeInteger (GradeUp yLength compare)
                {JType = JTBType.JTypeInteger ; JShape = yShape; JValue = JTypeIntegerArray rValue; }
            |_ ->
                let yShapeTail = Array.sub yShape 1 (yShape.Length-1)
                let frameSize = Array.fold (*) 1 yShapeTail
                let nValues = unbox (yShape.GetValue 0)
                let compareArray s e = GradeUpCompareUnicodeArray s e frameSize  yValue
                let rValue = Array.map JTypeIntToJTypeInteger (GradeUp nValues compareArray)
                {JType = JTBType.JTypeInteger ; JShape = [|nValues|]; JValue = JTypeIntegerArray rValue; }
        |_ ->
            raise JExceptionDomainError            

    let selectIndices (q:JTypeBoolean array) =
        let m = Array.fold (fun acc q -> if q then acc + 1 else acc) 0 q
        let r = Array.zeroCreate m
        let mutable i = 0
        let mutable j = 0
        while (i < (q.Length)) do
            if q.[i] then
                r.[j] <- i
                j <- j + 1
            i <- i + 1
        r

    let replicateIndices (a:JTypeInt array) =
        let m = Array.fold (+) 0 a
        let r = Array.zeroCreate m
        let ix = Array.init a.Length id
        let mutable i = 0
        let mutable j = 0
        while (j < ix.Length) do
            let mutable k = 0
            while k < a.[j] do
                r.[i] <- j
                i <- i + 1
                k <- k + 1
            j <- j + 1
        r

    let JSelect xNoun yNoun : JNoun =
        match xNoun.JType,xNoun.JShape,xNoun.JValue with
        |xType,xShape,JTypeBooleanArray xValue ->
            match xShape,yNoun.JShape with
            |[||],_ ->
                if (unbox (xValue.GetValue 0)) then
                    yNoun
                else 
                    JTake ZeroNounConstant yNoun
            |_,[||] ->
                raise JExceptionRankError
            |[|xLength|],yShape when xLength = (unbox(yShape.GetValue 0)) ->
                let indices = Array.map JTypeIntToJTypeInteger (selectIndices xValue)
                let xNounItem = {JType = JTBType.JTypeInteger ; JShape = [|indices.Length|]; JValue = JTypeIntegerArray indices; }
                JFrom xNounItem yNoun
            |[|xLength|],_ ->
                raise JExceptionLengthError
            |_ -> 
                raise JExceptionRankError
        |_ ->
            raise JExceptionDomainError

    let rec JReplicate xNoun yNoun : JNoun =
        match xNoun.JType,xNoun.JShape,xNoun.JValue with
        |xType,xShape,JTypeIntegerArray xValue ->
            let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
            match xShape,yNoun.JShape with
            |[||],[||] ->
                let indices = Array.map JTypeIntToJTypeInteger (replicateIndices xValueInt)
                let xNounItem = {JType = yNoun.JType ; JShape = [|indices.Length|]; JValue = JTypeIntegerArray indices; }
                let yNounItem = {JType = yNoun.JType ; JShape = [|1|]; JValue = yNoun.JValue; }
                JFrom xNounItem yNounItem
            |[||],[|0|] ->
                yNoun
            |[||],yShape ->
                let yLength = (unbox(yShape.GetValue 0))
                let n = unbox(xValueInt.GetValue 0)
                let a = Array.create yLength n
                let indices = Array.map JTypeIntToJTypeInteger (replicateIndices a)
                let xNounItem = {JType = yNoun.JType ; JShape = [|indices.Length|]; JValue = JTypeIntegerArray indices; }
                let yNounItem = {JType = yNoun.JType ; JShape = yNoun.JShape; JValue = yNoun.JValue; }
                JFrom xNounItem yNounItem
            |[|xLength|],[||] ->
                let indices = Array.map JTypeIntToJTypeInteger (replicateIndices [|xLength|])
                let xNounItem = {JType = yNoun.JType ; JShape = [|indices.Length|]; JValue = JTypeIntegerArray indices; }
                let yNounItem = {JType = yNoun.JType ; JShape = [|1|]; JValue = yNoun.JValue; }
                let replicatedYNoun = JFrom xNounItem yNounItem
                JReplicate xNoun replicatedYNoun
            |[|0|],[|0|] ->
                yNoun
            |[|xLength|],[|0|] ->
                raise JExceptionLengthError
            |[|xLength|],yShape when xLength = (unbox(yShape.GetValue 0)) ->
                let indices = Array.map JTypeIntToJTypeInteger (replicateIndices xValueInt)
                let xNounItem = {JType = JTBType.JTypeInteger ; JShape = [|indices.Length|]; JValue = JTypeIntegerArray indices; }
                JFrom xNounItem yNoun
            |[|xLength|],_ ->
                raise JExceptionLengthError
            |_ -> 
                raise JExceptionDomainError
        |_ ->
            raise JExceptionDomainError
 
    let BetweenConsecutive yNoun uVerb =
        //Applies the verb between consecutive elements of the vector argument
        let negativeOneNoun = {JType = JTBType.JTypeInteger ; JShape = [||]; JValue = JTypeIntegerArray [|-1L|]; }
        let aNoun = JDrop OneNounConstant yNoun
        let bNoun = JDrop negativeOneNoun yNoun
        uVerb aNoun bNoun


    let BooleanToInteger yNoun =
        match yNoun.JType,yNoun.JShape,yNoun.JValue with
        |yType,yShape,JTypeBooleanArray yValue ->
            let rValue = Array.map (fun q -> if q then 1L else 0L) yValue
            {JType = JTBType.JTypeInteger ; JShape = yShape; JValue = JTypeIntegerArray rValue; }
        |_ ->
            raise JExceptionDomainError


    let JIotaDyadic xNoun yNoun :JNoun =
    //This is a sorting solution to dyadic Iota which otherwise would take very long time
    //when both arguments are big.
    //The solution probably is slow for small arguments. For big arguments about 10 times
    //faster.
    //x and y are catenated and sorted. Equal items are grouped. Since the sort is stable
    //the x items will be before the y items in each group and no equal items will change order.
    //The indices of x are catenated with (1 + length of x), which is replicated for each y
    //These indices are sorted in the same way as x catenated with y.
    //If there is a hit the first item in each group in these sorted indices are now index 
    //to the first x hit. If there is no hit the first item in the group is (1 + length of x).
    //These first items are selected and replicated over the groups.
    //The results are sorted back to the original order. The results corresponding to x items are
    //dropped and the remains is the resulting indices.
        match xNoun.JShape,yNoun.JShape with
        |([|_|] | [||]), _ ->
            let xNounRaveled = JRavel xNoun
            let yNounRaveled = JRavel yNoun
            let unSorted = JCatenate xNoun yNounRaveled
            let sortOrder = JGradeUp unSorted
            let sorted = JFrom sortOrder unSorted
            let trueNoun = {JType = JTBType.JTypeBoolean ; JShape = [||]; JValue = JTypeBooleanArray [|true|]; }
            let firstInGroup = JTake (JTally sorted) (JCatenate trueNoun (BetweenConsecutive sorted JNotEqual))
            let unsortedIndexes = JCatenate (JIotaMonadic (JTally xNounRaveled)) (JShape (JTally yNounRaveled) (JTally xNounRaveled))
            let sortedIndexes = JFrom sortOrder unsortedIndexes
            let indexInXNounOrTallyNounAddOne = JSelect firstInGroup sortedIndexes
            let replicateVector = JSubtract (JScan ZeroNounConstant (BooleanToInteger firstInGroup) JAdd) OneNounConstant
            let indexesReplicatedOverGroups = JFrom replicateVector indexInXNounOrTallyNounAddOne
            let indexesInOriginalOrder = JFrom (JGradeUp sortOrder) indexesReplicatedOverGroups
            let indexesCorrespondingToYNoun = JDrop (JTally xNounRaveled) indexesInOriginalOrder
            JShape (JShapeOf yNoun) indexesCorrespondingToYNoun
        |[|_;_|], [|_;_|] ->
            let unSorted = JCatenate xNoun yNoun
            let sortOrder = JGradeUp unSorted
            let sorted = JFrom sortOrder unSorted
            let trueNoun = {JType = JTBType.JTypeBoolean ; JShape = [||]; JValue = JTypeBooleanArray [|true|]; }
            let NotAllEqual x y = JNot (JFold trueNoun (JEqual x y) JAnd)
            let oneOne = {JType = JTBType.JTypeInteger ; JShape = [|2|]; JValue = JTypeIntegerArray [|1L;1L|]; }
            let f x y = JRankDyadic x y NotAllEqual oneOne
            let firstInGroup = JTake (JTally sorted) (JCatenate trueNoun (BetweenConsecutive sorted f))
            let unsortedIndexes = JCatenate (JIotaMonadic (JTally xNoun)) (JShape (JTally yNoun) (JTally xNoun))
            let sortedIndexes = JFrom sortOrder unsortedIndexes
            let indexInXNounOrTallyNounAddOne = JSelect firstInGroup sortedIndexes
            let replicateVector = JSubtract (JScan ZeroNounConstant (BooleanToInteger firstInGroup) JAdd) OneNounConstant
            let indexesReplicatedOverGroups = JFrom replicateVector indexInXNounOrTallyNounAddOne
            let indexesInOriginalOrder = JFrom (JGradeUp sortOrder) indexesReplicatedOverGroups
            let indexesCorrespondingToYNoun = JDrop (JTally xNoun) indexesInOriginalOrder
            JShape (JTake OneNounConstant (JShapeOf yNoun)) indexesCorrespondingToYNoun
        |_ ->
            raise JExceptionRankError


    let JTranspose xNoun yNoun :JNoun =
        match xNoun.JType,xNoun.JShape,xNoun.JValue with
        |xType,xShape,JTypeIntegerArray xValue ->
            match xShape,xValue with
            |[|0|],_->
                match yNoun.JShape with
                |( [||] | [|0|] ) ->
                    {JType = yNoun.JType ; JShape = yNoun.JShape; JValue = yNoun.JValue; }
                |_ -> 
                    raise JExceptionLengthError                    
            |xShape,xValue when xShape.Length = 1 && xValue.Length = yNoun.JShape.Length ->
                let xValueInt = Array.map JTypeIntegerToJTypeInt xValue
                let sorted = Array.sort xValueInt
                let iota = Array.init xValueInt.Length id
                match sorted with
                |sorted when sorted = iota ->
                    let transpose (xType,xShape,xValueInt) (yType,(yShape:JTypeInt array),(yValue:'a array)) unionCase =
                        let (rValue:'a array) = Array.zeroCreate (Array.fold (*) 1 yShape)
                        let frameSize = Array.tail (Array.scanBack (*) yShape 1)
                        let rIx = ref 0
                        let yIx = 0
                        let rec iterate (dim:JTypeInt) (xShape:JTypeInt array) (xValue:JTypeInt array) (frameSize:JTypeInt array) (rValue:'a array) (rIx:int ref) yIx (yShape:JTypeInt array) (yValue:'a array) =
                            let mutable i = 0
                            let yDim = xValue.[dim]
                            while i < yShape.[yDim] do
                                let yIxItem = yIx + (i*frameSize.[yDim])
                                if dim < (yShape.Length - 1 ) then
                                    iterate (dim+1) xShape xValue frameSize rValue rIx yIxItem yShape yValue
                                else
                                    rValue.[!rIx] <- yValue.[yIxItem]
                                    rIx := !rIx + 1
                                i <- i + 1
                        iterate 0 xShape xValueInt frameSize rValue rIx yIx yShape yValue
                        let rShape = Array.map (fun x -> yShape.[x]) xValueInt
                        {JType = yType ; JShape = rShape; JValue = unionCase rValue; }
                    match yNoun.JType,yNoun.JShape,yNoun.JValue with
                    |yType,yShape,JTypeIntegerArray yValue ->
                        transpose (xType,xShape,xValueInt) (yType,yShape,yValue) JTypeIntegerArray
                    |yType,yShape,JTypeFloatArray yValue ->
                        transpose (xType,xShape,xValueInt) (yType,yShape,yValue) JTypeFloatArray
                    |yType,yShape,JTypeBooleanArray yValue ->
                        transpose (xType,xShape,xValueInt) (yType,yShape,yValue) JTypeBooleanArray
                    |yType,yShape,JTypeUnicodeArray yValue ->
                        transpose (xType,xShape,xValueInt) (yType,yShape,yValue) JTypeUnicodeArray
                    |yType,yShape,JTypeBoxedArray yValue ->
                        transpose (xType,xShape,xValueInt) (yType,yShape,yValue) JTypeBoxedArray
                |_ ->
                    raise JExceptionRankError
            |_ ->
                raise JExceptionLengthError
        |_ ->
            raise JExceptionDomainError


        
            
    let rec JDefaultFormat yNoun : JNoun =
        match yNoun.JType,yNoun.JShape,yNoun.JValue with
        |yType,yShape,JTypeIntegerArray yValue ->
            match yShape with
            |([||] | [|_|] |[|_;_|]) ->
                let rLength = 
                    match yShape with
                    |[||] ->
                        1
                    |_ ->
                        Array.fold (*) 1 yShape
                let (rValue:JTypeBoxed array) = Array.zeroCreate rLength
                let f ix y =
                    let s = sprintf " %i" y
                    let rValueItem = Array.ofSeq s
                    let rShapeItem = [|rValueItem.Length|]
                    let rNounItem = {JType = JTBType.JTypeUnicode  ; JShape = rShapeItem; JValue = JTypeUnicodeArray rValueItem; }
                    rValue.[ix] <- rNounItem
                Array.iteri f yValue
                JDefaultFormat {JType = JTBType.JTypeBoxed  ; JShape = yShape; JValue = JTypeBoxedArray rValue; }
            |_ ->
                let f acc x =
                    acc + sprintf " %i" x
                let s1 = Array.fold f "" yShape
                let a = Array.sub yValue 0 (min 20 yValue.Length)
                let s2 = Array.fold (fun acc x -> acc + sprintf " %i" x) "" a
                let s3 = sprintf "Type: Integer, Shape:%s , Values:%s ..." s1 s2
                let rValue = Array.ofSeq s3
                let rShape = [|rValue.Length|]
                {JType = JTBType.JTypeUnicode  ; JShape = rShape; JValue = JTypeUnicodeArray rValue; }  
        |yType,yShape,JTypeFloatArray yValue ->
            match yShape with
            |([||] | [|_|] |[|_;_|]) ->
                let rLength = 
                    match yShape with
                    |[||] ->
                        1
                    |_ ->
                        Array.fold (*) 1 yShape
                let (rValue:JTypeBoxed array) = Array.zeroCreate rLength
                let f ix y =
                    let s = sprintf " %.6g"  y
                    let rValueItem = Array.ofSeq s
                    let rShapeItem = [|rValueItem.Length|]
                    let rNounItem = {JType = JTBType.JTypeUnicode  ; JShape = rShapeItem; JValue = JTypeUnicodeArray rValueItem; }
                    rValue.[ix] <- rNounItem
                Array.iteri f yValue
                JDefaultFormat {JType = JTBType.JTypeBoxed  ; JShape = yShape; JValue = JTypeBoxedArray rValue; }
            |_ ->
                let f acc x =
                    acc + sprintf " %i" x
                let s1 = Array.fold f "" yShape
                let a = Array.sub yValue 0 (min 20 yValue.Length)
                let s2 = Array.fold (fun acc x -> acc + sprintf " %.6g" x) "" a
                let s3 = sprintf "Type: Float, Shape:%s , Values:%s ..." s1 s2
                let rValue = Array.ofSeq s3
                let rShape = [|rValue.Length|]
                {JType = JTBType.JTypeUnicode  ; JShape = rShape; JValue = JTypeUnicodeArray rValue; }  
        |yType,yShape,JTypeBooleanArray yValue ->
            match yShape with
            |([||] | [|_|] |[|_;_|]) ->
                let rLength = 
                    match yShape with
                    |[||] ->
                        1
                    |_ ->
                        Array.fold (*) 1 yShape
                let (rValue:JTypeBoxed array) = Array.zeroCreate rLength
                let f ix y =
                    let s = sprintf " %b"  y
                    let rValueItem = Array.ofSeq s
                    let rShapeItem = [|rValueItem.Length|]
                    let rNounItem = {JType = JTBType.JTypeUnicode  ; JShape = rShapeItem; JValue = JTypeUnicodeArray rValueItem; }
                    rValue.[ix] <- rNounItem
                Array.iteri f yValue
                JDefaultFormat {JType = JTBType.JTypeBoxed  ; JShape = yShape; JValue = JTypeBoxedArray rValue; }
            |_ ->
                let f acc x =
                    acc + sprintf " %i" x
                let s1 = Array.fold f "" yShape
                let a = Array.sub yValue 0 (min 20 yValue.Length)
                let s2 = Array.fold (fun acc x -> acc + sprintf " %b" x) "" a
                let s3 = sprintf "Type: Boolean, Shape:%s , Values:%s ..." s1 s2
                let rValue = Array.ofSeq s3
                let rShape = [|rValue.Length|]
                {JType = JTBType.JTypeUnicode  ; JShape = rShape; JValue = JTypeUnicodeArray rValue; }  
        |yType,yShape,JTypeUnicodeArray yValue ->
            match yShape with
            |([||] | [|_|] |[|_;_|]) ->
                {JType = JTBType.JTypeUnicode  ; JShape = yShape; JValue = JTypeUnicodeArray yValue; }
            |_ ->
                let f acc x =
                    acc + sprintf " %i" x
                let s1 = Array.fold f "" yShape
                let a = Array.sub yValue 0 (min 20 yValue.Length)
                let s2 = Array.fold (fun acc x -> acc + sprintf " %c" x) "" a
                let s3 = sprintf "Type: Unicode, Shape:%s , Values:%s ..." s1 s2
                let rValue = Array.ofSeq s3
                let rShape = [|rValue.Length|]
                {JType = JTBType.JTypeUnicode  ; JShape = rShape; JValue = JTypeUnicodeArray rValue; }                
        |yType,yShape,JTypeBoxedArray yValue ->
            match yShape with
            |yShape when 0 = Array.fold (*) 1 yShape ->
                {JType = JTBType.JTypeUnicode  ; JShape = [|0|]; JValue = JTypeUnicodeArray [||]; }
            |[||] ->
                let iNoun = yValue.[0]
                match iNoun.JType,iNoun.JShape,iNoun.JValue with
                |iType,iShape,JTypeUnicodeArray iValue ->
                    match iShape with
                    |iShape when iShape.Length <= 2 ->
                        iNoun
                    |_ ->
                        let f acc x =
                            acc + sprintf " %i" x
                        let s1 = Array.fold f "" iShape
                        let s2 = System.String (Array.sub iValue 0 (min 20 iValue.Length))
                        let s3 = sprintf "Type: Unicode, Shape:%s , Values:%s ..." s1 s2
                        let rValue = Array.ofSeq s3
                        let rShape = [|rValue.Length|]
                        {JType = JTBType.JTypeUnicode  ; JShape = rShape; JValue = JTypeUnicodeArray rValue; }
                |iType,iShape,JTypeBoxedArray iValue ->
                    JDefaultFormat iNoun
                |iType,iShape,JTypeIntegerArray iValue ->
                    JDefaultFormat (JDefaultFormat iNoun)
                |iType,iShape,JTypeFloatArray iValue ->
                    JDefaultFormat (JDefaultFormat iNoun)
                |iType,iShape,JTypeBooleanArray iValue ->
                    JDefaultFormat (JDefaultFormat iNoun)
            |[|_|] ->
                let rLength = 
                    match yShape with
                    |[||] ->
                        1
                    |_ ->
                        Array.fold (*) 1 yShape
                let (rValue:JTypeBoxed array) = Array.zeroCreate rLength
                let f ix x =
                    let aNounItem = JDefaultFormat x
                    let OneOne = {JType = JTBType.JTypeInteger  ; JShape = [|2|]; JValue = JTypeIntegerArray [|1L;1L|]; }
                    let MinusTwo = {JType = JTBType.JTypeInteger  ; JShape = [||]; JValue = JTypeIntegerArray [|-2L|]; }
                    let rNounItem= JShape (JTake MinusTwo (JCatenate OneOne (JShapeOf aNounItem))) aNounItem
                    rValue.[ix] <- (JBox rNounItem)
                Array.iteri f yValue
                let f acc  x =
                    let xNoun = JTally (JOpen x)
                    JMax acc xNoun
                let nRowsNoun = Array.fold f ZeroNounConstant rValue
                let f acc x =
                    let OneOne = {JType = JTBType.JTypeInteger  ; JShape = [|2|]; JValue = JTypeIntegerArray [|1L;1L|]; }
                    JBox (JRankDyadic (JOpen acc) (JTake nRowsNoun (JOpen x)) JCatenate OneOne)
                let nRows =
                    match nRowsNoun.JValue with 
                    |JTypeIntegerArray nValue ->
                        (int) nValue.[0]
                    |_ ->
                        raise JExceptionSystemError
                let init = {JType = JTBType.JTypeUnicode  ; JShape = [|nRows;0|]; JValue = JTypeUnicodeArray [||]; }
                let initBox = {JType = JTBType.JTypeBoxed  ; JShape = [||]; JValue = JTypeBoxedArray [|init|]; }
                let rNoun= Array.fold f initBox rValue 
                JOpen rNoun
            |[|_;_|] ->
                let rLength = 
                    match yShape with
                    |[||] ->
                        1
                    |_ ->
                        Array.fold (*) 1 yShape
                let (rValue:JTypeBoxed array) = Array.zeroCreate rLength
                let f ix x =
                    let aNounItem = JDefaultFormat  x
                    let OneOne = {JType = JTBType.JTypeInteger  ; JShape = [|2|]; JValue = JTypeIntegerArray [|1L;1L|]; }
                    let MinusTwo = {JType = JTBType.JTypeInteger  ; JShape = [||]; JValue = JTypeIntegerArray [|-2L|]; }
                    let rNounItem= JShape (JTake MinusTwo (JCatenate OneOne (JShapeOf aNounItem))) aNounItem
                    rValue.[ix] <- rNounItem
                Array.iteri f yValue
                let iNoun = {JType = JTBType.JTypeBoxed  ; JShape = yShape; JValue = JTypeBoxedArray rValue; }
                let f acc y = JMax acc (JTally (JOpen y))
                let g x y = JFold x y f
                let ZeroOne = {JType = JTBType.JTypeInteger  ; JShape = [|2|]; JValue = JTypeIntegerArray [|0L;1L|]; }
                let rowHeight = JRankDyadic ZeroNounConstant iNoun g ZeroOne
                let f acc y = 
                    let g y = 
                        let aaNoun = JOpen y
                        let aNoun = JShapeOf aaNoun
                        JFrom OneNounConstant aNoun
                    let bNoun = JRankMonadic y g ZeroNounConstant
                    JMax acc bNoun
                let columnWidth = JFold ZeroNounConstant iNoun f
                let ZeroZero = {JType = JTBType.JTypeInteger  ; JShape = [|2|]; JValue = JTypeIntegerArray [|0L;0L|]; }
                let f x y = JRankDyadic x y JCatenate ZeroZero
                let sizeTable = JRankDyadic rowHeight (JNegate columnWidth) f ZeroOne
                let f x y = JBox (JTake x (JOpen y))
                let OneZero = {JType = JTBType.JTypeInteger  ; JShape = [|2|]; JValue = JTypeIntegerArray [|1L;0L|]; }
                let rNounItem = JRankDyadic sizeTable iNoun f OneZero
                let rows = JFold ZeroNounConstant rowHeight JAdd
                let columns = JFold ZeroNounConstant columnWidth JAdd
                let EmptyUnicode = {JType = JTBType.JTypeUnicode  ; JShape = [|0|]; JValue = JTypeUnicodeArray [||]; }
                let EmptyUnicodeBox = {JType = JTBType.JTypeBoxed  ; JShape = [||]; JValue = JTypeBoxedArray [|EmptyUnicode|]; }
                let OneOne = {JType = JTBType.JTypeInteger  ; JShape = [|2|]; JValue = JTypeIntegerArray [|1L;1L|]; }
                let f acc y = JBox (JRankDyadic (JOpen acc) (JOpen y) JCatenate OneOne)
                let g x y = JFold x y f
                let boxedColumns = JRankDyadic EmptyUnicodeBox rNounItem g ZeroOne
                let h acc y = JBox (JCatenate (JOpen acc) (JOpen y))
                let init = JBox (JShape (JCatenate ZeroNounConstant columns) EmptyUnicode)
                let boxedPage = JFold init boxedColumns h
                JOpen boxedPage
            |_ ->
                let f acc x =
                    acc + sprintf " %i" x
                let s1 = Array.fold f "" yShape
                let s3 = sprintf "Type: Boxed, Shape:%s" s1
                let rValue = Array.ofSeq s3
                let rShape = [|rValue.Length|]
                {JType = JTBType.JTypeUnicode  ; JShape = rShape; JValue = JTypeUnicodeArray rValue; }


(*
    //This function is good to have for testing.
    let JPrint xNoun =
        printfn "%A %A %A" xNoun.JType xNoun.JShape xNoun.JValue
*)            

    let JPrint yNoun =
        let aNoun = JDefaultFormat yNoun
        match aNoun.JType,aNoun.JShape,aNoun.JValue with
        |aType,aShape,JTypeUnicodeArray aValue ->
            match aShape with 
            |[|0|] ->
                printfn ""
            |([||] | [|_|] )->
                let s = aValue
                printfn "%s" (System.String aValue)
            |_ ->
                let nValues = aShape.[0]
                let frameSize = aShape.[1]
                let mutable i = 0
                while i < nValues do
                    let a = Array.sub aValue (i*frameSize) frameSize
                    printfn "%s" (System.String a)
                    i <- i + 1
        |_ ->
            raise JExceptionSystemError

