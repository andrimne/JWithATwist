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

open JWithATwist
open JWithATwist.ParserMock
open System
open System.Diagnostics
open FParsec

let test p str =
    match run p str with
    | Success(result, _, rest)   -> printfn "Success: %A %A" result rest
    | Failure(errorMsg, n, m) -> printfn "Failure: %s %A %A" errorMsg n m


[<EntryPoint>]
let main argv = 

    let mutable quit = false
    while not quit do
        printfn @"
JWithATwist Copyright (C) 2016 Erling Hellenäs
This program comes with ABSOLUTELY NO WARRANTY; for details type `w'.
This is free software, and you are welcome to redistribute it
under certain conditions; type `c' for details.
Type h for help, type return to continue"
        let inputStream = Console.ReadLine()
        quit <- inputStream.Length=0
        match quit with
        |false ->
            let a =
                match inputStream with
                |"c" ->
                    printfn @"
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version."
                    ()
                |"w" ->
                    printfn @"
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.       "
                    ()
                |"h" ->
                    printfn @"
JWithATwist Mock Parser
JWithATwist is a programming language under development. It has many similarities with J.
You can look at www.jsoftware.com to find out what J is. 
This parser is supposed to become the parser for this new language, but the language ele-
ments are only mock objects. You can only work with integers. There are only a few opera-
tions you can use.
I use the J syntax to describe this language.
	- NOUN - an n-dimensional array of values where the values are simple data types
	like character or integer, but they can also can be other n-dimensional arrays. 
	- VERB - a function with one or two noun arguments returning a noun.
	- ADVERB - an operator with a verb argument and one or two noun arguments, usually
	returning a verb.
	- CONJUNCTION - an operator with two verb arguments and one or two noun arguments,
	normally returning a verb.
	- A MONADIC VERB has one argument, a DYADIC VERB has two. Adverbs and conjunctions
	are also called monadic or dyadic after their number of noun argments.
The operations in this mock parser:
	- Dyadic verb Addition, +
	- Dyadic verb Subtraction, -
	- Monadic verb Negation, |-
	- Monadic adverb, / . This operator is defined u / y <-> y u y. It applies the in-
	fix u verb between two copies of the verb argument.  + / 1 <-> 1 + 1 <-> 2.
	- Dyadic adverb, /- . This operator is defined x u/- y <-> x u y. It applies the
	infix u verb between the left and right argument.  1 + /- 1 <-> 1 + 1 <-> 2
	- Monadic conjunction, |. . This operator is defined  u |. v y <-> u y v y. It app-
	lies the infix dyadic verb v between copies of the right argument and the monadic
	verb u to the result. |- |. + 1 <-> |- 1 + 1 <-> -2 .
	- Dyadic conjunction, .  . This operator is defined x u . v y <-> u x v y . It app-
	lies the infix dyadic verb v between the left and right arguments and the monadic
	verb u to the result. 2 |- . + 5  <-> |- 2 + 5 <-> -7 .
There is no order of precedence between the operations. Execution is from right to left mo-
dified by parenthesis. |- |- |- 1 <-> -1
The language elements are associated to it's arguments like this.
	-A monadic verb has one noun argument to the right. Like this: |- 1 .
	-A dyadic verb has one noun argument to the left and one to the right. Like this:
	1 + 1 .
	-A monadic adverb has a verb argument to the left and a noun argument to the right.
	Like this: + / 1 .
	-A dyadic adverb has a verb argument to the left, one noun argument to the left
	and one to the	right. Like this: 1 + /- 1 .
	-A monadic conjunction has a verb argument to the left and one to the right, and
	one noun argument to the right. Like this: |- |. + 1 .
	-A dyadic conjunction has a verb argument to the left and one to the right, and
	one noun argument to the left and one to the right. Like this: 1 |- . + 1
	-An adverb to the left of a conjunction is together with it's verb the left argu-
	ment of the conjunction. Like this in the monadic case: + / |. + 1 . Like this in
	the dyadic case: 1 + / . + 1 .
An adverb together with its left verb forms a new verb. A conjunction together with its
right verb and it's left verb or adverb forms a new verb.
Monadic verbs, adverbs and conjunctions create verb trains like this:
|-        + /        |-        |- |. +        |-        + /  |. +        |-        1 
I use extra blanks to show the separation between verbs.
You always need at least one blank between two syntax elements.
A program is called a NOUN DEFINITION. It is written between curly brackets, like this 
{ some code } It can contain VERB DEFINITIONS, written between curly brackets where the
first bracket is immediately followed by ! . Like this {! some code } .
A verb definition can contain the NOUN FUNCTIONS - LEFT NOUN and RIGHT NOUN - denoted by
[ and ] . The Left Noun is a placeholder for the noun immediately preceding the verb defi- 
nition. The Right Noun is a placeholder for the noun immediately following the verb defini-
tion.
A verb definition containing only Right Nouns forms a monadic verb. A verb definition con-
taining a Left Noun forms a dyadic verb, even if there is no Right Noun. In that case you 
are forced to have a noun to the right of the verb definition and this noun is silently ig-
nored.
This is a verb definition of a monadic increment verb {! 1 + ] } . Example of use: 
{! 1 + ] } 2 .
This is a verb definition of a dyadic addition verb {! [ + ] } . Example of use : 
1 {! [ + ] } 2 .
Left and Right nouns within brackets are placeholders for the noun arguments to the first
verb definition enclosing the brackets.  In this example the Left noun is one and the Right
noun is two: 1 {! ( [ + ] ) + 1 } 2
There is no way to define adverbs and conjunctions in this mock parser. 
The verb definitions are very similar to the DIRECT DEFINITION FORM Ken Iverson used at his
lectures, see http://tinyurl.com/npx2umk .
In the present implementation the noun definitions does not add any functional value.
I will use this parser as an argument in a discussion about the J language. I have questio-
ned the syntax of tacit J many times. The answer is always - How should it be instead. This
is how I think it should be. Nearly exactly like normal scripted J. I want to add that if
you add a Left verb and a Right verb to this parser, you could add a verb definition from 
verbs which would possibly let you use higher order functions on these verbs.
You could of course also use Left verb and Right verb to define adverbs and conjunctions in
some kind of direct definition form.
The tacit J parser is in many ways similar to this parser. It does not have definitions of
adverbs and conjunctions either.
Some notes on implementation:
	- The parser creates a composite compiled .NET function which is executed when the 
	parsing process is finished. It is more like JIT-compiled than interpreted code.
	- The parser is written in F# and with #FParsec.
	- In the F# environment you can write code nearly as efficient as in C++.
	- The full .NET environment and the rest of the Windows environment, called Windows
	RT, is easily available from F#. Integration with the Windows environment is easy.
	- In JWithATwist an adverb or conjunction always returns a noun.

A sample session :
{ 1 }
1
{ 1 + 1 }
2
{ ( 1 - 1 ) + 1 }
1
{ ( 1 - 1 ) {! [ + ] } 1 }
1
{ { 1 - 1 } {! [ + ] } 1 }
1
{ + / 1 }
2
{ {! [ + ] } / 1 }
2
{ + / |. + 1 }
4
{ 2 + / . + 1 }
6
{ 2 + / . + |- |- 1 }
6
{ 2 + / . {! [ + ] } |- |- 1 }
6
{ 2 + / . + |- {! |- ] } 1 }
6
{ 2 + /- 3 }
5	
{ 5 {! ( [ + ] ) + [ + ] } 7 }
24
{ |- + / |- |. + + / |. + |- 5 }
-80"
                    ()
                |_ ->
                    ()
            ()
        |true ->
            ()

    printfn @"JWithATwist Mock Parser. Welcome to write an expression.
Remember - You always need a blank between two syntax elements."
    MockParser()


  
    //test (pchar '{') "{"
       
    //Console.ReadLine() |> ignore
    0 // return an integer exit code
