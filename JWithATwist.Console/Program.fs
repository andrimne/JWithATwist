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

open JWithATwist
open JWithATwist.Parser
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
JWithATwist
JWithATwist is a programming language under development. It has many similarities with J.
You can look at www.jsoftware.com to find out what J is.
You can find the documentation at https://github.com/andrimne/JWithATwist.DocBook/blob/master/target/en/JWithATwistReferenceManual.pdf
"
                    ()
                |_ ->
                    ()
            ()
        |true ->
            ()

    printfn @"JWithATwist. Welcome to write an expression.
Remember - You always need a blank between two syntax elements."
    Parser()


  
    //test (pchar '{') "{"
       
    //Console.ReadLine() |> ignore
    0 // return an integer exit code
