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
namespace JWithATwist.PerformanceTest

open System
open System.Collections
open System.Collections.Generic
open System.Text
open System.IO
open Microsoft.FSharp.Control
open Xunit
open Xunit.Abstractions
open JWithATwist.Base
open JWithATwist.Define
open JWithATwist.ParserDefinitions
open JWithATwist.ParserInterface
open JWithATwist.Parser
open FParsec
open Microsoft.Xunit.Performance

module PerformanceTest =

    [<MeasureInstructionsRetired(100)>]
    type ``Performance Tests`` () = 

        [<Benchmark>]
        [<InlineData(1000)>]
        member x.``Calibration`` (ms) =
            for iteration in Benchmark.Iterations do
                let g (iteration:BenchmarkIteration) =
                    use a = iteration.StartMeasurement() 
                    Async.RunSynchronously (async {do! Async.Sleep(ms)} )
                    ()
                g iteration



        [<Benchmark>]
        member x.``Iota`` () =
            let noun = JNounDefine  @"{ 30000000 }"
            let verbMonadic = JVerbMonadicDefine  @"{! |i. ] }"
            for iteration in Benchmark.Iterations do
                let g (iteration:BenchmarkIteration) =
                    use a = iteration.StartMeasurement() 
                    verbMonadic noun |> ignore
                    ()
                g iteration



        [<Benchmark>]
        member x.``Dyadic Rank`` () =
            let noun = JNounDefine  @"{ 500000 }"
            let verbMonadic = JVerbMonadicDefine  @"{! ( |i. ] ) + '/ 0 0 / |i. ] }"
            for iteration in Benchmark.Iterations do
                let g (iteration:BenchmarkIteration) =
                    use a = iteration.StartMeasurement() 
                    verbMonadic noun |> ignore
                    ()
                g iteration



        [<Benchmark>]
        member x.``Monadic Rank`` () =
            let noun = JNounDefine  @"{ |i. 700000 }"
            let verbMonadic = JVerbMonadicDefine  @"{! |- |'/ 0 / ] }"
            for iteration in Benchmark.Iterations do
                let g (iteration:BenchmarkIteration) =
                    use a = iteration.StartMeasurement() 
                    verbMonadic noun |> ignore
                    ()
                g iteration



        [<Benchmark>]
        member x.``Monadic Scalar function`` () =
            let noun = JNounDefine  @"{ |i. 15000000 }"
            let verbMonadic = JVerbMonadicDefine  @"{! |- ] }"
            for iteration in Benchmark.Iterations do
                let g (iteration:BenchmarkIteration) =
                    use a = iteration.StartMeasurement() 
                    verbMonadic noun |> ignore
                    ()
                g iteration


        [<Benchmark>]
        member x.``Dyadic Scalar function`` () =
            let noun = JNounDefine  @"{ |i. 10000000 }"
            let verbMonadic = JVerbMonadicDefine  @"{! ] + ] }"
            for iteration in Benchmark.Iterations do
                let g (iteration:BenchmarkIteration) =
                    use a = iteration.StartMeasurement() 
                    verbMonadic noun |> ignore
                    ()
                g iteration


        [<Benchmark>]
        member x.``Sort`` () =
            let noun = JNounDefine  @"{ |i. |- 2000000 }"
            let verbMonadic = JVerbMonadicDefine  @"{! ( |/: ] ) <- ] }"
            for iteration in Benchmark.Iterations do
                let g (iteration:BenchmarkIteration) =
                    use a = iteration.StartMeasurement() 
                    verbMonadic noun |> ignore
                    ()
                g iteration



        [<Benchmark>]
        member x.``Handling boxed data`` () =
            let xNoun = JNounDefine  @"{ 40000 $ 0 0 0 1 0 0 1 0 0 1 0 0 1 0   0 0 1 0 0 0 0 0 0 1 0 0 0 1   0 0 0 1 0 0 0 0 0 0 0 0 0 1   0 0 0 1 0 0 1 0 0 0 1 0 0 1 }"
            let yNoun = JNounDefine  @"{ 40000 $ 0 0 1 0 0 1 0 0 1 0 1 0 0 1   0 1 0 1 0 1 0 1 0 0 1 0 0 0   1 0 0 0 1 0 0 0 0 0 0 0 0 0   1 0 0 0 1 0 0 1 0 0 0 1 0 0 }"
            let verbDyadic = JVerbDyadicDefine  @"
{! 
    |> 
    ( |< |i. 0 ) {! |< ( |> [ ) , |> ] } /  
    ( 
        {! 
            ( |< |, -1 ) {! |< ( 1 + ( |i. 0 ) $ -1 <.- |> [ ) + |i. ] } \ ] 
        } 
        {! 
            ] - 0 , -1 >.- ] 
        } 
        {! 
            ( 1 = ] ) /:: |i. |# ] 
        }  
        [ , 1 
    ) 
    {! 
        |< 
        {! 
            ( |i. |- |# ] ) <- ] 
        } 
        {! 
            ( |_ 1 = 0 + \  ] ) * ] 
        } 
        {! 
            ( |i. |- |# ] ) <- ] 
        } 
        (  |>  [ ) <- ] 
    } '/ 0 1 / ] 
}"
            for iteration in Benchmark.Iterations do
                let g (iteration:BenchmarkIteration) =
                    use a = iteration.StartMeasurement() 
                    verbDyadic xNoun yNoun |> ignore
                    ()
                g iteration



