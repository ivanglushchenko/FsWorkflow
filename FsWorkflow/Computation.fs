namespace FsWorkflow

module Computation =
    type Result<'v> =
        | Value of 'v
        | Nothing
        | Error of System.Exception

    type Computation<'v> = (unit -> Result<'v>)

    let inline value v = fun () -> Value v

    let inline nothing() = fun () -> Nothing

    let inline error exc = fun () -> Error exc

    // M<'T> -> 'T
    let inline run cmp = cmp()
    
    // M<'T> * ('T -> M<'U>) -> M<'U>
    let bind cmp rest = 
        match run cmp with 
        | Value v -> rest v
        | Nothing -> nothing()
        | Error e -> error e

    // (unit -> M<'T>) -> M<'T>
    let delay f = fun () -> try f() |> run with | exc -> Error exc

    // M<'T> * M<'T> -> M<'T>
    let combine cmp1 cmp2 = fun () ->
        match run cmp1 with
        | Value v -> run cmp2
        | res -> res

    // (unit -> bool) * M<'T> -> M<'T>
    let rec whileLoop pred (body : Computation<'v>) : Computation<unit> =
        if pred() 
        then bind body (fun _ -> whileLoop pred body) 
        else nothing()

    // unit -> M<'T>
    let zero() = nothing

    // The builder class.
    type ComputationBuilder() =
        // Called for let! and do! in computation expressions.
        member x.Bind(cmp, rest) = bind cmp rest
        // Wraps a computation expression as a function.
        member x.Delay(cmp) = delay cmp
        //Called for return in computation expressions.
        member x.Return(v) = value v
        // Called for return! in computation expressions.
        member x.ReturnFrom(v) = v
        // Called for sequencing in computation expressions.
        member x.Combine(cmp1, cmp2) = combine cmp1 cmp2
        // Called for while...do expressions in computation expressions.
        member x.While(pred, body) = whileLoop pred body
        // Called for empty else branches of if...then expressions in computation expressions.
        member x.Zero() = zero

    let cmp = new ComputationBuilder()

module ComputationTest =
    open Computation

    let test() =
        let c1 = cmp { return 3 }
        let format = cmp { return "{0}" }
        let str = cmp { printfn "starting computation..."
                        let! arg0 = c1
                        let! arg1 = format
                        let s = System.String.Format(arg1, arg0)
                        let s2 = s + "_ADD"
                        let b = false 
                        //while b do
                        //    printfn "b is true"
                        return s2 }

        printfn "%A" str
        let res = Computation.run str
        printfn "%A" res

        let c1 = cmp { failwith "some error"
                       return 3 }
        let format = cmp { return "{0}" }
        let str = cmp { printfn "starting computation..."
                        let! arg0 = c1
                        let! arg1 = format
                        let s = System.String.Format(arg1, arg0)
                        let s2 = s + "_ADD" 
                        return s2 }

        printfn "%A" str
        let res = Computation.run str
        printfn "%A" res