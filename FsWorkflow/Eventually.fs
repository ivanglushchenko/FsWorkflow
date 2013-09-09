namespace FsWorkflow

module Eventually =
    // Computations that can be run step by step
    type Eventually<'T> =
        | Done of 'T
        | NotYetDone of (unit -> Eventually<'T>)

    // The bind for the computations. Append 'func' to the
    // computation.
    let rec bind func expr =
        match expr with
        | Done value -> NotYetDone (fun () -> func value)
        | NotYetDone work -> NotYetDone (fun () -> bind func (work()))

    // Return the final value wrapped in the Eventually type.
    let result value = Done value

    type OkOrException<'T> =
    | Ok of 'T
    | Exception of System.Exception

    // The catch for the computations. Stitch try/with throughout
    // the computation, and return the overall result as an OkOrException.
    let rec catch expr =
        match expr with
        | Done value -> result (Ok value)
        | NotYetDone work ->
            NotYetDone (fun () ->
            let res = try Ok(work()) with | exn -> Exception exn
            match res with
            | Ok cont -> catch cont // note, a tailcall
            | Exception exn -> result (Exception exn))

    // The delay operator.
    let delay func = NotYetDone (fun () -> func())

    // The stepping action for the computations.
    let step expr =
        match expr with
        | Done _ -> expr
        | NotYetDone func -> func ()

    // The rest of the operations are boilerplate.
    // The tryFinally operator.
    // This is boilerplate in terms of "result", "catch", and "bind".
    let tryFinally expr compensation =
        catch (expr)
        |> bind (fun res -> compensation();
                            match res with
                            | Ok value -> result value
                            | Exception exn -> raise exn)

    // The tryWith operator.
    // This is boilerplate in terms of "result", "catch", and "bind".
    let tryWith exn handler =
        catch exn
        |> bind (function Ok value -> result value | Exception exn -> handler exn)

    // The whileLoop operator.
    // This is boilerplate in terms of "result" and "bind".
    let rec whileLoop pred body =
        if pred() then body |> bind (fun _ -> whileLoop pred body)
        else result ()

    // The sequential composition operator.
    // This is boilerplate in terms of "result" and "bind".
    let combine expr1 expr2 =
        expr1 |> bind (fun () -> expr2)
    
    // The using operator.
    let using (resource: #System.IDisposable) func =
        tryFinally (func resource) (fun () -> resource.Dispose())

    // The forLoop operator.
    // This is boilerplate in terms of "catch", "result", and "bind".
    let forLoop (collection:seq<_>) func =
        let ie = collection.GetEnumerator()
        tryFinally (whileLoop (fun () -> ie.MoveNext())
                     (delay (fun () -> let value = ie.Current in func value)))
                     (fun () -> ie.Dispose())

    // The builder class.
    type EventuallyBuilder() =
        member x.Bind(comp, func) = bind func comp
        member x.Return(value) = result value
        member x.ReturnFrom(value) = value
        member x.Combine(expr1, expr2) = combine expr1 expr2
        member x.Delay(func) = delay func
        member x.Zero() = result ()
        //member x.TryWith(expr, handler) = tryWith expr handler
        //member x.TryFinally(expr, compensation) = tryFinally expr compensation
        //member x.For(coll:seq<_>, func) = forLoop coll func
        //member x.Using(resource, expr) = using resource expr
        member x.While(pred, body) = whileLoop pred body

    let eventually = new EventuallyBuilder()

module EventuallyTest =
    open Eventually

    let comp =
        eventually { //for x in 1 .. 2 do
                     //   printfn " x = %d" x
                     let b = true
                     while b do
                        printfn "b is true"
                     return 3 + 4 }

    // Try the remaining lines in F# interactive to see how this 
    // computation expression works in practice.
    let step x = Eventually.step x

    let test() =
        printfn "%s" "t1:"
        printfn "%A" (comp |> step)
        printfn "%s" "t2:"
        printfn "%A" (comp |> step |> step)
        printfn "%s" "t3:"
        printfn "%A" (comp |> step |> step |> step |> step |> step |> step)
        printfn "%s" "t4:"
        printfn "%A" (comp |> step |> step |> step |> step |> step |> step |> step |> step)