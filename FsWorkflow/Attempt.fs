namespace FsWorkflow

module Attempt =
    type Attempt<'T> = (unit -> 'T option)

    let succeed x = (fun () -> Some(x))
    let fail = (fun () -> None)
    let runAttempt (a : Attempt<'T>) = a()
    let bind p rest = match runAttempt p with None -> fail | Some r -> (rest r)
    let delay f = (fun () -> runAttempt (f()))
    let combine p1 p2 = (fun () -> match p1() with None -> p2() | res -> res)

    type AttemptBuilder() =
        /// Used to de-sugar uses of 'let!' inside computation expressions.
        member b.Bind(p, rest) = bind p rest
        /// Delays the construction of an attempt until just before it is executed
        member b.Delay(f) = delay f
        /// Used to de-sugar uses of 'return' inside computation expressions.
        member b.Return(x) = succeed x
        /// Used to de-sugar uses of 'return!' inside computation expressions.
        member b.ReturnFrom(x : Attempt<'T>) = x
        /// Used to de-sugar uses of 'c1; c2' inside computation expressions.
        member b.Combine(p1 : Attempt<'T>, p2 : Attempt<'T>) = combine p1 p2
        /// Used to de-sugar uses of 'if .. then ..' inside computation expressions when
        /// the 'else' branch is empty
        member b.Zero() = fail

    let attempt = new AttemptBuilder()

module AttemptTest =
    open Attempt

    let test() =
        let printThenSeven = attempt {  printf "starting..."
                                        return 3 + 4 }
        printfn "%A" (runAttempt printThenSeven)
        printfn "%A" (runAttempt printThenSeven)

        let failIfBig n = attempt {if n > 1000 then return! fail else return n}
        let sumIfBothSmall (inp1, inp2) =
            attempt {   let! n1 = failIfBig inp1
                        printfn "Hey, n1 was small!"
                        let! n2 = failIfBig inp2
                        printfn "n2 was also small!"
                        let sum = n1 + n2
                        return sum }
        runAttempt(sumIfBothSmall (999, 999))
