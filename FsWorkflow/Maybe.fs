namespace FsWorkflow

module Maybe = 
    type MaybeBuilder() =

        member this.Bind(m, f) = 
            Option.bind f m

        member this.Return(x) = 
            Some x

        member this.ReturnFrom(x) = 
            x

        member this.Zero() = 
            None

        member this.Combine (a,b) = 
            match a with
            | Some _ -> a  // if a is good, skip b
            | None -> b()  // if a is bad, run b

        member this.Delay(f) = 
            f

        member this.Run(f) = 
            f()

module MaybeDelayed =
    type Maybe<'a> = Maybe of (unit -> 'a option)

    type MaybeBuilder() =

        member this.Bind(m, f) = 
            Option.bind f m

        member this.Return(x) = 
            Some x

        member this.ReturnFrom(Maybe f) = 
            f()

        member this.Zero() = 
            None

        member this.Combine (a,b) = 
            match a with
            | Some _' -> a    // if a is good, skip b
            | None -> b()     // if a is bad, run b

        member this.Delay(f) = 
            f

        member this.Run(f) = 
            Maybe f

    let run (Maybe f) = f()

module MaybeLazy =
    type Maybe<'a> = Maybe of Lazy<'a option>

    type MaybeBuilder() =

        member this.Bind(m, f) = 
            Option.bind f m

        member this.Return(x) = 
            Some x

        member this.ReturnFrom(Maybe f) = 
            f.Force()

        member this.Zero() = 
            None

        member this.Combine (a,b) = 
            match a with
            | Some _' -> a    // if a is good, skip b
            | None -> b()     // if a is bad, run b

        member this.Delay(f) = 
            f

        member this.Run(f) = 
            Maybe (lazy f())

    let run (Maybe f) = f.Force()