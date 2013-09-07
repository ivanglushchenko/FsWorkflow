open FsWorkflow

[<EntryPoint>]
let main argv = 
    //let ret AttemptTest.test()
    let ret = DistributionTest.test()

    printfn "%A" ret
    0 // return an integer exit code