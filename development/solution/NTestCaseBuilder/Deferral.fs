namespace NTestCaseBuilder
    open System
    open NodeExtensions

    type Deferral =
        static member Create (deferredFactory: Func<IFactory>) =
            DeferralNode (fun () ->
                            let factory =
                                deferredFactory.Invoke()
                            (factory :?> INodeWrapper).Node)

