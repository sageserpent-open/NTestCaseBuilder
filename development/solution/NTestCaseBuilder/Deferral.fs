namespace NTestCaseBuilder
    open System
    open NodeExtensions

    type Deferral =
        static member Create (deferredFactory: Func<IFactory>) =
            let node =
                DeferralNode (fun () ->
                                let factory =
                                    deferredFactory.Invoke()
                                (factory :?> INodeWrapper).Node)
            TypedFactoryImplementation<'SingletonTestCase> node
            :> ITypedFactory<_>
