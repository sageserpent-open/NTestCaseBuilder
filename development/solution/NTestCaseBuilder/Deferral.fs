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
            TypedFactoryImplementation<_> node
            :> ITypedFactory<_>

        static member Create (deferredFactory: Func<ITypedFactory<'TestCase>>) =
            let node =
                DeferralNode (fun () ->
                                let factory =
                                    deferredFactory.Invoke()
                                (factory :?> INodeWrapper).Node)
            TypedFactoryImplementation<'TestCase> node
            :> ITypedFactory<'TestCase>
