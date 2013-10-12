module NTestCaseBuilder.Singleton

    open System.Collections
    open System
    open SageSerpent.Infrastructure
    open NodeExtensions

    let Create (singletonTestCase: 'SingletonTestCase) =
        let node =
            SingletonNode singletonTestCase
        TypedFactoryImplementation<'SingletonTestCase> node
        :> ITypedFactory<_>
