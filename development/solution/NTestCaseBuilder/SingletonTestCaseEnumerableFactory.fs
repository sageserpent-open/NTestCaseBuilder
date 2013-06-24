module NTestCaseBuilder.SingletonTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure
    open NodeExtensions

    let Create (singletonTestCase: 'SingletonTestCase) =
        let node =
            SingletonNode singletonTestCase
        TypedTestCaseEnumerableFactory<'SingletonTestCase> node
