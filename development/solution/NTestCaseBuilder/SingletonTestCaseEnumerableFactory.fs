module NTestCaseBuilder.SingletonTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure


    let Create (singletonTestCase: 'SingletonTestCase) =
        let node =
            NTestCaseBuilder.SingletonNode singletonTestCase
        TypedTestCaseEnumerableFactory<'SingletonTestCase> node
