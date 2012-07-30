module SageSerpent.NTestCaseBuilder.SingletonTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure


    let Create (singletonTestCase: 'SingletonTestCase) =
        let node =
            SageSerpent.NTestCaseBuilder.SingletonNode singletonTestCase
        TypedTestCaseEnumerableFactory<'SingletonTestCase> node
