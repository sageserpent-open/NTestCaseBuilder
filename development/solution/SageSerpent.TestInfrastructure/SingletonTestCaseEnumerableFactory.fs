module SageSerpent.TestInfrastructure.SingletonTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure
    
    
    let Create (singletonTestCase: 'SingletonTestCase) =
        let node =
            SageSerpent.TestInfrastructure.SingletonNode singletonTestCase
        TypedTestCaseEnumerableFactory<'SingletonTestCase> node
