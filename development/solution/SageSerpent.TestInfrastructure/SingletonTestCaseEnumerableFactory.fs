module SageSerpent.TestInfrastructure.SingletonTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure
    
    
    let Create singletonTestCase =
        let node =
            SageSerpent.TestInfrastructure.SingletonNode singletonTestCase
        TestCaseEnumerableFactoryCommonImplementation node :> ITestCaseEnumerableFactory
