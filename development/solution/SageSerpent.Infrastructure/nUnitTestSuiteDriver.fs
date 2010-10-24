#light

module SageSerpent.Infrastructure.NUnitTestSuiteDriver

    open NUnit.Core
    
    let RunAllTests () =
        let assemblyForTheseTests = System.Reflection.Assembly.GetEntryAssembly()
        let testSuite = (TestSuiteBuilder()).Build(assemblyForTheseTests.Location)
        testSuite.Run(NullListener()) |> ignore;
        
