module SageSerpent.Infrastructure.NUnitTestSuiteDriver

    open NUnit.ConsoleRunner
    
    let RunAllTests () =
        let assemblyForTheseTests = System.Reflection.Assembly.GetEntryAssembly()
        Runner.Main([| assemblyForTheseTests.Location |]) |> ignore
        
