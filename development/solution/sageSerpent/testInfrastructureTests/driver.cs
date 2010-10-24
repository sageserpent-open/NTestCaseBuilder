namespace SageSerpent.TestInfrastructureTests
{
    class Driver
    {
        public static void Main()
        {
            System.Reflection.Assembly assemblyForTheseTests = System.Reflection.Assembly.GetExecutingAssembly();

            NUnit.Core.TestSuite testSuite = new NUnit.Core.TestSuiteBuilder().Build(assemblyForTheseTests.Location);

            testSuite.Run(new NUnit.Core.NullListener());
        }
    }
}
