#ifndef __TEST_CASE_GENERATOR_HPP__
#define __TEST_CASE_GENERATOR_HPP__


namespace SageSerpent
{
    namespace TestInfrastructure
    {
        /// <remarks>Bad Rellin</remarks>
        public interface class ITestCaseGenerator
        {
            // TODO - convert to correct XML documentation.
            // 'requestedDegreesOfFreedomForCombinationCoverage' may be greater than or equal to the 'MaximumDegreesOfFreedom' property
            // - this is rather like the situation where collisions during test case generation prevent full coverage of all combinations.
            // All that happens is that the test case generator does its best.
            // If 'requestedDegreesOfFreedomForCombinationCoverage' is zero, this indicates that 'MaximumDegreesOfFreedom' combinations should be
            // generated.
            System::Collections::IEnumerator ^CreateIterator(System::UInt32 requestedDegreesOfFreedomForCombinationCoverage);
            
            property System::UInt32 MaximumDegreesOfFreedom
            {
                System::UInt32 get();
            }
        };
    }
}

#endif