#ifndef __TEST_CASE_FROM_ALTERNATIVES_GENERATOR_HPP__
#define __TEST_CASE_FROM_ALTERNATIVES_GENERATOR_HPP__

#include "testCaseGenerator.hpp"

#using "PowerCollections.dll"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        public ref class TestCaseFromAlternativesGenerator:
            ITestCaseGenerator
        {
        public:
            TestCaseFromAlternativesGenerator(Wintellect::PowerCollections::Set<ITestCaseGenerator ^> ^componentTestCaseGenerators);

            virtual System::Collections::IEnumerator ^CreateIterator(System::UInt32 requestedDegreesOfFreedomForCombinationCoverage);
            
            virtual property System::UInt32 MaximumDegreesOfFreedom
            {
                System::UInt32 get();
            }
        };
    }
}

#endif