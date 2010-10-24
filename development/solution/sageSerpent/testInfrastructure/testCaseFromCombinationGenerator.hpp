#ifndef __TEST_CASE_FROM_COMBINATION_GENERATOR_HPP__
#define __TEST_CASE_FROM_COMBINATION_GENERATOR_HPP__

#include "testCaseGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        public ref class TestCaseFromCombinationGenerator:
            ITestCaseGenerator
        {
        public:
            TestCaseFromCombinationGenerator(System::Collections::Generic::IEnumerable<ITestCaseGenerator ^> ^componentTestCaseGenerators,
                                             System::Delegate ^combiningClosure);
                                             
            // NOTE: how about a design-by-contract smart pointer? It does the 'right-thing' as defined by Spec#.
            // This then leads to doing something similar in .NET via impersonation.

            // NOTE: want to be able to obtain an constructed test case generator interface, knowing that
            // all compatible test case generators will be included within as alternates. This is to do substitutability testing.

            virtual System::Collections::IEnumerator ^CreateIterator(System::UInt32 requestedDegreesOfFreedomForCombinationCoverage);
            
            virtual property System::UInt32 MaximumDegreesOfFreedom
            {
                System::UInt32 get();
            }

            virtual property System::Boolean IsDead
            {
                System::Boolean get();
            }
        };
    }
}

#endif