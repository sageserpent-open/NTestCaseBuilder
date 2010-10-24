#ifndef _TEST_CASE_FROM_COMBINATION_GENERATOR_HPP_
#define _TEST_CASE_FROM_COMBINATION_GENERATOR_HPP_

#include "testCaseGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        generic <typename TestCase>
        public ref class TestCaseFromCombinationGenerator:
            ITestCaseGenerator<TestCase>
        {
        public:
            TestCaseFromCombinationGenerator(System::Collections::Generic::IEnumerable<ITestCaseGenerator<TestCase> ^> ^componentTestCaseGenerators,
                                             System::Delegate combiningClosure);

            // NOTE: how about a design-by-contract smart pointer? It does the 'right-thing' as defined by Spec#.
            // This then leads to doing something similar in .NET via impersonation.

            virtual System::Collections::Generic::IEnumerator<TestCase> ^CreateIterator();
        };
    }
}

#endif