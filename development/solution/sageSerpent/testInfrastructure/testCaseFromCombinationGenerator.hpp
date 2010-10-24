#ifndef _TEST_CASE_FROM_COMBINATION_GENERATOR_HPP_
#define _TEST_CASE_FROM_COMBINATION_GENERATOR_HPP_

#include "testCaseGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        public ref class TestCaseFromCombinationGenerator:
            ITestCaseGenerator
        {
        public:
            TestCaseFromCombinationGenerator(System::Collections::Generic::IEnumerable<ITestCaseGenerator ^> ^componentTestCaseGenerators);

            virtual System::Collections::IEnumerator ^CreateIterator();
        };
    }
}

#endif