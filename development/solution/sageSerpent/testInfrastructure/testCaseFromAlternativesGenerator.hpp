#ifndef _TEST_CASE_FROM_ALTERNATIVES_GENERATOR_HPP_
#define _TEST_CASE_FROM_ALTERNATIVES_GENERATOR_HPP_

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

            virtual System::Collections::IEnumerator ^CreateIterator();
        };
    }
}

#endif