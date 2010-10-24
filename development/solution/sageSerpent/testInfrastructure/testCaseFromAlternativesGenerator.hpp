#ifndef _TEST_CASE_FROM_ALTERNATIVES_GENERATOR_HPP_
#define _TEST_CASE_FROM_ALTERNATIVES_GENERATOR_HPP_

#include "testCaseGenerator.hpp"

#using "PowerCollections.dll"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        generic <typename TestCase>
        public ref class TestCaseFromAlternativesGenerator:
            ITestCaseGenerator<TestCase>
        {
        public:
            TestCaseFromAlternativesGenerator(Wintellect::PowerCollections::Set<ITestCaseGenerator<TestCase> ^> ^componentTestCaseGenerators);

            virtual System::Collections::Generic::IEnumerator<TestCase> ^CreateIterator();
        };
    }
}

#endif