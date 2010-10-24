#ifndef __TEST_CASE_FROM_ALTERNATIVES_GENERATOR_HPP__
#define __TEST_CASE_FROM_ALTERNATIVES_GENERATOR_HPP__

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