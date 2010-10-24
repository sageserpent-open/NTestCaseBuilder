#ifndef __TEST_CASE_FROM_COLLECTION_GENERATOR_HPP__
#define __TEST_CASE_FROM_COLLECTION_GENERATOR_HPP__

#include "testCaseGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        generic <typename TestCase>
        public ref class TestCaseFromCollectionGenerator:
            ITestCaseGenerator<TestCase>
        {
        public:
            TestCaseFromCollectionGenerator(System::Collections::Generic::ICollection<TestCase> ^collection);

            virtual System::Collections::Generic::IEnumerator<TestCase> ^CreateIterator();
        };
    }
}

#endif