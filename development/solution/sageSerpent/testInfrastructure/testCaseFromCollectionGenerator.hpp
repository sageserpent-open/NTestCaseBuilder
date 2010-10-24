#ifndef _TEST_CASE_FROM_COLLECTION_GENERATOR_HPP_
#define _TEST_CASE_FROM_COLLECTION_GENERATOR_HPP_

#include "testCaseGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        public ref class TestCaseFromCollectionGenerator:
            ITestCaseGenerator
        {
        public:
            TestCaseFromCollectionGenerator(System::Collections::ICollection ^collection);

            virtual System::Collections::IEnumerator ^CreateIterator();
        };
    }
}

#endif