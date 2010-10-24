#ifndef __TEST_CASE_FROM_COLLECTION_GENERATOR_HPP__
#define __TEST_CASE_FROM_COLLECTION_GENERATOR_HPP__

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