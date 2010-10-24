#ifndef __TEST_CASE_FROM_COMBINATION_GENERATOR_HPP__
#define __TEST_CASE_FROM_COMBINATION_GENERATOR_HPP__

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
            TestCaseFromCombinationGenerator(System::Collections::IEnumerable ^componentTestCaseGenerators,
                                             System::Delegate combiningClosure);
                                             
            // NOTE: now we have two type-tests at runtime - does the collection contain test case generators of any kind,
            // and if so, what is the type of each test case generator's type parameter?

            // NOTE: how about a design-by-contract smart pointer? It does the 'right-thing' as defined by Spec#.
            // This then leads to doing something similar in .NET via impersonation.

            // NOTE: want to be able to obtain an constructed test case generator interface, knowing that
            // all compatible test case generators will be included within as alternates. This is to do substitutability testing.

            virtual System::Collections::Generic::IEnumerator<TestCase> ^CreateIterator();
        };
    }
}

#endif