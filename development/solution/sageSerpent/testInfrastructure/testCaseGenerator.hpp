#ifndef _TEST_CASE_GENERATOR_HPP_
#define _TEST_CASE_GENERATOR_HPP_


namespace SageSerpent
{
    namespace TestInfrastructure
    {
        generic <typename TestCase>
        public interface class ITestCaseGenerator
        {
            System::Collections::Generic::IEnumerator<TestCase> ^CreateIterator();
        };
    }
}

#endif