#ifndef __TEST_CASE_GENERATOR_HPP__
#define __TEST_CASE_GENERATOR_HPP__


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