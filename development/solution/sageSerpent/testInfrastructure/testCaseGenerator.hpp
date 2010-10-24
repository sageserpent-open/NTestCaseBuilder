#ifndef _TEST_CASE_GENERATOR_HPP_
#define _TEST_CASE_GENERATOR_HPP_


namespace SageSerpent
{
    namespace TestInfrastructure
    {
        public interface class ITestCaseGenerator
        {
            System::Collections::IEnumerator ^CreateIterator();
        };
    }
}

#endif