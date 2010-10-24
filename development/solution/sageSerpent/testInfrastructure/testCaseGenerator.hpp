#ifndef __TEST_CASE_GENERATOR_HPP__
#define __TEST_CASE_GENERATOR_HPP__


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