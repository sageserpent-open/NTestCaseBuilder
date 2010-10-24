#include "testCaseFromCollectionGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        TestCaseFromCollectionGenerator::TestCaseFromCollectionGenerator(System::Collections::ICollection ^collection)
        {
            //throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }

        System::Collections::IEnumerator ^TestCaseFromCollectionGenerator::CreateIterator(System::UInt32 requestedDegreesOfFreedomForCombinationCoverage)
        {
            //throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
            return nullptr;
        }
        
        System::UInt32 TestCaseFromCollectionGenerator::MaximumDegreesOfFreedom::get()
        {
            return 0U;
        }
    }
}
