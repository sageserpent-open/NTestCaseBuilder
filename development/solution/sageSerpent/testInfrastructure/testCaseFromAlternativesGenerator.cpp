#include "testCaseFromAlternativesGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        TestCaseFromAlternativesGenerator::TestCaseFromAlternativesGenerator(Wintellect::PowerCollections::Set<ITestCaseGenerator ^> ^componentTestCaseGenerators)
        {
            //throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }

        System::Collections::IEnumerator ^TestCaseFromAlternativesGenerator::CreateIterator()
        {
            //throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
            return nullptr;
        }
        
        System::UInt32 TestCaseFromAlternativesGenerator::MaximumDegreesOfFreedom::get()
        {
            return 0U;
        }
    }
}
