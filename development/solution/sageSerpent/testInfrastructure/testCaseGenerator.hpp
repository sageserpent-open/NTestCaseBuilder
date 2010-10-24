#ifndef __TEST_CASE_GENERATOR_HPP__
#define __TEST_CASE_GENERATOR_HPP__


namespace SageSerpent
{
    namespace TestInfrastructure
    {
        /// <remarks>Bad Rellin</remarks>
        public interface class ITestCaseGenerator
        {
            // TODO - convert to correct XML documentation.
            // 'requestedDegreesOfFreedomForCombinationCoverage' may be greater than or equal to the 'MaximumDegreesOfFreedom' property
            // - this is rather like the situation where collisions during test case generation prevent full coverage of all combinations.
            // All that happens is that the test case generator does its best.
            // If 'requestedDegreesOfFreedomForCombinationCoverage' is zero, this indicates that 'MaximumDegreesOfFreedom' combinations should be
            // generated.

            // NOTE: it is possible to have a subtree of test case generators such that the subtree root generator cannot create any test cases
            // whatsoever: call the root a 'dead generator'. This is caused inductively by:-
            // 1. A collection-based test case generator working on an empty collection.
            // 2. A combination-based test case generator with at least one child subtree whose root is a dead generator.
            // 3. An alternatives-based test case generator with all of its child subtrees being dead generators.

            // TODO - it should be possible to query a test case generator for the set of unrelated maximal subtrees that
            // are dead. In the worst case, this may yield a singleton set that is the generator itself.

            // TODO - in the same way that accidental collisions between composed test cases cause the generator to reconsider
            // the combinations to gaurantee coverage, if a composed test case's constructor throws a precondition failure
            // exception, the generator should take the same kind of evasive action and try different combinations to get coverage.


            System::Collections::IEnumerator ^CreateIterator(System::UInt32 requestedDegreesOfFreedomForCombinationCoverage);
            
            property System::UInt32 MaximumDegreesOfFreedom
            {
                System::UInt32 get();
            }

            property System::Boolean IsDead
            {
                System::Boolean get();
            }
        };
    }
}

#endif