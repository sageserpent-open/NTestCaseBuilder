using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.FSharp.Collections;
using NUnit.Framework;

namespace NTestCaseBuilder.Examples
{
    [TestFixture]
    public class TestSortingAlgorithm
    {
        public class TestCase
        {
            public TestCase(Int32 leastItemInSequence, IEnumerable<UInt32> nonNegativeDeltas,
                            Permutation<Int32> permutation)
            {
                var originalMonotonicIncreasingSequence = new List<Int32>();

                var runningSum = leastItemInSequence;

                foreach (var nonNegativeDelta in nonNegativeDeltas)
                {
                    originalMonotonicIncreasingSequence.Add(runningSum);
                    runningSum += (Int32) nonNegativeDelta;
                }

                originalMonotonicIncreasingSequence.Add(runningSum);

                OriginalMonotonicIncreasingSequence = originalMonotonicIncreasingSequence;

                PermutedSequence = permutation(originalMonotonicIncreasingSequence);
            }

            public IEnumerable<Int32> PermutedSequence { get; set; }
            public IEnumerable<Int32> OriginalMonotonicIncreasingSequence { get; set; }
        }

        private TypedTestCaseEnumerableFactory<TestCase> BuildTestCaseFactory()
        {
            var factoryForLeastItemInSequence = TestVariableLevelEnumerableFactory.Create(Enumerable.Range(-3, 10));

            const int maximumNumberOfDeltas = 9;

            var factoryForNonNegativeDeltasAndPermutation =
                InterleavedTestCaseEnumerableFactory.Create(
                    from numberOfDeltas in Enumerable.Range(0, 1 + maximumNumberOfDeltas)
                    select BuildNonNegativeDeltasAndPermutationFactory(numberOfDeltas));

            return SynthesizedTestCaseEnumerableFactory.Create(factoryForLeastItemInSequence,
                                                               factoryForNonNegativeDeltasAndPermutation,
                                                               (leastItemInSequence, nonNegativeDeltasAndItsPermutation)
                                                               =>
                                                               new TestCase(leastItemInSequence,
                                                                            nonNegativeDeltasAndItsPermutation.Item1,
                                                                            nonNegativeDeltasAndItsPermutation.Item2));
        }

        private static TypedTestCaseEnumerableFactory<Tuple<FSharpList<UInt32>, Permutation<Int32>>>
            BuildNonNegativeDeltasAndPermutationFactory(int numberOfDeltas)
        {
            var factoryForNonNegativeDelta =
                TestVariableLevelEnumerableFactory.Create(from signedDelta in Enumerable.Range(0, 5)
                                                          select (UInt32) signedDelta);
            return
                SynthesizedTestCaseEnumerableFactory.CreateWithPermutation<UInt32, Int32>(
                    Enumerable.Repeat(factoryForNonNegativeDelta, numberOfDeltas));
        }

        public static void ParameterisedUnitTestForReassemblyOfPermutedMonotonicIncreasingSequenceBySortingAlgorithm(
            TestCase testCase)
        {
            var sortedSequence = SortingAlgorithmModule.Sort(testCase.PermutedSequence);

            Assert.IsTrue(sortedSequence.SequenceEqual(testCase.OriginalMonotonicIncreasingSequence));
        }

        [Test]
        public void TestReassemblyOfPermutedMonotonicIncreasingSequenceBySortingAlgorithm()
        {
            var factory = BuildTestCaseFactory();
            const Int32 strength = 3;

            factory.ExecuteParameterisedUnitTestForAllTypedTestCases(strength,
                                                                     ParameterisedUnitTestForReassemblyOfPermutedMonotonicIncreasingSequenceBySortingAlgorithm);
        }
    }
}