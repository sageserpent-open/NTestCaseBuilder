using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;
using System.Text;
using NUnit.Framework;
using SageSerpent.Infrastructure;
using SageSerpent.TestInfrastructure;
using Wintellect.PowerCollections;

namespace SageSerpent.TestInfrastructureTests
{
    [TestFixture]
    public class TestCaseGeneratorsTestFixture
    {
        [Test]
        public void TestCorrectOrderingOfAtomicTestCasesWithinEachOutputTestCase()
        {
            Random randomChoice = new Random(0);

            ForABunchOfTestCaseGenerators(
                delegate(ITestCaseGenerator testCaseGeneratorWithoutCollisions, ITestCaseGenerator testCaseGeneratorWithCollisions)
                    {
                        UInt32 requestedDegreesOfFreedomForCombinationCoverage =
                            (UInt32) randomChoice.Next((Int32) (testCaseGeneratorWithoutCollisions.MaximumDegreesOfFreedom + 1U));

                        IEnumerator testCaseIterator =
                            testCaseGeneratorWithoutCollisions.CreateIterator(requestedDegreesOfFreedomForCombinationCoverage);

                        SetOfSequencesOfOwningBags possibleSequencesOfOwningBags =
                            ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGeneratorWithoutCollisions).PossibleSequencesOfOwningBags();

                        while (testCaseIterator.MoveNext())
                        {
                            AbstractTestCase testCase = (AbstractTestCase) testCaseIterator.Current;

                            IEnumerable<Bag<AtomicTestCase>> owningBagsThatContributedToThisTestCase = SequenceOfOwningBags(testCase);

                            Assert.IsTrue(possibleSequencesOfOwningBags.Contains(owningBagsThatContributedToThisTestCase));
                        }
                    });
        }

        private static SetOfCombinations MakeAtomicTestCasesCombinations(ITestCaseGenerator testCaseGenerator, Random randomChoice)
        {
            UInt32 numberOfCombinationsLeftToTry = 100U;
            // NOTE: we may get some repeated combinations, or
            // permutations of the same combination. This is OK,
            // as long as it doesn't happen too often or as long as
            // it only occurs because there aren't enough combinations
            // to fulfill the test's expectations.

            SetOfCombinations atomicTestCasesCombinations = new SetOfCombinations();


            while (numberOfCombinationsLeftToTry-- != 0U)
            {
                Set<AtomicTestCase> atomicTestCasesCombination =
                    new Set<AtomicTestCase>(
                        ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).PickFeasibleCombinationOfAtomicTestCases(randomChoice));

                atomicTestCasesCombinations.Add(atomicTestCasesCombination);
            }

            return atomicTestCasesCombinations;
        }

        private static MultiDictionary<AtomicTestCase, Set<AtomicTestCase>> MakeAtomicStateToContainingCombinationMultiMap(
            SetOfCombinations atomicTestCasesCombinations)
        {
            MultiDictionary<AtomicTestCase, Set<AtomicTestCase>> atomicStateToContainingCombinationMultiMap =
                new MultiDictionary<AtomicTestCase, Set<AtomicTestCase>>(false);

            Algorithms.ForEach(atomicTestCasesCombinations, delegate(Set<AtomicTestCase> combination)
                {
                    Algorithms.ForEach(combination, delegate(AtomicTestCase
                                                        testCase)
                        { atomicStateToContainingCombinationMultiMap.Add(testCase, combination); });
                });
            return atomicStateToContainingCombinationMultiMap;
        }

        private static UInt32 ChooseHowManyDegreesOfFreedomRequiredToAllowAllCombinationsToBeCovered(SetOfCombinations atomicTestCasesCombinations, Random randomChoice, ITestCaseGenerator testCaseGenerator)
        {
            UInt32 maximumCombinationWidth = Algorithms.Maximum(Algorithms.Convert(atomicTestCasesCombinations, delegate(Set<AtomicTestCase>
                                                                                                                    combination)
                { return (UInt32) combination.Count; }));

            return (UInt32) randomChoice.Next((Int32) maximumCombinationWidth, (Int32) (testCaseGenerator.MaximumDegreesOfFreedom + 1U));
        }

        private static void StrikeOffCombinationsCoveredByTestCase(AbstractTestCase abstractTestCaseCollisions,
                                                                   MultiDictionary<AtomicTestCase, Set<AtomicTestCase>>
                                                                       atomicStateToContainingCombinationMultiMap,
                                                                   SetOfCombinations atomicTestCasesCombinations)
        {
            IDictionary<Set<AtomicTestCase>, UInt32> combinationToNumberOfAtomicTestCaseComponentsLeftToFind =
                new Dictionary<Set<AtomicTestCase>, UInt32>();

            Algorithms.ForEach(atomicTestCasesCombinations,
                               delegate(Set<AtomicTestCase> combination) { combinationToNumberOfAtomicTestCaseComponentsLeftToFind[combination] = (UInt32) combination.Count; });

            Algorithms.ForEach(abstractTestCaseCollisions.AtomicTestCases(), delegate(AtomicTestCase atomicTestCase)
                {
                    if (atomicStateToContainingCombinationMultiMap.ContainsKey(atomicTestCase))
                    {
                        Algorithms.ForEach(atomicStateToContainingCombinationMultiMap[atomicTestCase], delegate(Set<AtomicTestCase>
                                                                                                           combination)
                            {
                                --
                                    combinationToNumberOfAtomicTestCaseComponentsLeftToFind[combination];
                            });
                    }
                });

            Algorithms.ForEach(combinationToNumberOfAtomicTestCaseComponentsLeftToFind, delegate(KeyValuePair<Set<AtomicTestCase>, UInt32>
                                                                                            combinationAndNumberOfAtomicTestCaseComponentsLeftToFindPair)
                {
                    if (combinationAndNumberOfAtomicTestCaseComponentsLeftToFindPair.Value == 0U)
                    {
                        atomicTestCasesCombinations.Remove(combinationAndNumberOfAtomicTestCaseComponentsLeftToFindPair.Key);
                    }
                });
        }

        [Test]
        public void TestCoverageOfNWayCombinationsOfAtomicTestCasesOverAllOutputTestCases()
        {
            Random randomChoice = new Random(0);

            ForABunchOfTestCaseGenerators(
                delegate(ITestCaseGenerator testCaseGeneratorWithoutCollisions, ITestCaseGenerator testCaseGeneratorWithCollisions)
                {
                    SetOfCombinations atomicTestCasesCombinations =
                        MakeAtomicTestCasesCombinations(testCaseGeneratorWithoutCollisions, randomChoice);

                    MultiDictionary<AtomicTestCase, Set<AtomicTestCase>> atomicStateToContainingCombinationMultiMap =
                        MakeAtomicStateToContainingCombinationMultiMap(atomicTestCasesCombinations);

                    UInt32 numberOfDegreesOfFreedom =
                        ChooseHowManyDegreesOfFreedomRequiredToAllowAllCombinationsToBeCovered(atomicTestCasesCombinations, randomChoice,
                                                                                               testCaseGeneratorWithoutCollisions);

                    {
                        IEnumerator testCaseEnumerator = testCaseGeneratorWithoutCollisions.CreateIterator(numberOfDegreesOfFreedom);

                        CheckThatAllCombinationsAreCovered(testCaseEnumerator, atomicStateToContainingCombinationMultiMap,
                                                             atomicTestCasesCombinations);
                    }

                    {
                        IEnumerator testCaseEnumerator = testCaseGeneratorWithCollisions.CreateIterator(numberOfDegreesOfFreedom);

                        CheckThatAllCombinationsAreCovered(testCaseEnumerator, atomicStateToContainingCombinationMultiMap,
                                                             atomicTestCasesCombinations);
                    }
                });
        }

        private static void CheckThatAllCombinationsAreCovered(IEnumerator testCaseEnumerator, MultiDictionary<AtomicTestCase, Set<AtomicTestCase>> atomicStateToContainingCombinationMultiMap, SetOfCombinations atomicTestCasesCombinations)
        {
            SetOfCombinations atomicTestCasesCombinationsToBeStruckOff = new SetOfCombinations(atomicTestCasesCombinations);

            while (testCaseEnumerator.MoveNext())
            {
                AbstractTestCase testCase = (AbstractTestCase) testCaseEnumerator.Current;

                StrikeOffCombinationsCoveredByTestCase(testCase, atomicStateToContainingCombinationMultiMap, atomicTestCasesCombinationsToBeStruckOff);
            }

            Assert.IsTrue(atomicTestCasesCombinationsToBeStruckOff.Count == 0U);
        }

        private static SetOfCombinations PickTheLargestCombinationsOfSizeNotExceeding(IEnumerable<AtomicTestCase> testCases,
                                                                                      UInt32 maximumCombinationWidth)
        {
            IList<AtomicTestCase> testCasesInUsableForm = new List<AtomicTestCase>(testCases);
            return PickAllCombinationsOfSize(testCasesInUsableForm, Math.Min(maximumCombinationWidth, (UInt32) testCasesInUsableForm.Count));
        }

        private static SetOfCombinations PickAllCombinationsOfSize(IList<AtomicTestCase> testCases, UInt32 combinationSize)
        {
            if (!(testCases.Count >= combinationSize))
            {
                throw new PreconditionViolation("Attempt to pick out combination larger than size of source collection.");
            }

            if (0U == combinationSize)
            {
                SetOfCombinations trivialResult = new SetOfCombinations();

                trivialResult.Add(new Set<AtomicTestCase>());

                return trivialResult;
            }

            if (testCases.Count == combinationSize)
            {
                SetOfCombinations trivialResult = new SetOfCombinations();

                trivialResult.Add(new Set<AtomicTestCase>(testCases));

                return trivialResult;
            }

            // Alas, neither [head::tail] nor [head:tail] ...

            IList<AtomicTestCase> tailTestCases = Algorithms.Range(testCases, 1, testCases.Count - 1);

            AtomicTestCase headTestCase = testCases[0];

            SetOfCombinations result = new SetOfCombinations();

            result.AddMany(
                Algorithms.Convert(PickAllCombinationsOfSize(tailTestCases, combinationSize - 1U), delegate(Set<AtomicTestCase> combination)
                    {
                        Set<AtomicTestCase> value = combination.Clone();

                        value.Add(headTestCase);

                        return value;
                    }));

            result.UnionWith(PickAllCombinationsOfSize(tailTestCases, combinationSize));

            return result;
        }

        [Test]
        public void TestOptimalityOfCoverageOfAllNWayCombinationsOfAtomicTestCasesOverAllOutputTestCases()
        {
            Random randomChoice = new Random(0);

            ForABunchOfTestCaseGenerators(
                delegate(ITestCaseGenerator testCaseGeneratorWithoutCollisions, ITestCaseGenerator testCaseGeneratorWithCollisions)
                {
                    UInt32 maximumDegreesOfFreedom =
                        (UInt32)randomChoice.Next((Int32)testCaseGeneratorWithoutCollisions.MaximumDegreesOfFreedom) + 1U;

                    Set<AbstractTestCase> testCases = new Set<AbstractTestCase>();

                    IEnumerator testCaseEnumerator = testCaseGeneratorWithoutCollisions.CreateIterator(maximumDegreesOfFreedom);

                    while (testCaseEnumerator.MoveNext())
                    {
                        testCases.Add((AbstractTestCase)testCaseEnumerator.Current);
                    }

                    AbstractTestCase testCaseToBeExcluded = Algorithms.RandomSubset(testCases, 1, randomChoice)[0];

                    SetOfCombinations atomicTestCasesCombinations =
                        PickTheLargestCombinationsOfSizeNotExceeding(testCaseToBeExcluded.AtomicTestCases(), maximumDegreesOfFreedom);

                    MultiDictionary<AtomicTestCase, Set<AtomicTestCase>> atomicStateToContainingCombinationMultiMap =
                        MakeAtomicStateToContainingCombinationMultiMap(atomicTestCasesCombinations);

                    {
                        IEnumerator testCaseEnumeratorTwo = testCaseGeneratorWithoutCollisions.CreateIterator(maximumDegreesOfFreedom);

                        CheckThatAtLeastOneCombinationIsNotCovered(testCaseToBeExcluded, testCaseEnumeratorTwo, atomicStateToContainingCombinationMultiMap,
                                                                   atomicTestCasesCombinations);
                    }

                    {
                        IEnumerator testCaseEnumeratorTwo = testCaseGeneratorWithCollisions.CreateIterator(maximumDegreesOfFreedom);

                        CheckThatAtLeastOneCombinationIsNotCovered(testCaseToBeExcluded, testCaseEnumeratorTwo, atomicStateToContainingCombinationMultiMap,
                                                                   atomicTestCasesCombinations);
                    }
                });
        }

        private static void CheckThatAtLeastOneCombinationIsNotCovered(AbstractTestCase testCaseToBeExcluded, IEnumerator testCaseEnumeratorTwo, MultiDictionary<AtomicTestCase, Set<AtomicTestCase>> atomicStateToContainingCombinationMultiMap, SetOfCombinations atomicTestCasesCombinations)
        {
            SetOfCombinations atomicTestCasesCombinationsToBeStruckOff = new SetOfCombinations(atomicTestCasesCombinations);

            while (testCaseEnumeratorTwo.MoveNext())
            {
                AbstractTestCase testCase = (AbstractTestCase) testCaseEnumeratorTwo.Current;

                if (!testCase.Equals(testCaseToBeExcluded))
                {
                    StrikeOffCombinationsCoveredByTestCase(testCase, atomicStateToContainingCombinationMultiMap,
                                                           atomicTestCasesCombinationsToBeStruckOff);
                }
            }

            Assert.IsTrue(atomicTestCasesCombinationsToBeStruckOff.Count > 0U);
        }

        [Test]
        public void TestThatCoverageOfAllNWayCombinationsWorksAroundAnyCollisionsBetweenOutputTestCases()
        {
        }

        [Test]
        public void TestMaximumDegreesOfFreedom()
        {
            ForABunchOfTestCaseGenerators(
                delegate(ITestCaseGenerator testCaseGeneratorWithoutCollisions, ITestCaseGenerator testCaseGeneratorWithCollisions)
                    {
                        Assert.AreEqual(testCaseGeneratorWithoutCollisions.MaximumDegreesOfFreedom,
                                        ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGeneratorWithoutCollisions).
                                            MaximumNumberOfOwningSetsInSequence);
                    });
        }

        // TODO: pruning and pruning tests!

        // TODO: need to test precondition failures for when a test case generator tree is in pruned form
        // has at least one of either a test case generator working with an empty collection or a test
        // case generator with no child alternatives. Neither degrees of freedom nor test case generation
        // is valid. Also expect precondition failures when attempting to prune a tree that cannot yield
        // a valid pruned tree.

        private delegate void TestATestCaseGenerator(
            ITestCaseGenerator testCaseGeneratorWithoutCollisions, ITestCaseGenerator testCaseGeneratorWithCollisions);

        private static void ForABunchOfTestCaseGenerators(TestATestCaseGenerator test)
        {
            Random randomChoice = new Random(0);

            UInt32 countDown = 201U;

            while (countDown-- != 0U)
            {
                Int32 sharedSeed = randomChoice.Next();
                test(CreateRandomlyAssembledTestCaseGenerator(new Random(sharedSeed), false),
                     CreateRandomlyAssembledTestCaseGenerator(new Random(sharedSeed), true));
            }
        }

        private static IEnumerable<Bag<AtomicTestCase>> SequenceOfOwningBags(AbstractTestCase abstractTestCaseCollisions)
        {
            return
                Algorithms.Convert(abstractTestCaseCollisions.AtomicTestCases(),
                                   delegate(AtomicTestCase atomicTestCase) { return atomicTestCase.OwningSet; });
        }

        public abstract class AbstractTestCase
        {
            public abstract IEnumerable<AtomicTestCase> AtomicTestCases();

            public override Boolean Equals(Object another)
            {
                AbstractTestCase anotherOfCompatibleType = another as AbstractTestCase;

                if (null != anotherOfCompatibleType && !EquivalenceIndicies().IsDisjointFrom(anotherOfCompatibleType.EquivalenceIndicies()))
                {
                    return true;
                }

                return EqualWithoutConsideringCollisions(another);
            }

            public override Int32 GetHashCode()
            {
                return EquivalenceIndicies().Count > 0U
                           ? 0
                           : GetHashCodeWithoutConsideringCollisions();
            }

            protected abstract Boolean EqualWithoutConsideringCollisions(Object another);
            public abstract Int32 GetHashCodeWithoutConsideringCollisions();
            public abstract Set<Int32> EquivalenceIndicies();
        }

        public class AtomicTestCase: AbstractTestCase
        {
            private AtomicTestCase(Bag<AtomicTestCase> owningSet)
            {
                _owningSet = owningSet;
            }

            public override IEnumerable<AtomicTestCase> AtomicTestCases()
            {
                ICollection<AtomicTestCase> singletonSequence = new List<AtomicTestCase>();

                singletonSequence.Add(this);

                return singletonSequence;
            }

            public static void PutNewTestCaseInto(Bag<AtomicTestCase> owningBag)
            {
                AtomicTestCase testCase = new AtomicTestCase(owningBag);
                owningBag.Add(testCase);
            }

            public static void PutNewTestCaseInto(Bag<AtomicTestCase> owningBag, Int32 equivalenceIndex)
            {
                AtomicTestCase testCase = new AtomicTestCase(owningBag);
                testCase.SetEquivalenceIndex(equivalenceIndex);
                owningBag.Add(testCase);
            }

            public void SetEquivalenceIndex(Int32 equivalenceIndex)
            {
                _equivalenceIndex = equivalenceIndex;
                _equivalenceIndexExists = true;
            }

            private readonly Bag<AtomicTestCase> _owningSet;

            private Int32 _equivalenceIndex;
            private Boolean _equivalenceIndexExists = false;

            public Bag<AtomicTestCase> OwningSet
            {
                get { return _owningSet; }
            }

            protected override Boolean EqualWithoutConsideringCollisions(Object another)
            {
                AtomicTestCase anotherOfCompatibleType = another as AtomicTestCase;

                if (anotherOfCompatibleType == null)
                {
                    return false;
                }

                return ReferenceEquals(this, anotherOfCompatibleType);
            }

            public override Set<Int32> EquivalenceIndicies()
            {
                Set<Int32> result = new Set<Int32>();

                if (_equivalenceIndexExists)
                {
                    result.Add(_equivalenceIndex);
                }

                return result;
            }

            public override Int32 GetHashCodeWithoutConsideringCollisions()
            {
                return RuntimeHelpers.GetHashCode(this);
            }
        }

        public class ComposedTestCase: AbstractTestCase
        {
            public override IEnumerable<AtomicTestCase> AtomicTestCases()
            {
                return
                    Algorithms.Concatenate(
                        Algorithms.ToArray(
                            Algorithms.Convert(_childTestCases, delegate(AbstractTestCase testCase) { return testCase.AtomicTestCases(); })));
            }

            public static ComposedTestCase MakeShuffledCombination(IEnumerable<AbstractTestCase> testCases, Int32 seed)
            {
                ComposedTestCase result = new ComposedTestCase();

                result._childTestCases = Algorithms.RandomShuffle(testCases, new Random(seed));

                Algorithms.ForEach(result._childTestCases,
                                   delegate(AbstractTestCase childTestCase) { result._equivalenceIndicies.UnionWith(childTestCase.EquivalenceIndicies()); });

                return result;
            }

            protected override Boolean EqualWithoutConsideringCollisions(Object another)
            {
                ComposedTestCase anotherOfCompatibleType = another as ComposedTestCase;

                if (anotherOfCompatibleType == null)
                {
                    return false;
                }

                return Algorithms.EqualCollections(_childTestCases, anotherOfCompatibleType._childTestCases);
            }

            private IEnumerable<AbstractTestCase> _childTestCases;

            private readonly Set<Int32> _equivalenceIndicies = new Set<int>();

            public override Set<Int32> EquivalenceIndicies()
            {
                return _equivalenceIndicies;
            }

            public override Int32 GetHashCodeWithoutConsideringCollisions()
            {
                Int32 result = 0;

                Algorithms.ForEach(_childTestCases,
                                   delegate(AbstractTestCase childTestCase) { result ^= childTestCase.GetHashCodeWithoutConsideringCollisions(); });

                return result;
            }
        }

        public interface ITestCaseGeneratorIntrusiveTestHooks
        {
            SetOfSequencesOfOwningBags PossibleSequencesOfOwningBags();

            UInt32 MaximumNumberOfOwningSetsInSequence { get; }

            IEnumerable<AtomicTestCase> PickFeasibleCombinationOfAtomicTestCases(Random randomChoice);
        }

        public class SetOfCombinations: Set<Set<AtomicTestCase>>
        {
            public SetOfCombinations(): base(_setComparer)
            {
            }

            public SetOfCombinations(SetOfCombinations another):
                base(another, another.Comparer)
            {
                
            }

            private class TestCaseSetComparer: IEqualityComparer<Set<AtomicTestCase>>
            {
                public bool Equals(Set<AtomicTestCase> x, Set<AtomicTestCase> y)
                {
                    return _decoratedComparer.Equals(x, y);
                }

                public int GetHashCode(Set<AtomicTestCase> obj)
                {
                    return _decoratedComparer.GetHashCode(obj);
                }

                private static readonly IEqualityComparer<IEnumerable<AtomicTestCase>> _decoratedComparer =
                    Algorithms.GetSetEqualityComparer<AtomicTestCase>();
            }

            private static readonly IEqualityComparer<Set<AtomicTestCase>> _setComparer = new TestCaseSetComparer();
        }

        public class SetOfSequencesOfOwningBags: Set<IEnumerable<Bag<AtomicTestCase>>>
        {
            public SetOfSequencesOfOwningBags(): base(_sequenceOfBagsDeepValueComparer)
            {
            }

            private class TestCaseBagComparer: IEqualityComparer<Bag<AtomicTestCase>>
            {
                public bool Equals(Bag<AtomicTestCase> x, Bag<AtomicTestCase> y)
                {
                    return _decoratedComparer.Equals(x, y);
                }

                public int GetHashCode(Bag<AtomicTestCase> obj)
                {
                    return _decoratedComparer.GetHashCode(obj);
                }

                private static readonly IEqualityComparer<IEnumerable<AtomicTestCase>> _decoratedComparer =
                    Algorithms.GetSetEqualityComparer<AtomicTestCase>();
            }

            private static readonly IEqualityComparer<IEnumerable<Bag<AtomicTestCase>>> _sequenceOfBagsDeepValueComparer =
                Algorithms.GetCollectionEqualityComparer(new TestCaseBagComparer());
        }


        public class TestCaseFromCollectionGenerator: TestInfrastructure.TestCaseFromCollectionGenerator, ITestCaseGeneratorIntrusiveTestHooks
        {
            private TestCaseFromCollectionGenerator(Bag<AtomicTestCase> owningBag): base(owningBag)
            {
                if (!(owningBag.Count > 0U))
                {
                    throw new PreconditionViolation("The owning set of atomic test cases must have at least one element.");
                }

                _owningBag = owningBag;
            }

            public static TestCaseFromCollectionGenerator Create(Random randomChoice, Int32? equivalenceIndex)
            {
                Console.WriteLine('E');

                const UInt32 maximumNumberOfTestCasesInCollection = 10;

                Bag<AtomicTestCase> owningBag = new Bag<AtomicTestCase>();

                UInt32 countDown = (UInt32) randomChoice.Next((Int32) (maximumNumberOfTestCasesInCollection + 1U));

                while (countDown-- != 0U)
                {
                    AtomicTestCase.PutNewTestCaseInto(owningBag);
                }

                if (equivalenceIndex.HasValue)
                {
                    AtomicTestCase.PutNewTestCaseInto(owningBag, equivalenceIndex.Value);
                }

                return new TestCaseFromCollectionGenerator(owningBag);
            }


            public SetOfSequencesOfOwningBags PossibleSequencesOfOwningBags()
            {
                SetOfSequencesOfOwningBags result = new SetOfSequencesOfOwningBags();

                ICollection<Bag<AtomicTestCase>> singletonSequence = new List<Bag<AtomicTestCase>>();

                singletonSequence.Add(_owningBag);

                result.Add(singletonSequence);

                return result;
            }

            public UInt32 MaximumNumberOfOwningSetsInSequence
            {
                get { return 1U; }
            }

            public IEnumerable<AtomicTestCase> PickFeasibleCombinationOfAtomicTestCases(Random randomChoice)
            {
                return Algorithms.RandomSubset(_owningBag, 1, randomChoice);
            }

            private readonly Bag<AtomicTestCase> _owningBag = new Bag<AtomicTestCase>();
        }

        private static int? RandomlySteppedEquivalenceIndex(Random randomChoice, int? equivalenceIndex)
        {
            return randomChoice.Next(2) == 1
                       ? equivalenceIndex + 1
                       : equivalenceIndex - 1;
        }

        public class TestCaseFromAlternativesGenerator: TestInfrastructure.TestCaseFromAlternativesGenerator, ITestCaseGeneratorIntrusiveTestHooks
        {
            private TestCaseFromAlternativesGenerator(Set<ITestCaseGenerator> testCaseGenerators): base(testCaseGenerators)
            {
                if (!(testCaseGenerators.Count > 0U))
                {
                    throw new PreconditionViolation("There must be at least one alternative test case generator to choose from.");
                }

                _testCaseGenerators = testCaseGenerators;
            }

            public static TestCaseFromAlternativesGenerator Create(Random randomChoice,
                                                                   UInt32 treeDepth,
                                                                   UInt32 maximumDegreesOfFreedom,
                                                                   Int32? equivalenceIndex)
            {
                Console.WriteLine('A');

                Set<ITestCaseGenerator> testCaseGenerators = new Set<ITestCaseGenerator>();

                const UInt32 maximumNumberOfAlternativeTestCaseGenerators = 5;

                UInt32 countDown = (UInt32) randomChoice.Next((Int32) maximumNumberOfAlternativeTestCaseGenerators) + 1U;

                while (countDown-- != 0U)
                {
                    UInt32 maximumDegreesOfFreedomForChild = (UInt32) randomChoice.Next((Int32) (maximumDegreesOfFreedom)) + 1U;
                    testCaseGenerators.Add(
                        CreateRandomlyAssembledTestCaseGenerator(randomChoice, treeDepth, maximumDegreesOfFreedomForChild,
                                                                 RandomlySteppedEquivalenceIndex(randomChoice, equivalenceIndex)));
                }

                return new TestCaseFromAlternativesGenerator(testCaseGenerators);
            }

            public SetOfSequencesOfOwningBags PossibleSequencesOfOwningBags()
            {
                SetOfSequencesOfOwningBags result = new SetOfSequencesOfOwningBags();

                foreach (ITestCaseGenerator testCaseGenerator in _testCaseGenerators)
                {
                    result.UnionWith(((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).PossibleSequencesOfOwningBags());
                }

                return result;
            }

            public UInt32 MaximumNumberOfOwningSetsInSequence
            {
                get
                {
                    if (!(_testCaseGenerators.Count > 0))
                    {
                        throw new PreconditionViolation("Must have at least one child test case generator for this property to be defined.");
                    }

                    if (_testCaseGenerators.Count > 0U)
                    {
                        return
                            Algorithms.Maximum(
                                Algorithms.Convert(_testCaseGenerators,
                                                   delegate(ITestCaseGenerator testCaseGenerator) { return testCaseGenerator.MaximumDegreesOfFreedom; }));
                    }
                    else
                    {
                        throw new InternalAssertionViolation("This property must be strictly greater than zero as a postcondition.");
                    }
                }
            }

            public IEnumerable<AtomicTestCase> PickFeasibleCombinationOfAtomicTestCases(Random randomChoice)
            {
                return
                    ((ITestCaseGeneratorIntrusiveTestHooks) Algorithms.RandomSubset(_testCaseGenerators, 1, randomChoice)[0]).
                        PickFeasibleCombinationOfAtomicTestCases(randomChoice);
            }

            private readonly Set<ITestCaseGenerator> _testCaseGenerators;
        }

        public class TestCaseFromCombinationGenerator: TestInfrastructure.TestCaseFromCombinationGenerator, ITestCaseGeneratorIntrusiveTestHooks
        {
            public delegate AbstractTestCase PermutingClosure(IEnumerable<AbstractTestCase> input);

            private TestCaseFromCombinationGenerator(IList<ITestCaseGenerator> testCaseGenerators, PermutingClosure combiningClosure)
                : base(testCaseGenerators, AdaptPermutingClosure(combiningClosure, (UInt32) testCaseGenerators.Count))
            {
                _testCaseGenerators = testCaseGenerators;
            }

            public static TestCaseFromCombinationGenerator Create(Random randomChoice,
                                                                  UInt32 treeDepth,
                                                                  UInt32 maximumDegreesOfFreedom,
                                                                  Int32? equivalenceIndex)
            {
                Console.WriteLine('C');

                IList<ITestCaseGenerator> testCaseGenerators = new List<ITestCaseGenerator>();

                UInt32 numberOfPartitionPoints = (UInt32) randomChoice.Next((Int32) maximumDegreesOfFreedom) + 1U;

                OrderedSet<UInt32> potentialPartitionPoints = new OrderedSet<UInt32>();

                for (UInt32 potentialPartitionPoint = 1U; potentialPartitionPoint <= maximumDegreesOfFreedom; ++potentialPartitionPoint)
                {
                    potentialPartitionPoints.Add(potentialPartitionPoint);
                }

                OrderedSet<UInt32> selectedPartitionPoints = new OrderedSet<UInt32>();

                selectedPartitionPoints.AddMany(Algorithms.RandomSubset(potentialPartitionPoints, (Int32) numberOfPartitionPoints, randomChoice));

                selectedPartitionPoints.Add(1U);
                // We always put this into the set and use the iterator down below to pick it back out during set up
                // for the loop below. This means we don't have to worry about whether we might encounter the value of
                // one within the loop, which is a messy special case.
                selectedPartitionPoints.Add(maximumDegreesOfFreedom);
                // ... and similarly, we always put the final endpoint for the highest partition
                // in, so that our loop never runs short. If we only have one point to work with,
                // it will have the value of one and therefore will be processed as a degenerate
                // case via the loop setup.

                IEnumerator<UInt32> partitionPointIterator = selectedPartitionPoints.GetEnumerator();

                partitionPointIterator.MoveNext();

                UInt32 partitionStart = partitionPointIterator.Current;

                if (partitionPointIterator.MoveNext())
                {
                    UInt32 nextPartitionStart;
                    do
                    {
                        nextPartitionStart = partitionPointIterator.Current;
                        testCaseGenerators.Add(
                            CreateRandomlyAssembledTestCaseGenerator(randomChoice, treeDepth, nextPartitionStart - partitionStart,
                                                                     RandomlySteppedEquivalenceIndex(randomChoice, equivalenceIndex)));
                        partitionStart = nextPartitionStart;
                    } while (partitionPointIterator.MoveNext());
                }
                else
                {
                    testCaseGenerators.Add(
                        CreateRandomlyAssembledTestCaseGenerator(randomChoice, treeDepth, partitionStart,
                                                                 RandomlySteppedEquivalenceIndex(randomChoice, equivalenceIndex)));
                }

                return new TestCaseFromCombinationGenerator(testCaseGenerators, CreatePermutingClosure(randomChoice));
            }

            private static PermutingClosure CreatePermutingClosure(Random randomChoice)
            {
                Int32 seed = randomChoice.Next();
                return delegate(IEnumerable<AbstractTestCase> input) { return ComposedTestCase.MakeShuffledCombination(input, seed); };
            }

            private static String CreateUniqueIdentifier()
            {
                const UInt32 rangeOfAlphabet = 26U;

                UInt32 numberToBeInterpretedAccordingToABase = _namespaceNameGenerationState++;

                StringBuilder builder = new StringBuilder();

                do
                {
                    builder.Append('a' + numberToBeInterpretedAccordingToABase%rangeOfAlphabet);
                    numberToBeInterpretedAccordingToABase /= rangeOfAlphabet;
                } while (numberToBeInterpretedAccordingToABase != 0U);

                return builder.ToString();
            }

            public class PermutingClosureAdapterSupport
            {
                public PermutingClosure PermutingClosure
                {
                    set { _permutingClosure = value; }
                }

                private PermutingClosure _permutingClosure;
            }

            private static Delegate AdaptPermutingClosure(PermutingClosure permutingClosure, UInt32 numberOfArgumentsForAdaptedClosure)
            {
                Pair<Type, Type> permutingClosureAdapterTypeAndDelegateTypePair;

                if (
                    !_arityToPermutingClosureAdapterTypeAndDelegateTypePairMap.TryGetValue(numberOfArgumentsForAdaptedClosure,
                                                                                           out permutingClosureAdapterTypeAndDelegateTypePair))
                {
                    permutingClosureAdapterTypeAndDelegateTypePair.First = CreatePermutingClosureAdapterType(numberOfArgumentsForAdaptedClosure);
                    permutingClosureAdapterTypeAndDelegateTypePair.Second = CreatePermutingClosureDelegateType(numberOfArgumentsForAdaptedClosure);

                    _arityToPermutingClosureAdapterTypeAndDelegateTypePairMap.Add(numberOfArgumentsForAdaptedClosure,
                                                                                  permutingClosureAdapterTypeAndDelegateTypePair);
                }

                PermutingClosureAdapterSupport adapter =
                    (PermutingClosureAdapterSupport) permutingClosureAdapterTypeAndDelegateTypePair.First.GetConstructor(Type.EmptyTypes).Invoke(null);

                adapter.PermutingClosure = permutingClosure;

                return
                    Delegate.CreateDelegate(permutingClosureAdapterTypeAndDelegateTypePair.Second, adapter,
                                            permutingClosureAdapterTypeAndDelegateTypePair.First.GetMethod(_generatedMethodName));
            }

            private static Type CreatePermutingClosureAdapterType(UInt32 numberOfArgumentsForAdaptedClosure)
            {
                String arityAsString = numberOfArgumentsForAdaptedClosure.ToString();

                TypeBuilder typeBuilder =
                    _moduleBuilder.DefineType("_PermutingClosureAdapterFor" + arityAsString + "Arguments_" + CreateUniqueIdentifier());

                typeBuilder.SetParent(typeof (PermutingClosureAdapterSupport));

                Type[] argumentTypes = CreateArgumentTypesForAdaptedClosure(numberOfArgumentsForAdaptedClosure);

                MethodAttributes methodAttributes = MethodAttributes.Public;

                MethodBuilder methodBuilder =
                    typeBuilder.DefineMethod(_generatedMethodName, methodAttributes, typeof (AbstractTestCase), argumentTypes);

                ILGenerator codeGenerator = methodBuilder.GetILGenerator();

                Type collectionType = typeof (List<AbstractTestCase>);

                codeGenerator.Emit(OpCodes.Newobj, collectionType.GetConstructor(Type.EmptyTypes));
                codeGenerator.Emit(OpCodes.Stloc_0);

                for (Byte argumentIndex = 0; argumentIndex < numberOfArgumentsForAdaptedClosure; ++argumentIndex)
                {
                    codeGenerator.Emit(OpCodes.Ldloc_0);
                    codeGenerator.Emit(OpCodes.Ldarg_S, (Byte) (argumentIndex + 1U));
                    codeGenerator.Emit(OpCodes.Callvirt, collectionType.GetMethod("Add"));
                }

                codeGenerator.Emit(OpCodes.Ldarg_0);
                codeGenerator.Emit(OpCodes.Ldfld,
                                   typeof (PermutingClosureAdapterSupport).GetField("_permutingClosure",
                                                                                    BindingFlags.NonPublic | BindingFlags.Instance));
                codeGenerator.Emit(OpCodes.Ldloc_0);
                codeGenerator.Emit(OpCodes.Callvirt, typeof (PermutingClosure).GetMethod("Invoke"));
                codeGenerator.Emit(OpCodes.Ret);

                Type permutingClosureAdapterType = typeBuilder.CreateType();

                return permutingClosureAdapterType;
            }

            private static Type CreatePermutingClosureDelegateType(UInt32 numberOfArgumentsForAdaptedClosure)
            {
                String arityAsString = numberOfArgumentsForAdaptedClosure.ToString();

                TypeAttributes typeAttributes = TypeAttributes.Class | TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.AnsiClass
                                                | TypeAttributes.AutoClass;

                TypeBuilder typeBuilder =
                    _moduleBuilder.DefineType("_PermutingClosureDelegateFor" + arityAsString + "Arguments_" + CreateUniqueIdentifier(), typeAttributes);

                typeBuilder.SetParent(typeof (MulticastDelegate));

                Type[] constructorArgumentTypes = new Type[] {typeof (Object), typeof (IntPtr)};

                MethodAttributes constructorAttributes = MethodAttributes.RTSpecialName | MethodAttributes.HideBySig | MethodAttributes.Public;

                ConstructorBuilder constructorBuilder =
                    typeBuilder.DefineConstructor(constructorAttributes, CallingConventions.Standard, constructorArgumentTypes);

                constructorBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);


                Type[] methodArgumentTypes = CreateArgumentTypesForAdaptedClosure(numberOfArgumentsForAdaptedClosure);

                MethodAttributes methodAttributes = MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.NewSlot
                                                    | MethodAttributes.Virtual;

                String invokeMethodName = "Invoke";

                MethodBuilder methodBuilder =
                    typeBuilder.DefineMethod(invokeMethodName, methodAttributes, typeof (AbstractTestCase), methodArgumentTypes);

                methodBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);

                Type permutingClosureAdapterType = typeBuilder.CreateType();

                return permutingClosureAdapterType;
            }

            private static Type[] CreateArgumentTypesForAdaptedClosure(UInt32 numberOfArgumentsForAdaptedClosure)
            {
                Type[] result = new Type[numberOfArgumentsForAdaptedClosure];

                Algorithms.Fill(result, typeof (AbstractTestCase));

                return result;
            }

            public SetOfSequencesOfOwningBags PossibleSequencesOfOwningBags()
            {
                SetOfSequencesOfOwningBags result = new SetOfSequencesOfOwningBags();

                ConcatenateCrossProductOfSequences(_testCaseGenerators, new List<Bag<AtomicTestCase>>(), result);

                return result;
            }

            private static void ConcatenateCrossProductOfSequences(IList<ITestCaseGenerator> testCaseGenerators,
                                                                   IEnumerable<Bag<AtomicTestCase>> sequenceBeingBuiltUp,
                                                                   SetOfSequencesOfOwningBags result)
            {
                if (testCaseGenerators.Count == 0)
                {
                    result.Add(sequenceBeingBuiltUp);
                }
                else
                {
                    foreach (IEnumerable<Bag<AtomicTestCase>> sequence in
                        ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerators[0]).PossibleSequencesOfOwningBags())
                    {
                        ConcatenateCrossProductOfSequences(Algorithms.Range(testCaseGenerators, 1, testCaseGenerators.Count - 1),
                                                           Algorithms.Concatenate(sequenceBeingBuiltUp, sequence), result);
                    }
                }
            }

            public UInt32 MaximumNumberOfOwningSetsInSequence
            {
                get
                {
                    if (!(_testCaseGenerators.Count > 0))
                    {
                        throw new PreconditionViolation("Must have at least one child test case generator for this property to be defined.");
                    }

                    // Where is the 'fold-left' algorithm for .NET? :-(
                    UInt32 result = 0U;
                    Algorithms.ForEach(_testCaseGenerators,
                                       delegate(ITestCaseGenerator testCaseGenerator) { result += testCaseGenerator.MaximumDegreesOfFreedom; });

                    if (!(result > 0U))
                    {
                        throw new InternalAssertionViolation("This property must be strictly greater than zero as a postcondition.");
                    }

                    return result;
                }
            }

            public IEnumerable<AtomicTestCase> PickFeasibleCombinationOfAtomicTestCases(Random randomChoice)
            {
                UInt32 numberOfChildren = (UInt32) randomChoice.Next(_testCaseGenerators.Count) + 1U;

                IEnumerable<IEnumerable<AtomicTestCase>> sequenceOfCombinationsOfAtomicTestCasesFromChildren =
                    Algorithms.Convert(Algorithms.RandomSubset(_testCaseGenerators, (Int32) numberOfChildren, randomChoice),
                                       delegate(ITestCaseGenerator testCaseGenerator)
                                           {
                                               return
                                                   ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).PickFeasibleCombinationOfAtomicTestCases
                                                       (randomChoice);
                                           });

                return Algorithms.Concatenate(Algorithms.ToArray(sequenceOfCombinationsOfAtomicTestCasesFromChildren));
            }

            private readonly IList<ITestCaseGenerator> _testCaseGenerators;

            private static UInt32 _namespaceNameGenerationState = 0U;

            private static readonly String _generatedMethodName = "Invoke";

            private static readonly AssemblyBuilder _assemblyBuilder;

            private static readonly ModuleBuilder _moduleBuilder;

            static TestCaseFromCombinationGenerator()
            {
                String assemblyName = GetNonConflictingAssemblyName();

                AssemblyName assemblyNameThingie = new AssemblyName();

                assemblyNameThingie.Name = assemblyName;

                _assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyNameThingie, AssemblyBuilderAccess.Run);

                const String moduleName = "theBigLebowski";

                _moduleBuilder = _assemblyBuilder.DefineDynamicModule(moduleName);
            }

            private static String GetNonConflictingAssemblyName()
            {
                Assembly[] extantAssemblies = AppDomain.CurrentDomain.GetAssemblies();

                do
                {
                    String assemblyName = CreateUniqueIdentifier();

                    if (Algorithms.FindFirstWhere(extantAssemblies, delegate(Assembly assembly) { return assembly.GetName().Name == assemblyName; })
                        == null)
                    {
                        return assemblyName;
                    }
                } while (true);
            }

            private static readonly IDictionary<UInt32, Pair<Type, Type>> _arityToPermutingClosureAdapterTypeAndDelegateTypePairMap =
                new Dictionary<UInt32, Pair<Type, Type>>();
        }

        public static ITestCaseGenerator CreateRandomlyAssembledTestCaseGenerator(Random randomChoice, Boolean createDuplicatesAsWell)
        {
            Console.WriteLine();

            UInt32 upperBoundOfMaximumDegreesOfFreedom = 5U;
            return
                CreateRandomlyAssembledTestCaseGenerator(randomChoice, 0U,
                                                         (UInt32) randomChoice.Next((Int32) upperBoundOfMaximumDegreesOfFreedom) + 1U,
                                                         createDuplicatesAsWell
                                                             ? new Int32?(0)
                                                             : null);
        }

        public static ITestCaseGenerator CreateRandomlyAssembledTestCaseGenerator(Random randomChoice,
                                                                                  UInt32 parentTreeDepth,
                                                                                  UInt32 maximumDegreesOfFreedom,
                                                                                  Int32? equivalenceIndex)
        {
            if (maximumDegreesOfFreedom == 0U)
            {
                throw new PreconditionViolation("The test should specify the maximum degrees of freedom as being strictly greater than zero.");
            }

            UInt32 treeDepth = parentTreeDepth + 1U;

            {
                UInt32 countDown = treeDepth;

                Console.Write(treeDepth);

                while (--countDown != 0U)
                {
                    Console.Write(' ');
                }
            }

            UInt32 maximumPermittedTreeDepth = 9U;

            if (maximumDegreesOfFreedom == 1U && randomChoice.Next((Int32) maximumPermittedTreeDepth) + 1U <= treeDepth)
            {
                return TestCaseFromCollectionGenerator.Create(randomChoice, equivalenceIndex);
            }
            else
            {
                switch (randomChoice.Next(2))
                {
                    case 0:
                        return TestCaseFromAlternativesGenerator.Create(randomChoice, treeDepth, maximumDegreesOfFreedom, equivalenceIndex);

                    case 1:
                        return TestCaseFromCombinationGenerator.Create(randomChoice, treeDepth, maximumDegreesOfFreedom, equivalenceIndex);

                    default:
                        throw new InternalAssertionViolation("Default of this switch should not be executed.");
                }
            }
        }

        [SetUp]
        public void Reinitialise()
        {
        }
    }
}
