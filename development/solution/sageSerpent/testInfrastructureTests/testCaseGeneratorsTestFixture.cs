using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;
using System.Text;
using C5;
using NUnit.Framework;
using SageSerpent.Infrastructure;
using SageSerpent.TestInfrastructure;
using Wintellect.PowerCollections;

namespace SageSerpent.TestInfrastructureTests
{
    using AtomicTestCaseToContainingCombinationMultiMap = Wintellect.PowerCollections.MultiDictionary<AtomicTestCase, HashSet<AtomicTestCase>>;
    using CombinationOfAtomicTestCases = HashSet<AtomicTestCase>;
    using SequenceOfAtomicTestCases = C5.IList<AtomicTestCase>;
    using CollectionOwningAtomicTestCases = HashBag<AtomicTestCase>;
    using SequenceOfCollectionsOwningAtomicTestCases = System.Collections.Generic.IEnumerable<HashBag<AtomicTestCase>>;
    using SetOfCombinations = HashSet<HashSet<AtomicTestCase>>;
    using SetOfSequencesOfCollectionsOwningAtomicTestCases = HashSet<IEnumerable<HashBag<AtomicTestCase>>>;

    public abstract class AbstractTestCase
    {
        public abstract SequenceOfAtomicTestCases AtomicTestCases();

        public override Boolean Equals(Object another)
        {
            AbstractTestCase anotherOfCompatibleType = another as AbstractTestCase;

            if (null != anotherOfCompatibleType && !Algorithms.DisjointSets(EquivalenceIndicies(), anotherOfCompatibleType.EquivalenceIndicies()))
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
        public abstract HashSet<Int32> EquivalenceIndicies();
    }

    public class AtomicTestCase: AbstractTestCase
    {
        private AtomicTestCase(CollectionOwningAtomicTestCases owningCollection)
        {
            _owningCollection = owningCollection;
        }

        public override SequenceOfAtomicTestCases AtomicTestCases()
        {
            SequenceOfAtomicTestCases singletonSequence = new C5.LinkedList<AtomicTestCase>();

            singletonSequence.Add(this);

            return singletonSequence;
        }

        public static void PutNewTestCaseInto(CollectionOwningAtomicTestCases owningCollection)
        {
            AtomicTestCase testCase = new AtomicTestCase(owningCollection);
            owningCollection.Add(testCase);
        }

        public static void PutNewTestCaseInto(CollectionOwningAtomicTestCases owningCollection, Int32 equivalenceIndex)
        {
            AtomicTestCase testCase = new AtomicTestCase(owningCollection);
            testCase.SetEquivalenceIndex(equivalenceIndex);
            owningCollection.Add(testCase);
        }

        public void SetEquivalenceIndex(Int32 equivalenceIndex)
        {
            _equivalenceIndex = equivalenceIndex;
            _equivalenceIndexExists = true;
        }

        private readonly CollectionOwningAtomicTestCases _owningCollection;

        private Int32 _equivalenceIndex;
        private Boolean _equivalenceIndexExists = false;

        public CollectionOwningAtomicTestCases OwningCollection
        {
            get { return _owningCollection; }
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

        public override HashSet<Int32> EquivalenceIndicies()
        {
            HashSet<Int32> result = new HashSet<Int32>();

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
        public override SequenceOfAtomicTestCases AtomicTestCases()
        {
            SequenceOfAtomicTestCases result = new C5.LinkedList<AtomicTestCase>();
            result.AddAll
                (
                    Algorithms.Concatenate(
                        Algorithms.ToArray(
                            Algorithms.Convert(_childTestCases, delegate(AbstractTestCase testCase) { return testCase.AtomicTestCases(); }))));

            return result;
        }

        public static ComposedTestCase MakeShuffledCombination(IEnumerable<AbstractTestCase> testCases, Int32 seed)
        {
            ComposedTestCase result = new ComposedTestCase();

            result._childTestCases = Algorithms.RandomShuffle(testCases, new Random(seed));

            Algorithms.ForEach(result._childTestCases,
                               delegate(AbstractTestCase childTestCase) { result._equivalenceIndicies.AddAll(childTestCase.EquivalenceIndicies()); });

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

        private readonly HashSet<Int32> _equivalenceIndicies = new HashSet<int>();

        public override HashSet<Int32> EquivalenceIndicies()
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

                        SetOfSequencesOfCollectionsOwningAtomicTestCases possibleSequencesOfCollectionsOwningAtomicTestCases =
                            ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGeneratorWithoutCollisions).
                                PossibleSequencesOfCollectionsOwningAtomicTestCases();

                        while (testCaseIterator.MoveNext())
                        {
                            AbstractTestCase testCase = (AbstractTestCase) testCaseIterator.Current;

                            SequenceOfCollectionsOwningAtomicTestCases sequenceOfOwningCollectionsThatContributedToThisTestCase =
                                SequenceOfCollectionsOwningAtomicTestCases(testCase);

                            Assert.IsTrue(
                                possibleSequencesOfCollectionsOwningAtomicTestCases.Contains(sequenceOfOwningCollectionsThatContributedToThisTestCase));
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
                CombinationOfAtomicTestCases atomicTestCasesCombination =
                    ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).PickFeasibleCombinationOfAtomicTestCases(randomChoice);

                atomicTestCasesCombinations.Add(atomicTestCasesCombination);
            }

            return atomicTestCasesCombinations;
        }

        private static AtomicTestCaseToContainingCombinationMultiMap MakeAtomicStateToContainingCombinationMultiMap(
            SetOfCombinations atomicTestCasesCombinations)
        {
            AtomicTestCaseToContainingCombinationMultiMap atomicStateToContainingCombinationMultiMap =
                new AtomicTestCaseToContainingCombinationMultiMap(false);

            Algorithms.ForEach(atomicTestCasesCombinations, delegate(CombinationOfAtomicTestCases combination)
                {
                    Algorithms.ForEach(combination, delegate(AtomicTestCase
                                                        testCase)
                        { atomicStateToContainingCombinationMultiMap.Add(testCase, combination); });
                });
            return atomicStateToContainingCombinationMultiMap;
        }

        private static UInt32 ChooseHowManyDegreesOfFreedomRequiredToAllowAllCombinationsToBeCovered(SetOfCombinations atomicTestCasesCombinations,
                                                                                                     Random randomChoice,
                                                                                                     ITestCaseGenerator testCaseGenerator)
        {
            UInt32 maximumCombinationWidth = Algorithms.Maximum(Algorithms.Convert(atomicTestCasesCombinations, delegate(CombinationOfAtomicTestCases
                                                                                                                    combination)
                { return (UInt32) combination.Count; }));

            return (UInt32) randomChoice.Next((Int32) maximumCombinationWidth, (Int32) (testCaseGenerator.MaximumDegreesOfFreedom + 1U));
        }

        private static void StrikeOffCombinationsCoveredByTestCase(AbstractTestCase testCase,
                                                                   AtomicTestCaseToContainingCombinationMultiMap
                                                                       atomicStateToContainingCombinationMultiMap,
                                                                   SetOfCombinations atomicTestCasesCombinations)
        {
            C5.IDictionary<CombinationOfAtomicTestCases, UInt32> combinationToNumberOfAtomicTestCaseComponentsLeftToFindMap =
                new HashDictionary<CombinationOfAtomicTestCases, UInt32>();

            Algorithms.ForEach(atomicTestCasesCombinations,
                               delegate(CombinationOfAtomicTestCases combination) { combinationToNumberOfAtomicTestCaseComponentsLeftToFindMap[combination] = (UInt32) combination.Count; });

            Algorithms.ForEach(testCase.AtomicTestCases(), delegate(AtomicTestCase atomicTestCase)
                {
                    if (atomicStateToContainingCombinationMultiMap.ContainsKey(atomicTestCase))
                    {
                        Algorithms.ForEach(atomicStateToContainingCombinationMultiMap[atomicTestCase], delegate(CombinationOfAtomicTestCases
                                                                                                           combination)
                            {
                                --
                                    combinationToNumberOfAtomicTestCaseComponentsLeftToFindMap[combination];
                            });
                    }
                });

            Algorithms.ForEach(combinationToNumberOfAtomicTestCaseComponentsLeftToFindMap,
                               delegate(C5.KeyValuePair<CombinationOfAtomicTestCases, uint>
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

                        AtomicTestCaseToContainingCombinationMultiMap atomicStateToContainingCombinationMultiMap =
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

        private static void CheckThatAllCombinationsAreCovered(IEnumerator testCaseEnumerator,
                                                               AtomicTestCaseToContainingCombinationMultiMap
                                                                   atomicStateToContainingCombinationMultiMap,
                                                               SetOfCombinations atomicTestCasesCombinations)
        {
            SetOfCombinations atomicTestCasesCombinationsToBeStruckOff = (SetOfCombinations) atomicTestCasesCombinations.Clone();

            while (testCaseEnumerator.MoveNext())
            {
                AbstractTestCase testCase = (AbstractTestCase) testCaseEnumerator.Current;

                StrikeOffCombinationsCoveredByTestCase(testCase, atomicStateToContainingCombinationMultiMap, atomicTestCasesCombinationsToBeStruckOff);
            }

            Assert.IsTrue(atomicTestCasesCombinationsToBeStruckOff.Count == 0U);
        }

        private static SetOfCombinations PickTheLargestCombinationsOfSizeNotExceeding(SequenceOfAtomicTestCases testCases,
                                                                                      UInt32 maximumCombinationWidth)
        {
            return PickAllCombinationsOfSize(testCases, Math.Min(maximumCombinationWidth, (UInt32) testCases.Count));
        }

        private static SetOfCombinations PickAllCombinationsOfSize(SequenceOfAtomicTestCases testCases, UInt32 combinationSize)
        {
            if (!(testCases.Count >= combinationSize))
            {
                throw new PreconditionViolation("Attempt to pick out combination larger than size of source collection.");
            }

            if (0U == combinationSize)
            {
                SetOfCombinations trivialResult = new SetOfCombinations();

                trivialResult.Add(new CombinationOfAtomicTestCases());

                return trivialResult;
            }

            if (testCases.Count == combinationSize)
            {
                SetOfCombinations trivialResult = new SetOfCombinations();

                CombinationOfAtomicTestCases onlyCombination = new CombinationOfAtomicTestCases();
                onlyCombination.AddAll(testCases);

                trivialResult.Add(onlyCombination);

                return trivialResult;
            }

            // Alas, neither [head::tail] nor [head:tail] ...

            SequenceOfAtomicTestCases tailTestCases = testCases.View(1, testCases.Count - 1);

            AtomicTestCase headTestCase = testCases[0];

            SetOfCombinations result = new SetOfCombinations();

            result.AddAll(
                Algorithms.Convert(PickAllCombinationsOfSize(tailTestCases, combinationSize - 1U), delegate(CombinationOfAtomicTestCases combination)
                    {
                        CombinationOfAtomicTestCases value = (CombinationOfAtomicTestCases) combination.Clone();

                        value.Add(headTestCase);

                        return value;
                    }));

            result.AddAll(PickAllCombinationsOfSize(tailTestCases, combinationSize));

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
                            (UInt32) randomChoice.Next((Int32) testCaseGeneratorWithoutCollisions.MaximumDegreesOfFreedom) + 1U;

                        HashSet<AbstractTestCase> testCases = new HashSet<AbstractTestCase>();

                        IEnumerator testCaseEnumerator = testCaseGeneratorWithoutCollisions.CreateIterator(maximumDegreesOfFreedom);

                        while (testCaseEnumerator.MoveNext())
                        {
                            testCases.Add((AbstractTestCase) testCaseEnumerator.Current);
                        }

                        AbstractTestCase testCaseToBeExcluded = Algorithms.RandomSubset(testCases, 1, randomChoice)[0];

                        SetOfCombinations atomicTestCasesCombinations =
                            PickTheLargestCombinationsOfSizeNotExceeding(testCaseToBeExcluded.AtomicTestCases(), maximumDegreesOfFreedom == 0U
                                                                                                                     ? testCaseGeneratorWithoutCollisions
                                                                                                                           .MaximumDegreesOfFreedom
                                                                                                                     : maximumDegreesOfFreedom);

                        AtomicTestCaseToContainingCombinationMultiMap atomicStateToContainingCombinationMultiMap =
                            MakeAtomicStateToContainingCombinationMultiMap(atomicTestCasesCombinations);

                        {
                            IEnumerator testCaseEnumeratorTwo = testCaseGeneratorWithoutCollisions.CreateIterator(maximumDegreesOfFreedom);

                            CheckThatAtLeastOneCombinationIsNotCovered(testCaseToBeExcluded, testCaseEnumeratorTwo,
                                                                       atomicStateToContainingCombinationMultiMap, atomicTestCasesCombinations);
                        }

                        {
                            IEnumerator testCaseEnumeratorTwo = testCaseGeneratorWithCollisions.CreateIterator(maximumDegreesOfFreedom);

                            CheckThatAtLeastOneCombinationIsNotCovered(testCaseToBeExcluded, testCaseEnumeratorTwo,
                                                                       atomicStateToContainingCombinationMultiMap, atomicTestCasesCombinations);
                        }
                    });
        }

        private static void CheckThatAtLeastOneCombinationIsNotCovered(AbstractTestCase testCaseToBeExcluded,
                                                                       IEnumerator testCaseEnumeratorTwo,
                                                                       AtomicTestCaseToContainingCombinationMultiMap
                                                                           atomicStateToContainingCombinationMultiMap,
                                                                       SetOfCombinations atomicTestCasesCombinations)
        {
            SetOfCombinations atomicTestCasesCombinationsToBeStruckOff = (SetOfCombinations) atomicTestCasesCombinations.Clone();

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

        private static SequenceOfCollectionsOwningAtomicTestCases SequenceOfCollectionsOwningAtomicTestCases(AbstractTestCase testCase)
        {
            return Algorithms.Convert(testCase.AtomicTestCases(), delegate(AtomicTestCase atomicTestCase) { return atomicTestCase.OwningCollection; });
        }

        public interface ITestCaseGeneratorIntrusiveTestHooks
        {
            SetOfSequencesOfCollectionsOwningAtomicTestCases PossibleSequencesOfCollectionsOwningAtomicTestCases();

            UInt32 MaximumNumberOfOwningSetsInSequence { get; }

            CombinationOfAtomicTestCases PickFeasibleCombinationOfAtomicTestCases(Random randomChoice);
        }

        public class TestCaseFromCollectionGenerator: TestInfrastructure.TestCaseFromCollectionGenerator, ITestCaseGeneratorIntrusiveTestHooks
        {
            private TestCaseFromCollectionGenerator(CollectionOwningAtomicTestCases owningCollection): base(owningCollection.ToArray())
            {
                if (!(owningCollection.Count > 0U))
                {
                    throw new PreconditionViolation("The owning collection of atomic test cases must have at least one element.");
                }

                _owningCollection = owningCollection;
            }

            public static TestCaseFromCollectionGenerator Create(Random randomChoice, Int32? equivalenceIndex)
            {
                Console.WriteLine('E');

                const UInt32 maximumNumberOfTestCasesInCollection = 10;

                CollectionOwningAtomicTestCases owningCollection = new CollectionOwningAtomicTestCases();

                UInt32 countDown = (UInt32) randomChoice.Next((Int32) maximumNumberOfTestCasesInCollection) + 1U;

                while (countDown-- != 0U)
                {
                    AtomicTestCase.PutNewTestCaseInto(owningCollection);
                }

                if (equivalenceIndex.HasValue)
                {
                    AtomicTestCase.PutNewTestCaseInto(owningCollection, equivalenceIndex.Value);
                }

                return new TestCaseFromCollectionGenerator(owningCollection);
            }


            public SetOfSequencesOfCollectionsOwningAtomicTestCases PossibleSequencesOfCollectionsOwningAtomicTestCases()
            {
                SetOfSequencesOfCollectionsOwningAtomicTestCases result = new SetOfSequencesOfCollectionsOwningAtomicTestCases();

                System.Collections.Generic.ICollection<CollectionOwningAtomicTestCases> singletonSequence =
                    new List<CollectionOwningAtomicTestCases>();

                singletonSequence.Add(_owningCollection);

                result.Add(singletonSequence);

                return result;
            }

            public UInt32 MaximumNumberOfOwningSetsInSequence
            {
                get { return 1U; }
            }

            public CombinationOfAtomicTestCases PickFeasibleCombinationOfAtomicTestCases(Random randomChoice)
            {
                CombinationOfAtomicTestCases result = new CombinationOfAtomicTestCases();
                result.AddAll(Algorithms.RandomSubset(_owningCollection, 1, randomChoice));
                return result;
            }

            private readonly CollectionOwningAtomicTestCases _owningCollection = new CollectionOwningAtomicTestCases();
        }

        private static int? RandomlySteppedEquivalenceIndex(Random randomChoice, int? equivalenceIndex)
        {
            return randomChoice.Next(2) == 1
                       ? equivalenceIndex + 1
                       : equivalenceIndex - 1;
        }

        public class TestCaseFromAlternativesGenerator: TestInfrastructure.TestCaseFromAlternativesGenerator, ITestCaseGeneratorIntrusiveTestHooks
        {
            private TestCaseFromAlternativesGenerator(HashSet<ITestCaseGenerator> testCaseGenerators): base(testCaseGenerators)
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

                HashSet<ITestCaseGenerator> testCaseGenerators = new HashSet<ITestCaseGenerator>();

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

            public SetOfSequencesOfCollectionsOwningAtomicTestCases PossibleSequencesOfCollectionsOwningAtomicTestCases()
            {
                SetOfSequencesOfCollectionsOwningAtomicTestCases result = new SetOfSequencesOfCollectionsOwningAtomicTestCases();

                foreach (ITestCaseGenerator testCaseGenerator in _testCaseGenerators)
                {
                    result.AddAll(((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).PossibleSequencesOfCollectionsOwningAtomicTestCases());
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

            public CombinationOfAtomicTestCases PickFeasibleCombinationOfAtomicTestCases(Random randomChoice)
            {
                return
                    ((ITestCaseGeneratorIntrusiveTestHooks) Algorithms.RandomSubset(_testCaseGenerators, 1, randomChoice)[0]).
                        PickFeasibleCombinationOfAtomicTestCases(randomChoice);
            }

            private readonly HashSet<ITestCaseGenerator> _testCaseGenerators;
        }

        public class TestCaseFromCombinationGenerator: TestInfrastructure.TestCaseFromCombinationGenerator, ITestCaseGeneratorIntrusiveTestHooks
        {
            public delegate AbstractTestCase PermutingClosure(IEnumerable<AbstractTestCase> input);

            private TestCaseFromCombinationGenerator(C5.IList<ITestCaseGenerator> testCaseGenerators, PermutingClosure combiningClosure)
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

                C5.IList<ITestCaseGenerator> testCaseGenerators = new C5.LinkedList<ITestCaseGenerator>();

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
                // one within the loop, which is a messy special case...
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
                    !_arityToPermutingClosureAdapterTypeAndDelegateTypePairMap.Find(numberOfArgumentsForAdaptedClosure,
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

            public SetOfSequencesOfCollectionsOwningAtomicTestCases PossibleSequencesOfCollectionsOwningAtomicTestCases()
            {
                SetOfSequencesOfCollectionsOwningAtomicTestCases result = new SetOfSequencesOfCollectionsOwningAtomicTestCases();

                ConcatenateCrossProductOfSequences(_testCaseGenerators, new List<CollectionOwningAtomicTestCases>(), result);

                return result;
            }

            private static void ConcatenateCrossProductOfSequences(C5.IList<ITestCaseGenerator> testCaseGenerators,
                                                                   SequenceOfCollectionsOwningAtomicTestCases sequenceBeingBuiltUp,
                                                                   SetOfSequencesOfCollectionsOwningAtomicTestCases result)
            {
                if (testCaseGenerators.Count == 0)
                {
                    result.Add(sequenceBeingBuiltUp);
                }
                else
                {
                    foreach (SequenceOfCollectionsOwningAtomicTestCases sequence in
                        ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerators[0]).PossibleSequencesOfCollectionsOwningAtomicTestCases())
                    {
                        ConcatenateCrossProductOfSequences(testCaseGenerators.View(1, testCaseGenerators.Count - 1),
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

            public CombinationOfAtomicTestCases PickFeasibleCombinationOfAtomicTestCases(Random randomChoice)
            {
                UInt32 numberOfChildren = (UInt32) randomChoice.Next(_testCaseGenerators.Count) + 1U;

                IEnumerable<CombinationOfAtomicTestCases> sequenceOfCombinationsOfAtomicTestCasesFromChildren =
                    Algorithms.Convert(Algorithms.RandomSubset(_testCaseGenerators, (Int32) numberOfChildren, randomChoice),
                                       delegate(ITestCaseGenerator testCaseGenerator)
                                           {
                                               return
                                                   ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).PickFeasibleCombinationOfAtomicTestCases
                                                       (randomChoice);
                                           });

                CombinationOfAtomicTestCases result = new CombinationOfAtomicTestCases();

                result.AddAll(Algorithms.Concatenate(Algorithms.ToArray(sequenceOfCombinationsOfAtomicTestCasesFromChildren)));

                return result;
            }

            private readonly C5.IList<ITestCaseGenerator> _testCaseGenerators;

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

            private static readonly C5.IDictionary<UInt32, Pair<Type, Type>> _arityToPermutingClosureAdapterTypeAndDelegateTypePairMap =
                new C5.HashDictionary<UInt32, Pair<Type, Type>>();
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
