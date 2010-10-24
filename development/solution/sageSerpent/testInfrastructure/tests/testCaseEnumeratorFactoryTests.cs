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

namespace SageSerpent.TestInfrastructure.Tests
{
    using TestVariableLevelToContainingCombinationMultiMap = MultiDictionary<TestVariableLevel, HashSet<TestVariableLevel>>;
    using CombinationOfTestVariableLevels = HashSet<TestVariableLevel>;
    using SequenceOfTestVariableLevels = C5.IList<TestVariableLevel>;
    using CollectionOwningTestVariableLevels = C5.ICollection<TestVariableLevel>;
    using SequenceOfCollectionsOwningTestVariableLevels = IEnumerable<C5.ICollection<TestVariableLevel>>;
    using SetOfCombinations = HashSet<HashSet<TestVariableLevel>>;
    using SetOfSequencesOfCollectionsOwningTestVariableLevels = HashSet<IEnumerable<C5.ICollection<TestVariableLevel>>>;

    public abstract class AbstractTestCase
    {
        public abstract SequenceOfTestVariableLevels TestVariableLevels();

        public override Boolean Equals(Object another)
        {
            var anotherTestCase = another as AbstractTestCase;

            if (null != anotherTestCase &&
                !Algorithms.DisjointSets(EquivalenceIndicies(), anotherTestCase.EquivalenceIndicies()))
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

    public class TestVariableLevel : AbstractTestCase
    {
        private readonly CollectionOwningTestVariableLevels _owningCollection;

        private Int32 _equivalenceIndex;
        private Boolean _equivalenceIndexExists;

        private TestVariableLevel(CollectionOwningTestVariableLevels owningCollection)
        {
            _owningCollection = owningCollection;
        }

        public CollectionOwningTestVariableLevels OwningCollection
        {
            get { return _owningCollection; }
        }

        public override SequenceOfTestVariableLevels TestVariableLevels()
        {
            var singletonSequence = new C5.LinkedList<TestVariableLevel> {this};

            return singletonSequence;
        }

        public static void PutNewTestCaseInto(CollectionOwningTestVariableLevels owningCollection)
        {
            var testCase = new TestVariableLevel(owningCollection);
            owningCollection.Add(testCase);
        }

        public static void PutNewTestCaseInto(CollectionOwningTestVariableLevels owningCollection, Int32 equivalenceIndex)
        {
            var testCase = new TestVariableLevel(owningCollection);
            testCase.SetEquivalenceIndex(equivalenceIndex);
            owningCollection.Add(testCase);
        }

        public void SetEquivalenceIndex(Int32 equivalenceIndex)
        {
            _equivalenceIndex = equivalenceIndex;
            _equivalenceIndexExists = true;
        }

        protected override Boolean EqualWithoutConsideringCollisions(Object another)
        {
            var anotherOfCompatibleType = another as TestVariableLevel;

            return anotherOfCompatibleType != null && ReferenceEquals(this, anotherOfCompatibleType);
        }

        public override HashSet<Int32> EquivalenceIndicies()
        {
            var result = new HashSet<Int32>();

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

    public class ComposedTestCase : AbstractTestCase
    {
        private readonly HashSet<Int32> _equivalenceIndicies = new HashSet<int>();
        private IEnumerable<AbstractTestCase> _childTestCases;

        public override SequenceOfTestVariableLevels TestVariableLevels()
        {
            var result = new C5.LinkedList<TestVariableLevel>();
            result.AddAll
                (
                Algorithms.Concatenate(
                    Algorithms.ToArray(
                        Algorithms.Convert(_childTestCases,
                                           testCase => testCase.TestVariableLevels()))));

            return result;
        }

        public static ComposedTestCase MakeShuffledCombination(IEnumerable<AbstractTestCase> testCases, Int32 seed)
        {
            var result = new ComposedTestCase();

            result._childTestCases = Algorithms.RandomShuffle(testCases, new Random(seed));

            Algorithms.ForEach(result._childTestCases,
                               childTestCase => result._equivalenceIndicies.AddAll(childTestCase.EquivalenceIndicies()));

            return result;
        }

        protected override Boolean EqualWithoutConsideringCollisions(Object another)
        {
            var anotherOfCompatibleType = another as ComposedTestCase;

            return anotherOfCompatibleType != null &&
                   Algorithms.EqualCollections(_childTestCases, anotherOfCompatibleType._childTestCases);
        }

        public override HashSet<Int32> EquivalenceIndicies()
        {
            return _equivalenceIndicies;
        }

        public override Int32 GetHashCodeWithoutConsideringCollisions()
        {
            var result = 0;

            Algorithms.ForEach(_childTestCases,
                               childTestCase => result ^= childTestCase.GetHashCodeWithoutConsideringCollisions());

            return result;
        }
    }

    [TestFixture]
    public class TestCaseGeneratorsTestFixture
    {
        #region Setup/Teardown

        [SetUp]
        public void Reinitialise()
        {
        }

        #endregion

        private static SetOfCombinations MakeTestVariableLevelsCombinations(ITestCaseEnumeratorFactory testCaseGenerator,
                                                                         Random randomChoice)
        {
            var numberOfCombinationsLeftToTry = 100U;
            // NOTE: we may get some repeated combinations, or
            // permutations of the same combination. This is OK,
            // as long as it doesn't happen too often or as long as
            // it only occurs because there aren't enough combinations
            // to fulfill the test's expectations.

            var testVariableLevelsCombinations = new SetOfCombinations();


            while (numberOfCombinationsLeftToTry-- != 0U)
            {
                var testVariableLevelsCombination =
                    ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).PickFeasibleCombinationOfTestVariableLevels(
                        randomChoice);

                testVariableLevelsCombinations.Add(testVariableLevelsCombination);
            }

            return testVariableLevelsCombinations;
        }

        private static TestVariableLevelToContainingCombinationMultiMap MakeTestVariableLevelToContainingCombinationMultiMap(
            SetOfCombinations testVariableLevelsCombinations)
        {
            var testVariableLevelToContainingCombinationMultiMap =
                new TestVariableLevelToContainingCombinationMultiMap(false);

            Algorithms.ForEach(testVariableLevelsCombinations, combination => Algorithms.ForEach(combination,
                                                                                              testCase =>
                                                                                              testVariableLevelToContainingCombinationMultiMap
                                                                                                  .Add(testCase,
                                                                                                       combination)));
            return testVariableLevelToContainingCombinationMultiMap;
        }

        private static UInt32 ChooseHowManyDegreesOfFreedomRequiredToAllowAllCombinationsToBeCovered(
            SetOfCombinations testVariableLevelsCombinations,
            Random randomChoice,
            ITestCaseEnumeratorFactory testCaseGenerator)
        {
            var maximumCombinationWidth =
                Algorithms.Maximum(Algorithms.Convert(testVariableLevelsCombinations, combination => (UInt32)
                                                                                                  combination.Count));

            return
                (UInt32)
                randomChoice.Next((Int32) maximumCombinationWidth,
                                  (Int32) (testCaseGenerator.MaximumStrength + 1U));
        }

        private static void StrikeOffCombinationsCoveredByTestCase(AbstractTestCase testCase,
                                                                   TestVariableLevelToContainingCombinationMultiMap
                                                                       testVariableLevelToContainingCombinationMultiMap,
                                                                   SetOfCombinations testVariableLevelsCombinations)
        {
            C5.IDictionary<CombinationOfTestVariableLevels, UInt32>
                combinationToNumberOfTestVariableLevelComponentsLeftToFindMap =
                    new HashDictionary<CombinationOfTestVariableLevels, UInt32>();

            Algorithms.ForEach(testVariableLevelsCombinations,
                               combination => combinationToNumberOfTestVariableLevelComponentsLeftToFindMap[combination] =
                                              (UInt32) combination.Count);

            Algorithms.ForEach(testCase.TestVariableLevels(), testVariableLevel =>
                                                               {
                                                                   if (
                                                                       testVariableLevelToContainingCombinationMultiMap.
                                                                           ContainsKey(testVariableLevel))
                                                                   {
                                                                       Algorithms.ForEach(
                                                                           testVariableLevelToContainingCombinationMultiMap[
                                                                               testVariableLevel],
                                                                           combination => --
                                                                                          combinationToNumberOfTestVariableLevelComponentsLeftToFindMap
                                                                                              [combination]);
                                                                   }
                                                               });

            Algorithms.ForEach<C5.KeyValuePair<CombinationOfTestVariableLevels, uint>>(
                combinationToNumberOfTestVariableLevelComponentsLeftToFindMap,
                combinationAndNumberOfTestVariableLevelComponentsLeftToFindPair =>
                    {
                        if (combinationAndNumberOfTestVariableLevelComponentsLeftToFindPair.Value == 0U)
                        {
                            testVariableLevelsCombinations.Remove(
                                combinationAndNumberOfTestVariableLevelComponentsLeftToFindPair.Key);
                        }
                    });
        }

        private static void CheckThatAllCombinationsAreCovered(IEnumerator testCaseEnumerator,
                                                               TestVariableLevelToContainingCombinationMultiMap
                                                                   testVariableLevelToContainingCombinationMultiMap,
                                                               SetOfCombinations testVariableLevelsCombinations)
        {
            var testVariableLevelsCombinationsToBeStruckOff = (SetOfCombinations) testVariableLevelsCombinations.Clone();

            while (testCaseEnumerator.MoveNext())
            {
                var testCase = (AbstractTestCase) testCaseEnumerator.Current;

                StrikeOffCombinationsCoveredByTestCase(testCase, testVariableLevelToContainingCombinationMultiMap,
                                                       testVariableLevelsCombinationsToBeStruckOff);
            }

            Assert.IsTrue(testVariableLevelsCombinationsToBeStruckOff.Count == 0U);
        }

        private static SetOfCombinations PickTheLargestCombinationsOfSizeNotExceeding(
            SequenceOfTestVariableLevels testCases,
            UInt32 maximumCombinationWidth)
        {
            return PickAllCombinationsOfSize(testCases, Math.Min(maximumCombinationWidth, (UInt32) testCases.Count));
        }

        private static SetOfCombinations PickAllCombinationsOfSize(SequenceOfTestVariableLevels testCases,
                                                                   UInt32 combinationSize)
        {
            if (!(testCases.Count >= combinationSize))
            {
                throw new PreconditionViolation("Attempt to pick out combination larger than size of source collection.");
            }

            if (0U == combinationSize)
            {
                var trivialResult = new SetOfCombinations {new CombinationOfTestVariableLevels()};

                return trivialResult;
            }

            if (testCases.Count == combinationSize)
            {
                var trivialResult = new SetOfCombinations();

                var onlyCombination = new CombinationOfTestVariableLevels();
                onlyCombination.AddAll(testCases);

                trivialResult.Add(onlyCombination);

                return trivialResult;
            }

            // Alas, neither [head::tail] nor [head:tail] ...

            var tailTestCases = testCases.View(1, testCases.Count - 1);

            var headTestCase = testCases[0];

            var result = new SetOfCombinations();

            result.AddAll(
                Algorithms.Convert(PickAllCombinationsOfSize(tailTestCases, combinationSize - 1U),
                                   combination =>
                                       {
                                           var value = (CombinationOfTestVariableLevels) combination.Clone();

                                           value.Add(headTestCase);

                                           return value;
                                       }));

            result.AddAll(PickAllCombinationsOfSize(tailTestCases, combinationSize));

            return result;
        }

        private static void CheckThatAtLeastOneCombinationIsNotCovered(AbstractTestCase testCaseToBeExcluded,
                                                                       IEnumerator testCaseEnumeratorTwo,
                                                                       TestVariableLevelToContainingCombinationMultiMap
                                                                           testVariableLevelToContainingCombinationMultiMap,
                                                                       SetOfCombinations testVariableLevelsCombinations)
        {
            var testVariableLevelsCombinationsToBeStruckOff = (SetOfCombinations) testVariableLevelsCombinations.Clone();

            while (testCaseEnumeratorTwo.MoveNext())
            {
                var testCase = (AbstractTestCase) testCaseEnumeratorTwo.Current;

                if (!testCase.Equals(testCaseToBeExcluded))
                {
                    StrikeOffCombinationsCoveredByTestCase(testCase, testVariableLevelToContainingCombinationMultiMap,
                                                           testVariableLevelsCombinationsToBeStruckOff);
                }
            }

            Assert.IsTrue(testVariableLevelsCombinationsToBeStruckOff.Count > 0U);
        }

        private delegate void TestATestCaseGenerator(
            ITestCaseEnumeratorFactory testCaseGeneratorWithoutCollisions,
            ITestCaseEnumeratorFactory testCaseGeneratorWithCollisions);

        private static void ForABunchOfTestCaseGenerators(TestATestCaseGenerator test)
        {
            var randomChoice = new Random(0);

            var countDown = 201U;

            while (countDown-- != 0U)
            {
                var sharedSeed = randomChoice.Next();
                test(CreateRandomlyAssembledTestCaseGenerator(new Random(sharedSeed), false),
                     CreateRandomlyAssembledTestCaseGenerator(new Random(sharedSeed), true));
            }
        }

        private static SequenceOfCollectionsOwningTestVariableLevels SequenceOfCollectionsOwningTestVariableLevels(
            AbstractTestCase testCase)
        {
            return Algorithms.Convert(testCase.TestVariableLevels(),
                                      testVariableLevel => testVariableLevel.OwningCollection);
        }

        public interface ITestCaseGeneratorIntrusiveTestHooks
        {
            UInt32 MaximumNumberOfOwningSetsInSequence { get; }
            SetOfSequencesOfCollectionsOwningTestVariableLevels PossibleSequencesOfCollectionsOwningTestVariableLevels();

            CombinationOfTestVariableLevels PickFeasibleCombinationOfTestVariableLevels(Random randomChoice);
        }

        public class TestVariableLevelEnumeratorFactory : TestInfrastructure.TestVariableLevelEnumeratorFactory,
                                                          ITestCaseGeneratorIntrusiveTestHooks
        {
            private readonly CollectionOwningTestVariableLevels _owningCollection = new HashBag<TestVariableLevel>();

            private TestVariableLevelEnumeratorFactory(CollectionOwningTestVariableLevels owningCollection)
                : base(owningCollection.ToArray())
            {
                if (!(owningCollection.Count > 0U))
                {
                    throw new PreconditionViolation(
                        "There must be at least one test variable level.");
                }

                _owningCollection = owningCollection;
            }

            #region ITestCaseGeneratorIntrusiveTestHooks Members

            public SetOfSequencesOfCollectionsOwningTestVariableLevels PossibleSequencesOfCollectionsOwningTestVariableLevels()
            {
                var result = new SetOfSequencesOfCollectionsOwningTestVariableLevels();

                System.Collections.Generic.ICollection<CollectionOwningTestVariableLevels> singletonSequence =
                    new List<CollectionOwningTestVariableLevels> {_owningCollection};

                result.Add(singletonSequence);

                return result;
            }

            public UInt32 MaximumNumberOfOwningSetsInSequence
            {
                get { return 1U; }
            }

            public CombinationOfTestVariableLevels PickFeasibleCombinationOfTestVariableLevels(Random randomChoice)
            {
                var result = new CombinationOfTestVariableLevels();
                result.AddAll(Algorithms.RandomSubset(_owningCollection, 1, randomChoice));
                return result;
            }

            #endregion

            public static TestVariableLevelEnumeratorFactory Create(Random randomChoice, Int32? equivalenceIndex)
            {
                Console.WriteLine('E');

                const UInt32 maximumNumberOfTestCasesInCollection = 10;

                CollectionOwningTestVariableLevels owningCollection = new HashBag<TestVariableLevel>();

                var countDown = (UInt32) randomChoice.Next((Int32) maximumNumberOfTestCasesInCollection) + 1U;

                while (countDown-- != 0U)
                {
                    TestVariableLevel.PutNewTestCaseInto(owningCollection);
                }

                if (equivalenceIndex.HasValue)
                {
                    TestVariableLevel.PutNewTestCaseInto(owningCollection, equivalenceIndex.Value);
                }

                return new TestVariableLevelEnumeratorFactory(owningCollection);
            }
        }

        private static int? RandomlySteppedEquivalenceIndex(Random randomChoice, int? equivalenceIndex)
        {
            return randomChoice.Next(2) == 1
                       ? equivalenceIndex + 1
                       : equivalenceIndex - 1;
        }

        public class InterleavedTestCaseEnumeratorFactory : TestInfrastructure.InterleavedTestCaseEnumeratorFactory,
                                                            ITestCaseGeneratorIntrusiveTestHooks
        {
            private readonly HashSet<ITestCaseEnumeratorFactory> _testCaseGenerators;

            private InterleavedTestCaseEnumeratorFactory(HashSet<ITestCaseEnumeratorFactory> testCaseGenerators)
                : base(testCaseGenerators)
            {
                if (!(testCaseGenerators.Count > 0U))
                {
                    throw new PreconditionViolation(
                        "There must be at least one alternative test case generator to choose from.");
                }

                _testCaseGenerators = testCaseGenerators;
            }

            #region ITestCaseGeneratorIntrusiveTestHooks Members

            public SetOfSequencesOfCollectionsOwningTestVariableLevels PossibleSequencesOfCollectionsOwningTestVariableLevels()
            {
                var result = new SetOfSequencesOfCollectionsOwningTestVariableLevels();

                foreach (var testCaseGenerator in _testCaseGenerators)
                {
                    result.AddAll(
                        ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).
                            PossibleSequencesOfCollectionsOwningTestVariableLevels());
                }

                return result;
            }

            public UInt32 MaximumNumberOfOwningSetsInSequence
            {
                get
                {
                    if (!(_testCaseGenerators.Count > 0))
                    {
                        throw new PreconditionViolation(
                            "Must have at least one child test case generator for this property to be defined.");
                    }

                    if (_testCaseGenerators.Count > 0U)
                    {
                        return
                            Algorithms.Maximum(
                                Algorithms.Convert(_testCaseGenerators,
                                                   testCaseGenerator => testCaseGenerator.MaximumStrength));
                    }
                    else
                    {
                        throw new InternalAssertionViolation(
                            "This property must be strictly greater than zero as a postcondition.");
                    }
                }
            }

            public CombinationOfTestVariableLevels PickFeasibleCombinationOfTestVariableLevels(Random randomChoice)
            {
                return
                    ((ITestCaseGeneratorIntrusiveTestHooks)
                     Algorithms.RandomSubset(_testCaseGenerators, 1, randomChoice)[0]).
                        PickFeasibleCombinationOfTestVariableLevels(randomChoice);
            }

            #endregion

            public static InterleavedTestCaseEnumeratorFactory Create(Random randomChoice,
                                                                      UInt32 treeDepth,
                                                                      UInt32 maximumDegreesOfFreedom,
                                                                      Int32? equivalenceIndex)
            {
                Console.WriteLine('A');

                var testCaseGenerators = new HashSet<ITestCaseEnumeratorFactory>();

                const UInt32 maximumNumberOfAlternativeTestCaseGenerators = 5;

                var countDown = (UInt32) randomChoice.Next((Int32) maximumNumberOfAlternativeTestCaseGenerators) + 1U;

                while (countDown-- != 0U)
                {
                    var maximumDegreesOfFreedomForChild =
                        (UInt32) randomChoice.Next((Int32) (maximumDegreesOfFreedom)) + 1U;
                    testCaseGenerators.Add(
                        CreateRandomlyAssembledTestCaseGenerator(randomChoice, treeDepth,
                                                                 maximumDegreesOfFreedomForChild,
                                                                 RandomlySteppedEquivalenceIndex(randomChoice,
                                                                                                 equivalenceIndex)));
                }

                return new InterleavedTestCaseEnumeratorFactory(testCaseGenerators);
            }
        }

        public class SynthesizedTestCaseEnumeratorFactory : TestInfrastructure.SynthesizedTestCaseEnumeratorFactory,
                                                            ITestCaseGeneratorIntrusiveTestHooks
        {
            #region Delegates

            public delegate AbstractTestCase PermutingClosure(IEnumerable<AbstractTestCase> input);

            #endregion

            private const string _generatedMethodName = "Invoke";

            private static readonly C5.IDictionary<UInt32, Pair<Type, Type>>
                _arityToPermutingClosureAdapterTypeAndDelegateTypePairMap =
                    new HashDictionary<UInt32, Pair<Type, Type>>();

            private static readonly AssemblyBuilder _assemblyBuilder;
            private static readonly ModuleBuilder _moduleBuilder;
            private static UInt32 _namespaceNameGenerationState;
            private readonly C5.IList<ITestCaseEnumeratorFactory> _testCaseGenerators;

            static SynthesizedTestCaseEnumeratorFactory()
            {
                var assemblyName = GetNonConflictingAssemblyName();

                var assemblyNameThingie = new AssemblyName {Name = assemblyName};

                _assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyNameThingie,
                                                                                 AssemblyBuilderAccess.Run);

                const String moduleName = "theBigLebowski";

                _moduleBuilder = _assemblyBuilder.DefineDynamicModule(moduleName);
            }

            private SynthesizedTestCaseEnumeratorFactory(C5.IList<ITestCaseEnumeratorFactory> testCaseGenerators,
                                                         PermutingClosure combiningClosure)
                : base(testCaseGenerators, AdaptPermutingClosure(combiningClosure, (UInt32) testCaseGenerators.Count))
            {
                _testCaseGenerators = testCaseGenerators;
            }

            #region ITestCaseGeneratorIntrusiveTestHooks Members

            public SetOfSequencesOfCollectionsOwningTestVariableLevels PossibleSequencesOfCollectionsOwningTestVariableLevels()
            {
                var result = new SetOfSequencesOfCollectionsOwningTestVariableLevels();

                ConcatenateCrossProductOfSequences(_testCaseGenerators, new List<CollectionOwningTestVariableLevels>(),
                                                   result);

                return result;
            }

            public UInt32 MaximumNumberOfOwningSetsInSequence
            {
                get
                {
                    if (!(_testCaseGenerators.Count > 0))
                    {
                        throw new PreconditionViolation(
                            "Must have at least one child test case generator for this property to be defined.");
                    }

                    // Where is the 'fold-left' algorithm for .NET? :-(
                    var result = 0U;
                    Algorithms.ForEach(_testCaseGenerators,
                                       testCaseGenerator => result += testCaseGenerator.MaximumStrength);

                    if (!(result > 0U))
                    {
                        throw new InternalAssertionViolation(
                            "This property must be strictly greater than zero as a postcondition.");
                    }

                    return result;
                }
            }

            public CombinationOfTestVariableLevels PickFeasibleCombinationOfTestVariableLevels(Random randomChoice)
            {
                var numberOfChildren = (UInt32) randomChoice.Next(_testCaseGenerators.Count) + 1U;

                var sequenceOfCombinationsOfTestVariableLevelsFromChildren =
                    Algorithms.Convert(
                        Algorithms.RandomSubset(_testCaseGenerators, (Int32) numberOfChildren, randomChoice),
                        testCaseGenerator => ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).
                                                 PickFeasibleCombinationOfTestVariableLevels
                                                 (randomChoice));

                var result = new CombinationOfTestVariableLevels();

                result.AddAll(
                    Algorithms.Concatenate(Algorithms.ToArray(sequenceOfCombinationsOfTestVariableLevelsFromChildren)));

                return result;
            }

            #endregion

            public static SynthesizedTestCaseEnumeratorFactory Create(Random randomChoice,
                                                                      UInt32 treeDepth,
                                                                      UInt32 maximumDegreesOfFreedom,
                                                                      Int32? equivalenceIndex)
            {
                Console.WriteLine('C');

                var testCaseGenerators = new C5.LinkedList<ITestCaseEnumeratorFactory>();

                var numberOfPartitionPoints = (UInt32) randomChoice.Next((Int32) maximumDegreesOfFreedom) + 1U;

                var potentialPartitionPoints = new TreeSet<UInt32>();

                for (var potentialPartitionPoint = 1U;
                     potentialPartitionPoint <= maximumDegreesOfFreedom;
                     ++potentialPartitionPoint)
                {
                    potentialPartitionPoints.Add(potentialPartitionPoint);
                }

                var selectedPartitionPoints = new TreeSet<UInt32>();

                selectedPartitionPoints.AddAll(Algorithms.RandomSubset(potentialPartitionPoints,
                                                                       (Int32) numberOfPartitionPoints, randomChoice));

                selectedPartitionPoints.Add(1U);
                // We always put this into the set and use the iterator down below to pick it back out during set up
                // for the loop below. This means we don't have to worry about whether we might encounter the value of
                // one within the loop, which is a messy special case...
                selectedPartitionPoints.Add(maximumDegreesOfFreedom);
                // ... and similarly, we always put the final endpoint for the highest partition
                // in, so that our loop never runs short. If we only have one point to work with,
                // it will have the value of one and therefore will be processed as a degenerate
                // case via the loop setup.

                var partitionPointIterator = selectedPartitionPoints.GetEnumerator();

                partitionPointIterator.MoveNext();

                var partitionStart = partitionPointIterator.Current;

                if (partitionPointIterator.MoveNext())
                {
                    do
                    {
                        var nextPartitionStart = partitionPointIterator.Current;
                        testCaseGenerators.Add(
                            CreateRandomlyAssembledTestCaseGenerator(randomChoice, treeDepth,
                                                                     nextPartitionStart - partitionStart,
                                                                     RandomlySteppedEquivalenceIndex(randomChoice,
                                                                                                     equivalenceIndex)));
                        partitionStart = nextPartitionStart;
                    } while (partitionPointIterator.MoveNext());
                }
                else
                {
                    testCaseGenerators.Add(
                        CreateRandomlyAssembledTestCaseGenerator(randomChoice, treeDepth, partitionStart,
                                                                 RandomlySteppedEquivalenceIndex(randomChoice,
                                                                                                 equivalenceIndex)));
                }

                return new SynthesizedTestCaseEnumeratorFactory(testCaseGenerators, CreatePermutingClosure(randomChoice));
            }

            private static PermutingClosure CreatePermutingClosure(Random randomChoice)
            {
                int seed = randomChoice.Next();
                return
                    input => ComposedTestCase.MakeShuffledCombination(input, seed);
            }

            private static String CreateUniqueIdentifier()
            {
                const UInt32 rangeOfAlphabet = 26U;

                uint numberToBeInterpretedAccordingToABase = _namespaceNameGenerationState++;

                var builder = new StringBuilder();

                do
                {
                    builder.Append('a' + numberToBeInterpretedAccordingToABase%rangeOfAlphabet);
                    numberToBeInterpretedAccordingToABase /= rangeOfAlphabet;
                } while (numberToBeInterpretedAccordingToABase != 0U);

                return builder.ToString();
            }

            private static Delegate AdaptPermutingClosure(PermutingClosure permutingClosure,
                                                          UInt32 numberOfArgumentsForAdaptedClosure)
            {
                Pair<Type, Type> permutingClosureAdapterTypeAndDelegateTypePair;

                if (
                    !_arityToPermutingClosureAdapterTypeAndDelegateTypePairMap.Find(numberOfArgumentsForAdaptedClosure,
                                                                                    out
                                                                                        permutingClosureAdapterTypeAndDelegateTypePair))
                {
                    permutingClosureAdapterTypeAndDelegateTypePair.First =
                        CreatePermutingClosureAdapterType(numberOfArgumentsForAdaptedClosure);
                    permutingClosureAdapterTypeAndDelegateTypePair.Second =
                        CreatePermutingClosureDelegateType(numberOfArgumentsForAdaptedClosure);

                    _arityToPermutingClosureAdapterTypeAndDelegateTypePairMap.Add(numberOfArgumentsForAdaptedClosure,
                                                                                  permutingClosureAdapterTypeAndDelegateTypePair);
                }

                var adapter =
                    (PermutingClosureAdapterSupport)
                    permutingClosureAdapterTypeAndDelegateTypePair.First.GetConstructor(Type.EmptyTypes).Invoke(null);

                adapter.PermutingClosure = permutingClosure;

                return
                    Delegate.CreateDelegate(permutingClosureAdapterTypeAndDelegateTypePair.Second, adapter,
                                            permutingClosureAdapterTypeAndDelegateTypePair.First.GetMethod(
                                                _generatedMethodName));
            }

            private static Type CreatePermutingClosureAdapterType(UInt32 numberOfArgumentsForAdaptedClosure)
            {
                var arityAsString = numberOfArgumentsForAdaptedClosure.ToString();

                var typeBuilder =
                    _moduleBuilder.DefineType("_PermutingClosureAdapterFor" + arityAsString + "Arguments_" +
                                              CreateUniqueIdentifier());

                typeBuilder.SetParent(typeof (PermutingClosureAdapterSupport));

                var argumentTypes = CreateArgumentTypesForAdaptedClosure(numberOfArgumentsForAdaptedClosure);

                var methodAttributes = MethodAttributes.Public;

                var methodBuilder =
                    typeBuilder.DefineMethod(_generatedMethodName, methodAttributes, typeof (AbstractTestCase),
                                             argumentTypes);

                var codeGenerator = methodBuilder.GetILGenerator();

                var collectionType = typeof (List<AbstractTestCase>);

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
                                                                                    BindingFlags.NonPublic |
                                                                                    BindingFlags.Instance));
                codeGenerator.Emit(OpCodes.Ldloc_0);
                codeGenerator.Emit(OpCodes.Callvirt, typeof (PermutingClosure).GetMethod("Invoke"));
                codeGenerator.Emit(OpCodes.Ret);

                var permutingClosureAdapterType = typeBuilder.CreateType();

                return permutingClosureAdapterType;
            }

            private static Type CreatePermutingClosureDelegateType(UInt32 numberOfArgumentsForAdaptedClosure)
            {
                var arityAsString = numberOfArgumentsForAdaptedClosure.ToString();

                var typeAttributes = TypeAttributes.Class | TypeAttributes.Public | TypeAttributes.Sealed |
                                     TypeAttributes.AnsiClass
                                     | TypeAttributes.AutoClass;

                var typeBuilder =
                    _moduleBuilder.DefineType(
                        "_PermutingClosureDelegateFor" + arityAsString + "Arguments_" + CreateUniqueIdentifier(),
                        typeAttributes);

                typeBuilder.SetParent(typeof (MulticastDelegate));

                var constructorArgumentTypes = new[] {typeof (Object), typeof (IntPtr)};

                var constructorAttributes = MethodAttributes.RTSpecialName | MethodAttributes.HideBySig |
                                            MethodAttributes.Public;

                var constructorBuilder =
                    typeBuilder.DefineConstructor(constructorAttributes, CallingConventions.Standard,
                                                  constructorArgumentTypes);

                constructorBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);


                var methodArgumentTypes = CreateArgumentTypesForAdaptedClosure(numberOfArgumentsForAdaptedClosure);

                var methodAttributes = MethodAttributes.Public | MethodAttributes.HideBySig |
                                       MethodAttributes.NewSlot
                                       | MethodAttributes.Virtual;

                var invokeMethodName = "Invoke";

                var methodBuilder =
                    typeBuilder.DefineMethod(invokeMethodName, methodAttributes, typeof (AbstractTestCase),
                                             methodArgumentTypes);

                methodBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);

                var permutingClosureAdapterType = typeBuilder.CreateType();

                return permutingClosureAdapterType;
            }

            private static Type[] CreateArgumentTypesForAdaptedClosure(UInt32 numberOfArgumentsForAdaptedClosure)
            {
                var result = new Type[numberOfArgumentsForAdaptedClosure];

                Algorithms.Fill(result, typeof (AbstractTestCase));

                return result;
            }

            private static void ConcatenateCrossProductOfSequences(
                C5.IList<ITestCaseEnumeratorFactory> testCaseGenerators,
                SequenceOfCollectionsOwningTestVariableLevels
                    sequenceBeingBuiltUp,
                SetOfSequencesOfCollectionsOwningTestVariableLevels
                    result)
            {
                if (testCaseGenerators.Count == 0)
                {
                    result.Add(sequenceBeingBuiltUp);
                }
                else
                {
                    foreach (var sequence in
                        ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerators[0]).
                            PossibleSequencesOfCollectionsOwningTestVariableLevels())
                    {
                        ConcatenateCrossProductOfSequences(testCaseGenerators.View(1, testCaseGenerators.Count - 1),
                                                           Algorithms.Concatenate(sequenceBeingBuiltUp, sequence),
                                                           result);
                    }
                }
            }

            private static String GetNonConflictingAssemblyName()
            {
                var extantAssemblies = AppDomain.CurrentDomain.GetAssemblies();

                do
                {
                    var assemblyName = CreateUniqueIdentifier();

                    if (Algorithms.FindFirstWhere(extantAssemblies,
                                                  assembly => assembly.GetName().Name == assemblyName)
                        == null)
                    {
                        return assemblyName;
                    }
                } while (true);
            }

            #region Nested type: PermutingClosureAdapterSupport

            public class PermutingClosureAdapterSupport
            {
                private PermutingClosure _permutingClosure;

                public PermutingClosure PermutingClosure
                {
                    set { _permutingClosure = value; }
                }
            }

            #endregion
        }

        public static ITestCaseEnumeratorFactory CreateRandomlyAssembledTestCaseGenerator(Random randomChoice,
                                                                                          Boolean createDuplicatesAsWell)
        {
            Console.WriteLine();

            var upperBoundOfMaximumDegreesOfFreedom = 5U;
            return
                CreateRandomlyAssembledTestCaseGenerator(randomChoice, 0U,
                                                         (UInt32)
                                                         randomChoice.Next((Int32) upperBoundOfMaximumDegreesOfFreedom) +
                                                         1U,
                                                         createDuplicatesAsWell
                                                             ? new Int32?(0)
                                                             : null);
        }

        public static ITestCaseEnumeratorFactory CreateRandomlyAssembledTestCaseGenerator(Random randomChoice,
                                                                                          UInt32 parentTreeDepth,
                                                                                          UInt32 maximumDegreesOfFreedom,
                                                                                          Int32? equivalenceIndex)
        {
            if (maximumDegreesOfFreedom == 0U)
            {
                throw new PreconditionViolation(
                    "The test should specify the maximum degrees of freedom as being strictly greater than zero.");
            }

            var treeDepth = parentTreeDepth + 1U;

            {
                UInt32 countDown = treeDepth;

                Console.Write(treeDepth);

                while (--countDown != 0U)
                {
                    Console.Write(' ');
                }
            }

            var maximumPermittedTreeDepth = 9U;

            if (maximumDegreesOfFreedom == 1U && randomChoice.Next((Int32) maximumPermittedTreeDepth) + 1U <= treeDepth)
            {
                return TestVariableLevelEnumeratorFactory.Create(randomChoice, equivalenceIndex);
            }
            else
            {
                switch (randomChoice.Next(2))
                {
                    case 0:
                        return InterleavedTestCaseEnumeratorFactory.Create(randomChoice, treeDepth,
                                                                           maximumDegreesOfFreedom,
                                                                           equivalenceIndex);

                    case 1:
                        return SynthesizedTestCaseEnumeratorFactory.Create(randomChoice, treeDepth,
                                                                           maximumDegreesOfFreedom,
                                                                           equivalenceIndex);

                    default:
                        throw new InternalAssertionViolation("Default of this switch should not be executed.");
                }
            }
        }

        [Test]
        public void TestCorrectOrderingOfTestVariableLevelsWithinEachOutputTestCase()
        {
            var randomChoice = new Random(0);

            ForABunchOfTestCaseGenerators(
                (testCaseGeneratorWithoutCollisions, testCaseGeneratorWithCollisions) =>
                    {
                        var requestedDegreesOfFreedomForCombinationCoverage =
                            (UInt32)
                            randomChoice.Next((Int32) (testCaseGeneratorWithoutCollisions.MaximumStrength + 1U));

                        var testCaseIterator =
                            testCaseGeneratorWithoutCollisions.CreateEnumerator(
                                requestedDegreesOfFreedomForCombinationCoverage);

                        var
                            possibleSequencesOfCollectionsOwningTestVariableLevels =
                                ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGeneratorWithoutCollisions).
                                    PossibleSequencesOfCollectionsOwningTestVariableLevels();

                        while (testCaseIterator.MoveNext())
                        {
                            var testCase = (AbstractTestCase) testCaseIterator.Current;

                            var
                                sequenceOfOwningCollectionsThatContributedToThisTestCase =
                                    SequenceOfCollectionsOwningTestVariableLevels(testCase);

                            Assert.IsTrue(
                                possibleSequencesOfCollectionsOwningTestVariableLevels.Contains(
                                    sequenceOfOwningCollectionsThatContributedToThisTestCase));
                        }
                    });
        }

        [Test]
        public void TestCoverageOfNWayCombinationsOfTestVariableLevelsOverAllOutputTestCases()
        {
            var randomChoice = new Random(0);

            ForABunchOfTestCaseGenerators(
                (testCaseGeneratorWithoutCollisions, testCaseGeneratorWithCollisions) =>
                    {
                        var testVariableLevelsCombinations =
                            MakeTestVariableLevelsCombinations(testCaseGeneratorWithoutCollisions, randomChoice);

                        var testVariableLevelToContainingCombinationMultiMap =
                            MakeTestVariableLevelToContainingCombinationMultiMap(testVariableLevelsCombinations);

                        var numberOfDegreesOfFreedom =
                            ChooseHowManyDegreesOfFreedomRequiredToAllowAllCombinationsToBeCovered(
                                testVariableLevelsCombinations, randomChoice,
                                testCaseGeneratorWithoutCollisions);

                        {
                            var testCaseEnumerator =
                                testCaseGeneratorWithoutCollisions.CreateEnumerator(numberOfDegreesOfFreedom);

                            CheckThatAllCombinationsAreCovered(testCaseEnumerator,
                                                               testVariableLevelToContainingCombinationMultiMap,
                                                               testVariableLevelsCombinations);
                        }

                        {
                            var testCaseEnumerator =
                                testCaseGeneratorWithCollisions.CreateEnumerator(numberOfDegreesOfFreedom);

                            CheckThatAllCombinationsAreCovered(testCaseEnumerator,
                                                               testVariableLevelToContainingCombinationMultiMap,
                                                               testVariableLevelsCombinations);
                        }
                    });
        }

        [Test]
        public void TestMaximumDegreesOfFreedom()
        {
            ForABunchOfTestCaseGenerators(
                (testCaseGeneratorWithoutCollisions, testCaseGeneratorWithCollisions) =>
                Assert.AreEqual(testCaseGeneratorWithoutCollisions.MaximumStrength,
                                ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGeneratorWithoutCollisions).
                                    MaximumNumberOfOwningSetsInSequence));
        }

        [Test]
        public void TestOptimalityOfCoverageOfAllNWayCombinationsOfTestVariableLevelsOverAllOutputTestCases()
        {
            var randomChoice = new Random(0);

            ForABunchOfTestCaseGenerators(
                (testCaseGeneratorWithoutCollisions, testCaseGeneratorWithCollisions) =>
                    {
                        var maximumDegreesOfFreedom =
                            (UInt32)
                            randomChoice.Next((Int32) testCaseGeneratorWithoutCollisions.MaximumStrength) + 1U;

                        var testCases = new HashSet<AbstractTestCase>();

                        var testCaseEnumerator =
                            testCaseGeneratorWithoutCollisions.CreateEnumerator(maximumDegreesOfFreedom);

                        while (testCaseEnumerator.MoveNext())
                        {
                            testCases.Add((AbstractTestCase) testCaseEnumerator.Current);
                        }

                        var testCaseToBeExcluded = Algorithms.RandomSubset(testCases, 1, randomChoice)[0];

                        var testVariableLevelsCombinations =
                            PickTheLargestCombinationsOfSizeNotExceeding(testCaseToBeExcluded.TestVariableLevels(),
                                                                         maximumDegreesOfFreedom == 0U
                                                                             ? testCaseGeneratorWithoutCollisions
                                                                                   .MaximumStrength
                                                                             : maximumDegreesOfFreedom);

                        var testVariableLevelToContainingCombinationMultiMap =
                            MakeTestVariableLevelToContainingCombinationMultiMap(testVariableLevelsCombinations);

                        {
                            var testCaseEnumeratorTwo =
                                testCaseGeneratorWithoutCollisions.CreateEnumerator(maximumDegreesOfFreedom);

                            CheckThatAtLeastOneCombinationIsNotCovered(testCaseToBeExcluded, testCaseEnumeratorTwo,
                                                                       testVariableLevelToContainingCombinationMultiMap,
                                                                       testVariableLevelsCombinations);
                        }

                        {
                            var testCaseEnumeratorTwo =
                                testCaseGeneratorWithCollisions.CreateEnumerator(maximumDegreesOfFreedom);

                            CheckThatAtLeastOneCombinationIsNotCovered(testCaseToBeExcluded, testCaseEnumeratorTwo,
                                                                       testVariableLevelToContainingCombinationMultiMap,
                                                                       testVariableLevelsCombinations);
                        }
                    });
        }
    }
}