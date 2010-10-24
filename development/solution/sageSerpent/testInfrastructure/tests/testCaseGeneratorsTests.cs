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
    using AtomicTestCaseToContainingCombinationMultiMap = MultiDictionary<AtomicTestCase, HashSet<AtomicTestCase>>;
    using CombinationOfAtomicTestCases = HashSet<AtomicTestCase>;
    using SequenceOfAtomicTestCases = C5.IList<AtomicTestCase>;
    using CollectionOwningAtomicTestCases = C5.ICollection<AtomicTestCase>;
    using SequenceOfCollectionsOwningAtomicTestCases = IEnumerable<C5.ICollection<AtomicTestCase>>;
    using SetOfCombinations = HashSet<HashSet<AtomicTestCase>>;
    using SetOfSequencesOfCollectionsOwningAtomicTestCases = HashSet<IEnumerable<C5.ICollection<AtomicTestCase>>>;

    public abstract class AbstractTestCase
    {
        public abstract SequenceOfAtomicTestCases AtomicTestCases();

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

    public class AtomicTestCase : AbstractTestCase
    {
        private readonly CollectionOwningAtomicTestCases _owningCollection;

        private Int32 _equivalenceIndex;
        private Boolean _equivalenceIndexExists;

        private AtomicTestCase(CollectionOwningAtomicTestCases owningCollection)
        {
            _owningCollection = owningCollection;
        }

        public CollectionOwningAtomicTestCases OwningCollection
        {
            get { return _owningCollection; }
        }

        public override SequenceOfAtomicTestCases AtomicTestCases()
        {
            var singletonSequence = new C5.LinkedList<AtomicTestCase> {this};

            return singletonSequence;
        }

        public static void PutNewTestCaseInto(CollectionOwningAtomicTestCases owningCollection)
        {
            var testCase = new AtomicTestCase(owningCollection);
            owningCollection.Add(testCase);
        }

        public static void PutNewTestCaseInto(CollectionOwningAtomicTestCases owningCollection, Int32 equivalenceIndex)
        {
            var testCase = new AtomicTestCase(owningCollection);
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
            var anotherOfCompatibleType = another as AtomicTestCase;

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

        public override SequenceOfAtomicTestCases AtomicTestCases()
        {
            var result = new C5.LinkedList<AtomicTestCase>();
            result.AddAll
                (
                Algorithms.Concatenate(
                    Algorithms.ToArray(
                        Algorithms.Convert(_childTestCases,
                                           testCase => testCase.AtomicTestCases()))));

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

        private static SetOfCombinations MakeAtomicTestCasesCombinations(ITestCaseGenerator testCaseGenerator,
                                                                         Random randomChoice)
        {
            var numberOfCombinationsLeftToTry = 100U;
            // NOTE: we may get some repeated combinations, or
            // permutations of the same combination. This is OK,
            // as long as it doesn't happen too often or as long as
            // it only occurs because there aren't enough combinations
            // to fulfill the test's expectations.

            var atomicTestCasesCombinations = new SetOfCombinations();


            while (numberOfCombinationsLeftToTry-- != 0U)
            {
                var atomicTestCasesCombination =
                    ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).PickFeasibleCombinationOfAtomicTestCases(
                        randomChoice);

                atomicTestCasesCombinations.Add(atomicTestCasesCombination);
            }

            return atomicTestCasesCombinations;
        }

        private static AtomicTestCaseToContainingCombinationMultiMap MakeAtomicStateToContainingCombinationMultiMap(
            SetOfCombinations atomicTestCasesCombinations)
        {
            var atomicStateToContainingCombinationMultiMap =
                new AtomicTestCaseToContainingCombinationMultiMap(false);

            Algorithms.ForEach(atomicTestCasesCombinations, combination => Algorithms.ForEach(combination,
                                                                                              testCase =>
                                                                                              atomicStateToContainingCombinationMultiMap
                                                                                                  .Add(testCase,
                                                                                                       combination)));
            return atomicStateToContainingCombinationMultiMap;
        }

        private static UInt32 ChooseHowManyDegreesOfFreedomRequiredToAllowAllCombinationsToBeCovered(
            SetOfCombinations atomicTestCasesCombinations,
            Random randomChoice,
            ITestCaseGenerator testCaseGenerator)
        {
            var maximumCombinationWidth =
                Algorithms.Maximum(Algorithms.Convert(atomicTestCasesCombinations, combination => (UInt32)
                                                                                                  combination.Count));

            return
                (UInt32)
                randomChoice.Next((Int32) maximumCombinationWidth,
                                  (Int32) (testCaseGenerator.MaximumDegreesOfFreedom + 1U));
        }

        private static void StrikeOffCombinationsCoveredByTestCase(AbstractTestCase testCase,
                                                                   AtomicTestCaseToContainingCombinationMultiMap
                                                                       atomicStateToContainingCombinationMultiMap,
                                                                   SetOfCombinations atomicTestCasesCombinations)
        {
            C5.IDictionary<CombinationOfAtomicTestCases, UInt32>
                combinationToNumberOfAtomicTestCaseComponentsLeftToFindMap =
                    new HashDictionary<CombinationOfAtomicTestCases, UInt32>();

            Algorithms.ForEach(atomicTestCasesCombinations,
                               combination => combinationToNumberOfAtomicTestCaseComponentsLeftToFindMap[combination] =
                                              (UInt32) combination.Count);

            Algorithms.ForEach(testCase.AtomicTestCases(), atomicTestCase =>
                                                               {
                                                                   if (
                                                                       atomicStateToContainingCombinationMultiMap.
                                                                           ContainsKey(atomicTestCase))
                                                                   {
                                                                       Algorithms.ForEach(
                                                                           atomicStateToContainingCombinationMultiMap[
                                                                               atomicTestCase],
                                                                           combination => --
                                                                                          combinationToNumberOfAtomicTestCaseComponentsLeftToFindMap
                                                                                              [combination]);
                                                                   }
                                                               });

            Algorithms.ForEach<C5.KeyValuePair<CombinationOfAtomicTestCases, uint>>(
                combinationToNumberOfAtomicTestCaseComponentsLeftToFindMap,
                combinationAndNumberOfAtomicTestCaseComponentsLeftToFindPair =>
                    {
                        if (combinationAndNumberOfAtomicTestCaseComponentsLeftToFindPair.Value == 0U)
                        {
                            atomicTestCasesCombinations.Remove(
                                combinationAndNumberOfAtomicTestCaseComponentsLeftToFindPair.Key);
                        }
                    });
        }

        private static void CheckThatAllCombinationsAreCovered(IEnumerator testCaseEnumerator,
                                                               AtomicTestCaseToContainingCombinationMultiMap
                                                                   atomicStateToContainingCombinationMultiMap,
                                                               SetOfCombinations atomicTestCasesCombinations)
        {
            var atomicTestCasesCombinationsToBeStruckOff = (SetOfCombinations) atomicTestCasesCombinations.Clone();

            while (testCaseEnumerator.MoveNext())
            {
                var testCase = (AbstractTestCase) testCaseEnumerator.Current;

                StrikeOffCombinationsCoveredByTestCase(testCase, atomicStateToContainingCombinationMultiMap,
                                                       atomicTestCasesCombinationsToBeStruckOff);
            }

            Assert.IsTrue(atomicTestCasesCombinationsToBeStruckOff.Count == 0U);
        }

        private static SetOfCombinations PickTheLargestCombinationsOfSizeNotExceeding(
            SequenceOfAtomicTestCases testCases,
            UInt32 maximumCombinationWidth)
        {
            return PickAllCombinationsOfSize(testCases, Math.Min(maximumCombinationWidth, (UInt32) testCases.Count));
        }

        private static SetOfCombinations PickAllCombinationsOfSize(SequenceOfAtomicTestCases testCases,
                                                                   UInt32 combinationSize)
        {
            if (!(testCases.Count >= combinationSize))
            {
                throw new PreconditionViolation("Attempt to pick out combination larger than size of source collection.");
            }

            if (0U == combinationSize)
            {
                var trivialResult = new SetOfCombinations {new CombinationOfAtomicTestCases()};

                return trivialResult;
            }

            if (testCases.Count == combinationSize)
            {
                var trivialResult = new SetOfCombinations();

                var onlyCombination = new CombinationOfAtomicTestCases();
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
                                           var value = (CombinationOfAtomicTestCases) combination.Clone();

                                           value.Add(headTestCase);

                                           return value;
                                       }));

            result.AddAll(PickAllCombinationsOfSize(tailTestCases, combinationSize));

            return result;
        }

        private static void CheckThatAtLeastOneCombinationIsNotCovered(AbstractTestCase testCaseToBeExcluded,
                                                                       IEnumerator testCaseEnumeratorTwo,
                                                                       AtomicTestCaseToContainingCombinationMultiMap
                                                                           atomicStateToContainingCombinationMultiMap,
                                                                       SetOfCombinations atomicTestCasesCombinations)
        {
            var atomicTestCasesCombinationsToBeStruckOff = (SetOfCombinations) atomicTestCasesCombinations.Clone();

            while (testCaseEnumeratorTwo.MoveNext())
            {
                var testCase = (AbstractTestCase) testCaseEnumeratorTwo.Current;

                if (!testCase.Equals(testCaseToBeExcluded))
                {
                    StrikeOffCombinationsCoveredByTestCase(testCase, atomicStateToContainingCombinationMultiMap,
                                                           atomicTestCasesCombinationsToBeStruckOff);
                }
            }

            Assert.IsTrue(atomicTestCasesCombinationsToBeStruckOff.Count > 0U);
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
            var randomChoice = new Random(0);

            var countDown = 201U;

            while (countDown-- != 0U)
            {
                var sharedSeed = randomChoice.Next();
                test(CreateRandomlyAssembledTestCaseGenerator(new Random(sharedSeed), false),
                     CreateRandomlyAssembledTestCaseGenerator(new Random(sharedSeed), true));
            }
        }

        private static SequenceOfCollectionsOwningAtomicTestCases SequenceOfCollectionsOwningAtomicTestCases(
            AbstractTestCase testCase)
        {
            return Algorithms.Convert(testCase.AtomicTestCases(),
                                      atomicTestCase => atomicTestCase.OwningCollection);
        }

        public interface ITestCaseGeneratorIntrusiveTestHooks
        {
            UInt32 MaximumNumberOfOwningSetsInSequence { get; }
            SetOfSequencesOfCollectionsOwningAtomicTestCases PossibleSequencesOfCollectionsOwningAtomicTestCases();

            CombinationOfAtomicTestCases PickFeasibleCombinationOfAtomicTestCases(Random randomChoice);
        }

        public class TestCaseFromCollectionGenerator : TestInfrastructure.TestCaseFromCollectionGenerator,
                                                       ITestCaseGeneratorIntrusiveTestHooks
        {
            private readonly CollectionOwningAtomicTestCases _owningCollection = new HashBag<AtomicTestCase>();

            private TestCaseFromCollectionGenerator(CollectionOwningAtomicTestCases owningCollection)
                : base(owningCollection.ToArray())
            {
                if (!(owningCollection.Count > 0U))
                {
                    throw new PreconditionViolation(
                        "The owning collection of atomic test cases must have at least one element.");
                }

                _owningCollection = owningCollection;
            }

            #region ITestCaseGeneratorIntrusiveTestHooks Members

            public SetOfSequencesOfCollectionsOwningAtomicTestCases PossibleSequencesOfCollectionsOwningAtomicTestCases()
            {
                var result = new SetOfSequencesOfCollectionsOwningAtomicTestCases();

                System.Collections.Generic.ICollection<CollectionOwningAtomicTestCases> singletonSequence =
                    new List<CollectionOwningAtomicTestCases> {_owningCollection};

                result.Add(singletonSequence);

                return result;
            }

            public UInt32 MaximumNumberOfOwningSetsInSequence
            {
                get { return 1U; }
            }

            public CombinationOfAtomicTestCases PickFeasibleCombinationOfAtomicTestCases(Random randomChoice)
            {
                var result = new CombinationOfAtomicTestCases();
                result.AddAll(Algorithms.RandomSubset(_owningCollection, 1, randomChoice));
                return result;
            }

            #endregion

            public static TestCaseFromCollectionGenerator Create(Random randomChoice, Int32? equivalenceIndex)
            {
                Console.WriteLine('E');

                const UInt32 maximumNumberOfTestCasesInCollection = 10;

                CollectionOwningAtomicTestCases owningCollection = new HashBag<AtomicTestCase>();

                var countDown = (UInt32) randomChoice.Next((Int32) maximumNumberOfTestCasesInCollection) + 1U;

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
        }

        private static int? RandomlySteppedEquivalenceIndex(Random randomChoice, int? equivalenceIndex)
        {
            return randomChoice.Next(2) == 1
                       ? equivalenceIndex + 1
                       : equivalenceIndex - 1;
        }

        public class TestCaseFromAlternativesGenerator : TestInfrastructure.TestCaseFromAlternativesGenerator,
                                                         ITestCaseGeneratorIntrusiveTestHooks
        {
            private readonly HashSet<ITestCaseGenerator> _testCaseGenerators;

            private TestCaseFromAlternativesGenerator(HashSet<ITestCaseGenerator> testCaseGenerators)
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

            public SetOfSequencesOfCollectionsOwningAtomicTestCases PossibleSequencesOfCollectionsOwningAtomicTestCases()
            {
                var result = new SetOfSequencesOfCollectionsOwningAtomicTestCases();

                foreach (var testCaseGenerator in _testCaseGenerators)
                {
                    result.AddAll(
                        ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).
                            PossibleSequencesOfCollectionsOwningAtomicTestCases());
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
                                                   testCaseGenerator => testCaseGenerator.MaximumDegreesOfFreedom));
                    }
                    else
                    {
                        throw new InternalAssertionViolation(
                            "This property must be strictly greater than zero as a postcondition.");
                    }
                }
            }

            public CombinationOfAtomicTestCases PickFeasibleCombinationOfAtomicTestCases(Random randomChoice)
            {
                return
                    ((ITestCaseGeneratorIntrusiveTestHooks)
                     Algorithms.RandomSubset(_testCaseGenerators, 1, randomChoice)[0]).
                        PickFeasibleCombinationOfAtomicTestCases(randomChoice);
            }

            #endregion

            public static TestCaseFromAlternativesGenerator Create(Random randomChoice,
                                                                   UInt32 treeDepth,
                                                                   UInt32 maximumDegreesOfFreedom,
                                                                   Int32? equivalenceIndex)
            {
                Console.WriteLine('A');

                var testCaseGenerators = new HashSet<ITestCaseGenerator>();

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

                return new TestCaseFromAlternativesGenerator(testCaseGenerators);
            }
        }

        public class TestCaseFromCombinationGenerator : TestInfrastructure.TestCaseFromCombinationGenerator,
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
            private readonly C5.IList<ITestCaseGenerator> _testCaseGenerators;

            static TestCaseFromCombinationGenerator()
            {
                var assemblyName = GetNonConflictingAssemblyName();

                var assemblyNameThingie = new AssemblyName {Name = assemblyName};

                _assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyNameThingie,
                                                                                 AssemblyBuilderAccess.Run);

                const String moduleName = "theBigLebowski";

                _moduleBuilder = _assemblyBuilder.DefineDynamicModule(moduleName);
            }

            private TestCaseFromCombinationGenerator(C5.IList<ITestCaseGenerator> testCaseGenerators,
                                                     PermutingClosure combiningClosure)
                : base(testCaseGenerators, AdaptPermutingClosure(combiningClosure, (UInt32) testCaseGenerators.Count))
            {
                _testCaseGenerators = testCaseGenerators;
            }

            #region ITestCaseGeneratorIntrusiveTestHooks Members

            public SetOfSequencesOfCollectionsOwningAtomicTestCases PossibleSequencesOfCollectionsOwningAtomicTestCases()
            {
                var result = new SetOfSequencesOfCollectionsOwningAtomicTestCases();

                ConcatenateCrossProductOfSequences(_testCaseGenerators, new List<CollectionOwningAtomicTestCases>(),
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
                                       testCaseGenerator => result += testCaseGenerator.MaximumDegreesOfFreedom);

                    if (!(result > 0U))
                    {
                        throw new InternalAssertionViolation(
                            "This property must be strictly greater than zero as a postcondition.");
                    }

                    return result;
                }
            }

            public CombinationOfAtomicTestCases PickFeasibleCombinationOfAtomicTestCases(Random randomChoice)
            {
                var numberOfChildren = (UInt32) randomChoice.Next(_testCaseGenerators.Count) + 1U;

                var sequenceOfCombinationsOfAtomicTestCasesFromChildren =
                    Algorithms.Convert(
                        Algorithms.RandomSubset(_testCaseGenerators, (Int32) numberOfChildren, randomChoice),
                        testCaseGenerator => ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGenerator).
                                                 PickFeasibleCombinationOfAtomicTestCases
                                                 (randomChoice));

                var result = new CombinationOfAtomicTestCases();

                result.AddAll(
                    Algorithms.Concatenate(Algorithms.ToArray(sequenceOfCombinationsOfAtomicTestCasesFromChildren)));

                return result;
            }

            #endregion

            public static TestCaseFromCombinationGenerator Create(Random randomChoice,
                                                                  UInt32 treeDepth,
                                                                  UInt32 maximumDegreesOfFreedom,
                                                                  Int32? equivalenceIndex)
            {
                Console.WriteLine('C');

                var testCaseGenerators = new C5.LinkedList<ITestCaseGenerator>();

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

                return new TestCaseFromCombinationGenerator(testCaseGenerators, CreatePermutingClosure(randomChoice));
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

            private static void ConcatenateCrossProductOfSequences(C5.IList<ITestCaseGenerator> testCaseGenerators,
                                                                   SequenceOfCollectionsOwningAtomicTestCases
                                                                       sequenceBeingBuiltUp,
                                                                   SetOfSequencesOfCollectionsOwningAtomicTestCases
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
                            PossibleSequencesOfCollectionsOwningAtomicTestCases())
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

        public static ITestCaseGenerator CreateRandomlyAssembledTestCaseGenerator(Random randomChoice,
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

        public static ITestCaseGenerator CreateRandomlyAssembledTestCaseGenerator(Random randomChoice,
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
                return TestCaseFromCollectionGenerator.Create(randomChoice, equivalenceIndex);
            }
            else
            {
                switch (randomChoice.Next(2))
                {
                    case 0:
                        return TestCaseFromAlternativesGenerator.Create(randomChoice, treeDepth, maximumDegreesOfFreedom,
                                                                        equivalenceIndex);

                    case 1:
                        return TestCaseFromCombinationGenerator.Create(randomChoice, treeDepth, maximumDegreesOfFreedom,
                                                                       equivalenceIndex);

                    default:
                        throw new InternalAssertionViolation("Default of this switch should not be executed.");
                }
            }
        }

        [Test]
        public void TestCorrectOrderingOfAtomicTestCasesWithinEachOutputTestCase()
        {
            var randomChoice = new Random(0);

            ForABunchOfTestCaseGenerators(
                (testCaseGeneratorWithoutCollisions, testCaseGeneratorWithCollisions) =>
                    {
                        var requestedDegreesOfFreedomForCombinationCoverage =
                            (UInt32)
                            randomChoice.Next((Int32) (testCaseGeneratorWithoutCollisions.MaximumDegreesOfFreedom + 1U));

                        var testCaseIterator =
                            testCaseGeneratorWithoutCollisions.CreateIterator(
                                requestedDegreesOfFreedomForCombinationCoverage);

                        var
                            possibleSequencesOfCollectionsOwningAtomicTestCases =
                                ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGeneratorWithoutCollisions).
                                    PossibleSequencesOfCollectionsOwningAtomicTestCases();

                        while (testCaseIterator.MoveNext())
                        {
                            var testCase = (AbstractTestCase) testCaseIterator.Current;

                            var
                                sequenceOfOwningCollectionsThatContributedToThisTestCase =
                                    SequenceOfCollectionsOwningAtomicTestCases(testCase);

                            Assert.IsTrue(
                                possibleSequencesOfCollectionsOwningAtomicTestCases.Contains(
                                    sequenceOfOwningCollectionsThatContributedToThisTestCase));
                        }
                    });
        }

        [Test]
        public void TestCoverageOfNWayCombinationsOfAtomicTestCasesOverAllOutputTestCases()
        {
            var randomChoice = new Random(0);

            ForABunchOfTestCaseGenerators(
                (testCaseGeneratorWithoutCollisions, testCaseGeneratorWithCollisions) =>
                    {
                        var atomicTestCasesCombinations =
                            MakeAtomicTestCasesCombinations(testCaseGeneratorWithoutCollisions, randomChoice);

                        var atomicStateToContainingCombinationMultiMap =
                            MakeAtomicStateToContainingCombinationMultiMap(atomicTestCasesCombinations);

                        var numberOfDegreesOfFreedom =
                            ChooseHowManyDegreesOfFreedomRequiredToAllowAllCombinationsToBeCovered(
                                atomicTestCasesCombinations, randomChoice,
                                testCaseGeneratorWithoutCollisions);

                        {
                            var testCaseEnumerator =
                                testCaseGeneratorWithoutCollisions.CreateIterator(numberOfDegreesOfFreedom);

                            CheckThatAllCombinationsAreCovered(testCaseEnumerator,
                                                               atomicStateToContainingCombinationMultiMap,
                                                               atomicTestCasesCombinations);
                        }

                        {
                            var testCaseEnumerator =
                                testCaseGeneratorWithCollisions.CreateIterator(numberOfDegreesOfFreedom);

                            CheckThatAllCombinationsAreCovered(testCaseEnumerator,
                                                               atomicStateToContainingCombinationMultiMap,
                                                               atomicTestCasesCombinations);
                        }
                    });
        }

        [Test]
        public void TestMaximumDegreesOfFreedom()
        {
            ForABunchOfTestCaseGenerators(
                (testCaseGeneratorWithoutCollisions, testCaseGeneratorWithCollisions) =>
                Assert.AreEqual(testCaseGeneratorWithoutCollisions.MaximumDegreesOfFreedom,
                                ((ITestCaseGeneratorIntrusiveTestHooks) testCaseGeneratorWithoutCollisions).
                                    MaximumNumberOfOwningSetsInSequence));
        }

        [Test]
        public void TestOptimalityOfCoverageOfAllNWayCombinationsOfAtomicTestCasesOverAllOutputTestCases()
        {
            var randomChoice = new Random(0);

            ForABunchOfTestCaseGenerators(
                (testCaseGeneratorWithoutCollisions, testCaseGeneratorWithCollisions) =>
                    {
                        var maximumDegreesOfFreedom =
                            (UInt32)
                            randomChoice.Next((Int32) testCaseGeneratorWithoutCollisions.MaximumDegreesOfFreedom) + 1U;

                        var testCases = new HashSet<AbstractTestCase>();

                        var testCaseEnumerator =
                            testCaseGeneratorWithoutCollisions.CreateIterator(maximumDegreesOfFreedom);

                        while (testCaseEnumerator.MoveNext())
                        {
                            testCases.Add((AbstractTestCase) testCaseEnumerator.Current);
                        }

                        var testCaseToBeExcluded = Algorithms.RandomSubset(testCases, 1, randomChoice)[0];

                        var atomicTestCasesCombinations =
                            PickTheLargestCombinationsOfSizeNotExceeding(testCaseToBeExcluded.AtomicTestCases(),
                                                                         maximumDegreesOfFreedom == 0U
                                                                             ? testCaseGeneratorWithoutCollisions
                                                                                   .MaximumDegreesOfFreedom
                                                                             : maximumDegreesOfFreedom);

                        var atomicStateToContainingCombinationMultiMap =
                            MakeAtomicStateToContainingCombinationMultiMap(atomicTestCasesCombinations);

                        {
                            var testCaseEnumeratorTwo =
                                testCaseGeneratorWithoutCollisions.CreateIterator(maximumDegreesOfFreedom);

                            CheckThatAtLeastOneCombinationIsNotCovered(testCaseToBeExcluded, testCaseEnumeratorTwo,
                                                                       atomicStateToContainingCombinationMultiMap,
                                                                       atomicTestCasesCombinations);
                        }

                        {
                            var testCaseEnumeratorTwo =
                                testCaseGeneratorWithCollisions.CreateIterator(maximumDegreesOfFreedom);

                            CheckThatAtLeastOneCombinationIsNotCovered(testCaseToBeExcluded, testCaseEnumeratorTwo,
                                                                       atomicStateToContainingCombinationMultiMap,
                                                                       atomicTestCasesCombinations);
                        }
                    });
        }
    }
}