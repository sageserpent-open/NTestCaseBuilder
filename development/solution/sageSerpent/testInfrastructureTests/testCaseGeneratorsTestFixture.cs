using NUnit.Framework;
using SageSerpent.Infrastructure;

namespace SageSerpent.TestInfrastructureTests
{
    [TestFixture]
    public class TestCaseGeneratorsTestFixture
    {
        [Test]
        public void TestCorrectOrderingOfComponentTestCasesWithinCombinedTestCase()
        {
            System.UInt32 countDown = 201U;

            TestCaseGeneratorFactory factory = new TestCaseGeneratorFactory();

            while (countDown-- != 0U)
            {
                factory.CreateRandomlyAssembledTestCaseGenerator();
            }
        }

        // TODO: need to think about the impact of collisions due to transforms. Does this affect the validity of combinations being exhaustive?

        private class TestCase
        {
            private TestCase(Wintellect.PowerCollections.Set<TestCase> owningSet)
            {
                System.Collections.Generic.List<AtomicState> atomicStates = new System.Collections.Generic.List<AtomicState>();
                atomicStates.Add(new AtomicState(owningSet));
                _atomicStates = atomicStates;
            }

            public static void PutNewTestCaseInto(Wintellect.PowerCollections.Set<TestCase> owningSet)
            {
                TestCase testCase = new TestCase(owningSet);
                owningSet.Add(testCase);
            }

            private TestCase()
            {
            }

            public static TestCase MakeShuffledCombination(System.Collections.Generic.IEnumerable<TestCase> testCases,
                                                           System.Int32 seed)
            {
                TestCase result = new TestCase();

                System.Collections.Generic.IEnumerable<AtomicState> concatenatedSequence
                    = new System.Collections.Generic.List<AtomicState>();

                foreach (TestCase testCase in Wintellect.PowerCollections.Algorithms.RandomShuffle(testCases, new System.Random(seed)))
                {
                    concatenatedSequence = Wintellect.PowerCollections.Algorithms.Concatenate(concatenatedSequence, testCase._atomicStates);
                }

                result._atomicStates = concatenatedSequence;

                return result;
            }

            public System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>> SequenceOfOwningSets()
            {
                return Wintellect.PowerCollections.Algorithms.Convert(_atomicStates, delegate(AtomicState atomicState) { return atomicState.OwningSet; });
            }

            private class AtomicState
            {
                public AtomicState(Wintellect.PowerCollections.Set<TestCase> owningSet)
                {
                    _owningSet = owningSet;
                }

                public Wintellect.PowerCollections.Set<TestCase> OwningSet
                {
                    get
                    {
                        return _owningSet;
                    }
                }

                private Wintellect.PowerCollections.Set<TestCase> _owningSet; 
            }

            private System.Collections.Generic.IEnumerable<AtomicState> _atomicStates;
        }

        interface ITracksHowOwningSetsContributeToOutput
        {
            Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>>
                PossibleSequencesOfOwningSets();
        }

        private class TestCaseFromCollectionGenerator:
            SageSerpent.TestInfrastructure.TestCaseFromCollectionGenerator,
            ITracksHowOwningSetsContributeToOutput
        {
            private TestCaseFromCollectionGenerator(Wintellect.PowerCollections.Set<TestCase> owningSet):
                base(owningSet)
            {
                _owningSet = owningSet;
            }

            public static TestCaseFromCollectionGenerator Create(TestCaseGeneratorFactory factory)
            {
                System.Console.WriteLine('E');

                const System.UInt32 maximumNumberOfTestCasesInCollection = 10;

                Wintellect.PowerCollections.Set<TestCase> owningSet
                    = new Wintellect.PowerCollections.Set<TestCase>();

                System.UInt32 countDown = (System.UInt32)factory.RandomChoice.Next((System.Int32)maximumNumberOfTestCasesInCollection);

                while (countDown-- != 0U)
                {
                    TestCase.PutNewTestCaseInto(owningSet);
                }

                return new TestCaseFromCollectionGenerator(owningSet);
            }


            public Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>>
                PossibleSequencesOfOwningSets()
            {
                Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>> result
                    = new Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>>();

                System.Collections.Generic.ICollection<Wintellect.PowerCollections.Set<TestCase>> singletonSequence
                    = new System.Collections.Generic.List<Wintellect.PowerCollections.Set<TestCase>>();

                singletonSequence.Add(_owningSet);

                result.Add(singletonSequence);

                return result;
            }

            private Wintellect.PowerCollections.Set<TestCase> _owningSet
                = new Wintellect.PowerCollections.Set<TestCase>();
        }

        private class TestCaseFromAlternativesGenerator :
            SageSerpent.TestInfrastructure.TestCaseFromAlternativesGenerator,
            ITracksHowOwningSetsContributeToOutput
        {
            private TestCaseFromAlternativesGenerator(Wintellect.PowerCollections.Set<SageSerpent.TestInfrastructure.ITestCaseGenerator> testCaseGenerators):
                base(testCaseGenerators)
            {
                _testCaseGenerators = testCaseGenerators;
            }

            public static TestCaseFromAlternativesGenerator Create(TestCaseGeneratorFactory factory,
                                                                   System.UInt32 treeDepth)
            {
                System.Console.WriteLine('A');

                Wintellect.PowerCollections.Set<SageSerpent.TestInfrastructure.ITestCaseGenerator> testCaseGenerators
                    = new Wintellect.PowerCollections.Set<SageSerpent.TestInfrastructure.ITestCaseGenerator>();

                const System.Int32 maximumNumberOfAlternativeTestCaseGenerators = 5;

                System.UInt32 countDown = (System.UInt32)factory.RandomChoice.Next((System.Int32)maximumNumberOfAlternativeTestCaseGenerators);

                while (countDown-- != 0U)
                {
                    testCaseGenerators.Add(factory.CreateRandomlyAssembledTestCaseGenerator(treeDepth));
                }

                return new TestCaseFromAlternativesGenerator(testCaseGenerators);
            }

            public Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>>
                PossibleSequencesOfOwningSets()
            {
                Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>> result
                    = new Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>>();

                foreach (SageSerpent.TestInfrastructure.ITestCaseGenerator testCaseGenerator in _testCaseGenerators)
                {
                    result = result.Union(((ITracksHowOwningSetsContributeToOutput)testCaseGenerator).PossibleSequencesOfOwningSets());
                }

                return result;
            }

            private Wintellect.PowerCollections.Set<SageSerpent.TestInfrastructure.ITestCaseGenerator> _testCaseGenerators;
        }

        private class TestCaseFromCombinationGenerator :
            SageSerpent.TestInfrastructure.TestCaseFromCombinationGenerator,
            ITracksHowOwningSetsContributeToOutput
        {
            private delegate TestCase PermutingClosure(System.Collections.Generic.IEnumerable<TestCase> input);

            private TestCaseFromCombinationGenerator(System.Collections.Generic.IList<SageSerpent.TestInfrastructure.ITestCaseGenerator> testCaseGenerators,
                                                     PermutingClosure combiningClosure):
                base(testCaseGenerators, AdaptPermutingClosure(combiningClosure, (System.UInt32)testCaseGenerators.Count))
            {
                _testCaseGenerators = testCaseGenerators;
            }

            public static TestCaseFromCombinationGenerator Create(TestCaseGeneratorFactory factory,
                                                                  System.UInt32 treeDepth)
            {
                System.Console.WriteLine('C');

                System.Collections.Generic.IList<SageSerpent.TestInfrastructure.ITestCaseGenerator> testCaseGenerators
                    = new System.Collections.Generic.List<SageSerpent.TestInfrastructure.ITestCaseGenerator>();

                const System.Int32 maximumNumberOfCombinedTestCaseGenerators = 5;

                System.UInt32 countDown = (System.UInt32)factory.RandomChoice.Next((System.Int32)maximumNumberOfCombinedTestCaseGenerators);

                while (countDown-- != 0U)
                {
                    testCaseGenerators.Add(factory.CreateRandomlyAssembledTestCaseGenerator(treeDepth));
                }

                return new TestCaseFromCombinationGenerator(testCaseGenerators, CreatePermutingClosure(factory.RandomChoice));
            }

            private static PermutingClosure CreatePermutingClosure(System.Random randomChoice)
            {
                System.Int32 seed = randomChoice.Next();
                return delegate(System.Collections.Generic.IEnumerable<TestCase> input)
                    {
                        return TestCase.MakeShuffledCombination(input, seed);
                    };
            }

            private static System.String CreateUniqueIdentifier()
            {
                const System.UInt32 rangeOfAlphabet = 26U;

                System.UInt32 numberToBeInterpretedAccordingToABase = _namespaceNameGenerationState++;

                System.Text.StringBuilder builder = new System.Text.StringBuilder();

                do
                {
                    builder.Append('a' + numberToBeInterpretedAccordingToABase % rangeOfAlphabet);
                    numberToBeInterpretedAccordingToABase /= rangeOfAlphabet;
                } while (numberToBeInterpretedAccordingToABase != 0U);

                return builder.ToString();
            }

            struct Placeholder
            {
                public Placeholder(PermutingClosure permutingClosure)
                {
                    _permutingClosure = permutingClosure;
                }

                private TestCase Invoke(TestCase x)
                {
                    System.Collections.Generic.List<TestCase> argumentList = new System.Collections.Generic.List<TestCase>();

                    argumentList.Add(x);

                    return _permutingClosure(argumentList);
                }

                private delegate TestCase StronglyTypedDelegate(TestCase x);

                public System.Delegate CreateDelegate()
                {
                    return new StronglyTypedDelegate(Invoke);
                }

                private PermutingClosure _permutingClosure;
            }

            private static System.Delegate AdaptPermutingClosure(PermutingClosure permutingClosure, System.UInt32 numberOfArgumentsForAdaptedClosure)
            {
                return new Placeholder(permutingClosure).CreateDelegate();
            }

            public Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>>
                PossibleSequencesOfOwningSets()
            {
                Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>> result
                    = new Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>>();

                ConcatenateCrossProductOfSequences(_testCaseGenerators, new System.Collections.Generic.List<Wintellect.PowerCollections.Set<TestCase>>(), result);

                return result;
            }

            private 
                void ConcatenateCrossProductOfSequences(System.Collections.Generic.IList<SageSerpent.TestInfrastructure.ITestCaseGenerator> testCaseGenerators,
                                                        System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>> sequenceBeingBuiltUp,
                                                        Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>> result)
            {
                if (testCaseGenerators.Count == 0)
                {
                    result.Add(sequenceBeingBuiltUp);
                }
                else
                {
                    foreach (System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>> sequence in ((ITracksHowOwningSetsContributeToOutput)testCaseGenerators[0]).PossibleSequencesOfOwningSets())
                    {
                        ConcatenateCrossProductOfSequences(Wintellect.PowerCollections.Algorithms.Range(testCaseGenerators, 1, testCaseGenerators.Count - 1),
                                                           Wintellect.PowerCollections.Algorithms.Concatenate(sequenceBeingBuiltUp,
                                                                                                              sequence),
                                                           result);
                    }
                }
            }

            private System.Collections.Generic.IList<SageSerpent.TestInfrastructure.ITestCaseGenerator> _testCaseGenerators;

            static System.UInt32 _namespaceNameGenerationState = 0U;
        }

        class TestCaseGeneratorFactory
        {
            public System.Random RandomChoice
            {
                get
                {
                    return _randomChoice;
                }
            }

            public SageSerpent.TestInfrastructure.ITestCaseGenerator CreateRandomlyAssembledTestCaseGenerator()
            {
                System.Console.WriteLine();
                return CreateRandomlyAssembledTestCaseGenerator(0U);
            }

            public SageSerpent.TestInfrastructure.ITestCaseGenerator CreateRandomlyAssembledTestCaseGenerator(System.UInt32 treeDepth)
            {
                ++treeDepth;

                {
                    System.UInt32 countDown = treeDepth;

                    System.Console.Write(treeDepth);

                    while (--countDown != 0U)
                    {
                        System.Console.Write(' ');
                    }
                }

                if (treeDepth == 1U && !_doneTheRootOnlyTree)
                {
                    _doneTheRootOnlyTree = true;
                    return TestCaseFromCollectionGenerator.Create(this);
                }
                else if (_randomChoice.Next((System.Int32)treeDepth) != 0)
                {
                    return TestCaseFromCollectionGenerator.Create(this);
                }
                else
                {
                    switch (_randomChoice.Next(2))
                    {
                        case 0:
                            return TestCaseFromAlternativesGenerator.Create(this, treeDepth);

                        case 1:
                            return TestCaseFromCombinationGenerator.Create(this, treeDepth);

                        default:
                            throw new InternalAssertionViolation("Default of this switch should not be executed.");
                    }
                }
            }

            private System.Random _randomChoice = new System.Random(0);

            private bool _doneTheRootOnlyTree = false;
        }

        [SetUp]
        public void Reinitialise()
        {
        }

    }
}
