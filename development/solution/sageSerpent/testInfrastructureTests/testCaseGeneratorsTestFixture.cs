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

        public class TestCase
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

        public interface ITracksHowOwningSetsContributeToOutput
        {
            Wintellect.PowerCollections.Set<System.Collections.Generic.IEnumerable<Wintellect.PowerCollections.Set<TestCase>>>
                PossibleSequencesOfOwningSets();
        }

        public class TestCaseFromCollectionGenerator :
            SageSerpent.TestInfrastructure.TestCaseFromCollectionGenerator,
            ITracksHowOwningSetsContributeToOutput
        {
            private TestCaseFromCollectionGenerator(Wintellect.PowerCollections.Set<TestCase> owningSet) :
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

        public class TestCaseFromAlternativesGenerator :
            SageSerpent.TestInfrastructure.TestCaseFromAlternativesGenerator,
            ITracksHowOwningSetsContributeToOutput
        {
            private TestCaseFromAlternativesGenerator(Wintellect.PowerCollections.Set<SageSerpent.TestInfrastructure.ITestCaseGenerator> testCaseGenerators) :
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

        public class TestCaseFromCombinationGenerator :
            SageSerpent.TestInfrastructure.TestCaseFromCombinationGenerator,
            ITracksHowOwningSetsContributeToOutput
        {
            public delegate TestCase PermutingClosure(System.Collections.Generic.IEnumerable<TestCase> input);

            private TestCaseFromCombinationGenerator(System.Collections.Generic.IList<SageSerpent.TestInfrastructure.ITestCaseGenerator> testCaseGenerators,
                                                     PermutingClosure combiningClosure) :
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

            public class PermutingClosureAdapterSupport
            {
                public PermutingClosure PermutingClosure
                {
                    set
                    {
                        _permutingClosure = value;
                    }
                }

                private PermutingClosure _permutingClosure;
            }

            private static System.Delegate AdaptPermutingClosure(PermutingClosure permutingClosure, System.UInt32 numberOfArgumentsForAdaptedClosure)
            {
                Wintellect.PowerCollections.Pair<System.Type, System.Type> permutingClosureAdapterTypeAndDelegateTypePair;

                if (!_arityToPermutingClosureAdapterTypeAndDelegateTypePairMap.TryGetValue(numberOfArgumentsForAdaptedClosure, out permutingClosureAdapterTypeAndDelegateTypePair))
                {
                    permutingClosureAdapterTypeAndDelegateTypePair.First = CreatePermutingClosureAdapterType(numberOfArgumentsForAdaptedClosure);
                    permutingClosureAdapterTypeAndDelegateTypePair.Second = CreatePermutingClosureDelegateType(numberOfArgumentsForAdaptedClosure);

                    _arityToPermutingClosureAdapterTypeAndDelegateTypePairMap.Add(numberOfArgumentsForAdaptedClosure, permutingClosureAdapterTypeAndDelegateTypePair);
                }

                PermutingClosureAdapterSupport adapter = (PermutingClosureAdapterSupport)permutingClosureAdapterTypeAndDelegateTypePair.First.GetConstructor(System.Type.EmptyTypes).Invoke(null);

                adapter.PermutingClosure = permutingClosure;

                return System.Delegate.CreateDelegate(permutingClosureAdapterTypeAndDelegateTypePair.Second, adapter, permutingClosureAdapterTypeAndDelegateTypePair.First.GetMethod(_generatedMethodName));
            }

            private static System.Type CreatePermutingClosureAdapterType(System.UInt32 numberOfArgumentsForAdaptedClosure)
            {
                System.String arityAsString = numberOfArgumentsForAdaptedClosure.ToString();

                System.Reflection.Emit.TypeBuilder typeBuilder = _moduleBuilder.DefineType("_PermutingClosureAdapterFor" + arityAsString + "Arguments_" + CreateUniqueIdentifier());

                typeBuilder.SetParent(typeof(PermutingClosureAdapterSupport));

                System.Type[] argumentTypes = CreateArgumentTypesForAdaptedClosure(numberOfArgumentsForAdaptedClosure);

                System.Reflection.MethodAttributes methodAttributes = System.Reflection.MethodAttributes.Public;

                System.Reflection.Emit.MethodBuilder methodBuilder = typeBuilder.DefineMethod(_generatedMethodName,
                                                                                              methodAttributes,
                                                                                              typeof(TestCase),
                                                                                              argumentTypes);

                System.Reflection.Emit.ILGenerator codeGenerator = methodBuilder.GetILGenerator();

                System.Type collectionType = typeof(System.Collections.Generic.List<TestCase>);

                codeGenerator.Emit(System.Reflection.Emit.OpCodes.Newobj, collectionType.GetConstructor(System.Type.EmptyTypes));
                codeGenerator.Emit(System.Reflection.Emit.OpCodes.Stloc_0);

                for (System.Byte argumentIndex = 0; argumentIndex < numberOfArgumentsForAdaptedClosure; ++argumentIndex)
                {
                    codeGenerator.Emit(System.Reflection.Emit.OpCodes.Ldloc_0);
                    codeGenerator.Emit(System.Reflection.Emit.OpCodes.Ldarg_S, (System.Byte)(argumentIndex + 1U));
                    codeGenerator.Emit(System.Reflection.Emit.OpCodes.Callvirt, collectionType.GetMethod("Add"));
                }

                codeGenerator.Emit(System.Reflection.Emit.OpCodes.Ldarg_0);
                codeGenerator.Emit(System.Reflection.Emit.OpCodes.Ldfld, typeof(PermutingClosureAdapterSupport).GetField("_permutingClosure", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance));
                codeGenerator.Emit(System.Reflection.Emit.OpCodes.Ldloc_0);
                codeGenerator.Emit(System.Reflection.Emit.OpCodes.Callvirt, typeof(PermutingClosure).GetMethod("Invoke"));
                codeGenerator.Emit(System.Reflection.Emit.OpCodes.Ret);

                System.Type permutingClosureAdapterType = typeBuilder.CreateType();

                return permutingClosureAdapterType;
            }

            private static System.Type CreatePermutingClosureDelegateType(System.UInt32 numberOfArgumentsForAdaptedClosure)
            {
                System.String arityAsString = numberOfArgumentsForAdaptedClosure.ToString();

                System.Reflection.TypeAttributes typeAttributes = System.Reflection.TypeAttributes.Class
                                                                  | System.Reflection.TypeAttributes.Public
                                                                  | System.Reflection.TypeAttributes.Sealed
                                                                  | System.Reflection.TypeAttributes.AnsiClass
                                                                  | System.Reflection.TypeAttributes.AutoClass;

                System.Reflection.Emit.TypeBuilder typeBuilder = _moduleBuilder.DefineType("_PermutingClosureDelegateFor" + arityAsString + "Arguments_" + CreateUniqueIdentifier(),
                                                                                           typeAttributes);

                typeBuilder.SetParent(typeof(System.MulticastDelegate));

                System.Type[] constructorArgumentTypes = new System.Type[] { typeof(System.Object), typeof(System.IntPtr) };

                System.Reflection.MethodAttributes constructorAttributes = System.Reflection.MethodAttributes.RTSpecialName
                                                                           | System.Reflection.MethodAttributes.HideBySig
                                                                           | System.Reflection.MethodAttributes.Public;

                System.Reflection.Emit.ConstructorBuilder constructorBuilder
                    = typeBuilder.DefineConstructor(constructorAttributes,
                                                    System.Reflection.CallingConventions.Standard,
                                                    new System.Type[] { typeof(object), typeof(System.IntPtr) });

                constructorBuilder.SetImplementationFlags(System.Reflection.MethodImplAttributes.Runtime
                                                          | System.Reflection.MethodImplAttributes.Managed);



                System.Type[] methodArgumentTypes = CreateArgumentTypesForAdaptedClosure(numberOfArgumentsForAdaptedClosure);

                System.Reflection.MethodAttributes methodAttributes = System.Reflection.MethodAttributes.Public
                                                                      | System.Reflection.MethodAttributes.HideBySig
                                                                      | System.Reflection.MethodAttributes.NewSlot
                                                                      | System.Reflection.MethodAttributes.Virtual;

                System.String invokeMethodName = "Invoke";

                System.Reflection.Emit.MethodBuilder methodBuilder = typeBuilder.DefineMethod(invokeMethodName,
                                                                                              methodAttributes,
                                                                                              typeof(TestCase),
                                                                                              methodArgumentTypes);

                methodBuilder.SetImplementationFlags(System.Reflection.MethodImplAttributes.Runtime
                                                     | System.Reflection.MethodImplAttributes.Managed);

                System.Type permutingClosureAdapterType = typeBuilder.CreateType();

                return permutingClosureAdapterType;
            }

            private static System.Type[] CreateArgumentTypesForAdaptedClosure(System.UInt32 numberOfArgumentsForAdaptedClosure)
            {
                System.Type[] result = new System.Type[numberOfArgumentsForAdaptedClosure];

                Wintellect.PowerCollections.Algorithms.Fill(result, typeof(TestCase));

                return result;
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

            private static System.UInt32 _namespaceNameGenerationState = 0U;

            private static System.String _generatedMethodName = "Invoke";

            private static System.Reflection.Emit.AssemblyBuilder _assemblyBuilder;

            private static System.Reflection.Emit.ModuleBuilder _moduleBuilder;

            static TestCaseFromCombinationGenerator()
            {
                System.String assemblyName = GetNonConflictingAssemblyName();

                System.Reflection.AssemblyName assemblyNameThingie = new System.Reflection.AssemblyName();

                assemblyNameThingie.Name = assemblyName;

                _assemblyBuilder = System.AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyNameThingie, System.Reflection.Emit.AssemblyBuilderAccess.Run);

                const System.String moduleName = "theBigLebowski";

                _moduleBuilder = _assemblyBuilder.DefineDynamicModule(moduleName);
            }

            private static System.String GetNonConflictingAssemblyName()
            {
                System.Reflection.Assembly[] extantAssemblies = System.AppDomain.CurrentDomain.GetAssemblies();

                do
                {
                    System.String assemblyName = CreateUniqueIdentifier();

                    if (Wintellect.PowerCollections.Algorithms.FindFirstWhere(extantAssemblies,
                                                                              delegate(System.Reflection.Assembly assembly)
                                                                              {
                                                                                  return assembly.GetName().Name == assemblyName;
                                                                              }) == null)
                    {
                        return assemblyName;
                    }
                } while (true);
            }

            private static System.Collections.Generic.IDictionary<System.UInt32, Wintellect.PowerCollections.Pair<System.Type, System.Type>> _arityToPermutingClosureAdapterTypeAndDelegateTypePairMap
                = new System.Collections.Generic.Dictionary<System.UInt32, Wintellect.PowerCollections.Pair<System.Type, System.Type>>();
        }

        public class TestCaseGeneratorFactory
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
