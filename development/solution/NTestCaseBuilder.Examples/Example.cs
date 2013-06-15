using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using NTestCaseBuilder;
using SageSerpent.Infrastructure;
using Key = System.Int32;
using Value = System.String;
using Operation =
    System.Action<System.Collections.Generic.IDictionary<System.Int32, System.String>>;


namespace NTestCaseBuilder.Examples
{
    ///<summary>
    ///</summary>
    [TestFixture]
    public class SimpleExample
    {
        private static readonly Key[] LevelsOne = {0, 56, 789, 789}, LevelsTwo = {100, 123};

        ///<summary>
        ///</summary>
        [Test]
        public void TakeEnumeratorOutForTestDrive()
        {
            var a = TestVariableLevelEnumerableFactory.Create(LevelsOne);
            var b = TestVariableLevelEnumerableFactory.Create(LevelsTwo);

            var c = SynthesizedTestCaseEnumerableFactory.Create(a, b,
                                                                ((resultOne,
                                                                  resultTwo) =>
                                                                 string.Format(
                                                                     "{0}, {1}",
                                                                     resultOne.
                                                                         ToString
                                                                         (),
                                                                     resultTwo.
                                                                         ToString
                                                                         ())));

            var d =
                SynthesizedTestCaseEnumerableFactory.Create(
                    TestVariableLevelEnumerableFactory.Create(LevelsOne), input => input.ToString());

            var dWithATwist = SynthesizedTestCaseEnumerableFactory.Create(
                new List<TestCaseEnumerableFactory> {d}, (Converter<Object, Object>) (thing => thing));

            var e = InterleavedTestCaseEnumerableFactory.Create(new List<TestCaseEnumerableFactory> {dWithATwist, c});

            var strength = e.MaximumStrength;

            do
            {
                foreach (var u in e.CreateEnumerable(strength))
                {
                    Console.WriteLine(u.ToString());
                }

                Console.WriteLine("****************");
            } while (--strength != 0U);
        }
    }

    ///<summary>
    ///</summary>
    [TestFixture]
    public class ComplexExample
    {
        private enum OperationKind
        {
            DoNothing,
            Insertion,
            Deletion,
            Replacement,
            Query
        }

        private class OperationListBuilder
        {
            private const Int32 MaximumValueRepresentation = 20;
            private readonly Key _key;

            private readonly IDictionary<OperationKind, OperationCreator>
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists =
                    new Dictionary<OperationKind, OperationCreator>(),
                _operationKindToOperationCreatorMapWhereNoEntryExists = new Dictionary<OperationKind, OperationCreator>
                    ();

            private readonly Random _randomBehaviour;
            private Value _value;

            public OperationListBuilder
                (Key key,
                 Random randomBehaviourInitialState)
            {
                Operations = new List<Operation>();
                _key = key;
                _randomBehaviour = new Random(randomBehaviourInitialState.Next());

                AddStateTransitionsForWhenAnEntryAlreadyExists();

                AddStateTransitionsForWhenNoEntryExists();
            }

            public IList<Operation> Operations { get; private set; }

            private void AddStateTransitionsForWhenNoEntryExists()
            {
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add
                    (OperationKind.DoNothing, AddDoNothingOperation);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add
                    (OperationKind.Insertion, AddInsertionOperationThatShouldSucceed);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add
                    (OperationKind.Deletion, AddDeletionOperationThatShouldFail);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add
                    (OperationKind.Replacement, AddReplacementOperation);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add
                    (OperationKind.Query, AddQueryOperationThatShouldFail);
            }

            private void AddStateTransitionsForWhenAnEntryAlreadyExists()
            {
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add
                    (OperationKind.DoNothing, AddDoNothingOperation);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add
                    (OperationKind.Insertion, AddInsertionOperationThatShouldFail);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add
                    (OperationKind.Deletion, AddDeletionOperationThatShouldSucceed);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add
                    (OperationKind.Replacement, AddReplacementOperation);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add
                    (OperationKind.Query, AddQueryOperationThatShouldSucceed);
            }

            public void AppendNewOperationOfKind(OperationKind operationKind)
            {
                if (null != _value)
                {
                    _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists[operationKind]();
                }
                else
                {
                    _operationKindToOperationCreatorMapWhereNoEntryExists[operationKind]();
                }
            }

            private void AddDoNothingOperation()
            {
                Operations.Add(obj => { });
            }

            private void AddQueryOperationThatShouldFail()
            {
                Operations.Add(indexedSortedDictionary => Assert.IsFalse(indexedSortedDictionary.ContainsKey(_key)));
            }

            private void AddQueryOperationThatShouldSucceed()
            {
                Operations.Add
                    (indexedSortedDictionary =>
                         {
                             Assert.IsTrue(indexedSortedDictionary.ContainsKey(_key));
                             Assert.IsTrue(indexedSortedDictionary[_key] == _value);
                         });
            }

            private void AddDeletionOperationThatShouldFail()
            {
                Operations.Add(indexedSortedDictionary => Assert.IsFalse(indexedSortedDictionary.Remove(_key)));
            }

            private void AddDeletionOperationThatShouldSucceed()
            {
                Operations.Add(indexedSortedDictionary => Assert.IsTrue(indexedSortedDictionary.Remove(_key)));
                _value = null;
            }

            private void AddInsertionOperationThatShouldSucceed()
            {
                _value = MakeRandomValue();
                Operations.Add(indexedSortedDictionary => indexedSortedDictionary.Add(_key, _value));
            }

            private void AddInsertionOperationThatShouldFail()
            {
                Operations.Add
                    (indexedSortedDictionary =>
                         {
                             try
                             {
                                 indexedSortedDictionary.Add(_key, MakeRandomValue());
                             }
                             catch (ArgumentException)
                             {
                                 return;
                             }

                             var stringBuilder = new StringBuilder();

                             stringBuilder.AppendFormat(
                                 "Should not have been able to insert with key {0} as it already has an entry in the dictionary {1} of {2}",
                                 _key, indexedSortedDictionary, _value);

                             Assert.Fail(stringBuilder.ToString());
                         });
            }

            private void AddReplacementOperation()
            {
                _value = MakeRandomValue();
                Operations.Add(indexedSortedDictionary => indexedSortedDictionary[_key] = _value);
            }

            private Value MakeRandomValue()
            {
                return _randomBehaviour.ChooseAnyNumberFromOneTo(MaximumValueRepresentation).ToString();
            }

            #region Nested type: OperationCreator

            private delegate void OperationCreator();

            #endregion
        }

        private static IList<Operation> CreateOperationsAccordingToSequenceOfKinds
            (IEnumerable<OperationKind> kinds,
             Key key,
             Random randomBehaviour)
        {
            var operationsListBuilder = new OperationListBuilder(key, randomBehaviour);

            foreach (var operationKind in kinds)
            {
                operationsListBuilder.AppendNewOperationOfKind(operationKind);
            }

            return operationsListBuilder.Operations;
        }

        private delegate Int32 MappingToAvoidPreviouslyChosenIndices(Int32 index);

        private delegate void PlacementOfOperationsIntoFinalOrder(Int32 numberOfIndicesToChooseFrom,
                                                                  MappingToAvoidPreviouslyChosenIndices
                                                                      mappingToAvoidPreviouslyChosenIndices,
                                                                  IList<Operation> operationsPlacedIntoFinalOrder);

        private static void BaseCaseForPlacementOfOperationsIntoFinalOrder
            (Int32 numberOfIndicesToChooseFrom,
             MappingToAvoidPreviouslyChosenIndices mappingToAvoidPreviouslyChosenIndices,
             IList<Operation> operationsPlacedIntoFinalOrder)
        {
        }

        private static PlacementOfOperationsIntoFinalOrder BuildInductiveCaseForPlacementOfOperationsIntoFinalOrder
            (IList<Operation> operationsPertainingToTheSameKeyToPlaceIntoFinalOrder,
             Int32 combinationSelector,
             PlacementOfOperationsIntoFinalOrder placementOfOperationsForRemainingKeysIntoFinalOrder)
        {
            return
                (numberOfIndicesToChooseFrom, mappingToAvoidPreviouslyChosenIndices, operationsPlacedIntoFinalOrder) =>
                    {
                        var numberOfIndicesToChoose1 =
                            operationsPertainingToTheSameKeyToPlaceIntoFinalOrder
                                .
                                Count;
                        if (numberOfIndicesToChooseFrom <
                            numberOfIndicesToChoose1)
                        {
                            throw new LogicErrorException
                                ("Test has an internal logic error - should have eventually reached base case"
                                 +
                                 " where all indices for placement had been chosen without having to look for more");
                        }

                        var indicesToPlaceAt1 = ChooseIndicesToPlaceAt
                            (numberOfIndicesToChooseFrom,
                             numberOfIndicesToChoose1,
                             combinationSelector);

                        PlaceOperations
                            (operationsPertainingToTheSameKeyToPlaceIntoFinalOrder,
                             indicesToPlaceAt1,
                             mappingToAvoidPreviouslyChosenIndices,
                             operationsPlacedIntoFinalOrder);

                        var composedMappingToAvoidAllIndices1 =
                            ComposeMappingToAvoidAllIndices
                                (indicesToPlaceAt1,
                                 mappingToAvoidPreviouslyChosenIndices);

                        if (numberOfIndicesToChooseFrom >
                            numberOfIndicesToChoose1)
                        {
                            placementOfOperationsForRemainingKeysIntoFinalOrder
                                (numberOfIndicesToChooseFrom
                                 - numberOfIndicesToChoose1,
                                 composedMappingToAvoidAllIndices1,
                                 operationsPlacedIntoFinalOrder);
                        }
                    };
        }

        private static MappingToAvoidPreviouslyChosenIndices ComposeMappingToAvoidAllIndices
            (IEnumerable<Int32> indicesToPlaceAt,
             MappingToAvoidPreviouslyChosenIndices mappingToAvoidPreviouslyChosenIndices)
        {
            var mappingAvoidingJustTheIndicesChosenInThisCall = BargainBasement.MappingAvoidingIndices(indicesToPlaceAt);

            return
                index =>
                mappingToAvoidPreviouslyChosenIndices(mappingAvoidingJustTheIndicesChosenInThisCall.Invoke(index));
        }

        private static void PlaceOperations
            (IList<Operation> operationsPertainingToTheSameKeyToPlaceIntoFinalOrder,
             IEnumerable<Int32> indicesToPlaceAt,
             MappingToAvoidPreviouslyChosenIndices mappingToAvoidPreviouslyChosenIndices,
             IList<Operation> operationsPlacedIntoFinalOrder)
        {
            var placementIndices =
                indicesToPlaceAt.Select(index => mappingToAvoidPreviouslyChosenIndices(index)).ToArray();

            for (var operationIndex = 0;
                 operationIndex < operationsPertainingToTheSameKeyToPlaceIntoFinalOrder.Count;
                 ++operationIndex)
            {
                operationsPlacedIntoFinalOrder[placementIndices[operationIndex]] =
                    operationsPertainingToTheSameKeyToPlaceIntoFinalOrder[operationIndex];
            }
        }

        private static IEnumerable<Int32> ChooseIndicesToPlaceAt
            (Int32 numberOfIndicesToChooseFrom,
             Int32 numberOfIndicesToChoose,
             Int32 combinationSelector)
        {
            var indicesToChooseFrom = new Int32[numberOfIndicesToChooseFrom];

            for (var index = 0; index < numberOfIndicesToChooseFrom; ++index)
            {
                indicesToChooseFrom[index] = index;
            }

            return CombinatoricUtilities.GenerateCombinationOfGivenSizePreservingOrder
                (numberOfIndicesToChoose, indicesToChooseFrom, combinationSelector);
        }

        private static readonly C5.IList<Key> Keys = new C5.ArrayList<Int32> {0, 1};

        private static readonly IEnumerable<OperationKind> OperationKinds =
            ((IList<Int32>) Enum.GetValues(typeof (OperationKind))).Select(constant => (OperationKind) constant);


        private static TypedTestCaseEnumerableFactory<PlacementOfOperationsIntoFinalOrder> MakeTestCaseEnumerableFactory
            (Random randomBehaviour,
             Int32 sequenceLength,
             C5.IList<Key> keys)
        {
            if (0 == keys.Count)
            {
                return MakeRecursionBaseFactory();
            }

            var key = keys[0];

            var synthesizingFactoryForOperationSequenceEnumerable =
                MakeSynthesizingFactoryForOperationSequenceEnumerable(key, randomBehaviour);

            var numberOfCombinations = BargainBasement.NumberOfCombinations
                (sequenceLength * keys.Count, sequenceLength);

            var testVariableLevelFactoryForIndexCombinationEnumerable =
                MakeTestVariableLevelFactoryForIndexCombinationEnumerable(numberOfCombinations);

            var factoryDealingWithRemainingKeys = MakeTestCaseEnumerableFactory
                (randomBehaviour, sequenceLength, keys.View(1, keys.Count - 1));

            return MakeRecursionInductiveCaseFactory
                (synthesizingFactoryForOperationSequenceEnumerable,
                 testVariableLevelFactoryForIndexCombinationEnumerable,
                 factoryDealingWithRemainingKeys);
        }

        private static TypedTestCaseEnumerableFactory<Int32> MakeTestVariableLevelFactoryForIndexCombinationEnumerable
            (Int32 numberOfCombinations)
        {
            var combinationSelector = numberOfCombinations;

            var combinationSelectors = new List<Int32>();

            while (0U != combinationSelector--)
            {
                combinationSelectors.Add(combinationSelector);
            }
            return TestVariableLevelEnumerableFactory.Create(combinationSelectors);
        }

        private static TypedTestCaseEnumerableFactory<PlacementOfOperationsIntoFinalOrder>
            MakeRecursionInductiveCaseFactory
            (TypedTestCaseEnumerableFactory<IList<Operation>> synthesizingFactoryForOperationSequence,
             TypedTestCaseEnumerableFactory<Int32> testVariableLevelFactoryForFinalOperationsListIndexCombinations,
             TypedTestCaseEnumerableFactory<PlacementOfOperationsIntoFinalOrder> factoryDealingWithRemainingKeys)
        {
            return
                SynthesizedTestCaseEnumerableFactory.Create(synthesizingFactoryForOperationSequence,
                     testVariableLevelFactoryForFinalOperationsListIndexCombinations,
                     factoryDealingWithRemainingKeys,
                     BuildInductiveCaseForPlacementOfOperationsIntoFinalOrder);
        }

        private static TypedTestCaseEnumerableFactory<PlacementOfOperationsIntoFinalOrder> MakeRecursionBaseFactory()
        {
            return SingletonTestCaseEnumerableFactory.Create
                ((PlacementOfOperationsIntoFinalOrder) BaseCaseForPlacementOfOperationsIntoFinalOrder);
        }

        private static TypedTestCaseEnumerableFactory<IList<Operation>>
            MakeSynthesizingFactoryForOperationSequenceEnumerable
            (Key key,
             Random randomBehaviour)
        {
            return SynthesizedTestCaseEnumerableFactory.Create(
                TestVariableLevelEnumerableFactory.Create(OperationKinds),
                TestVariableLevelEnumerableFactory.Create(OperationKinds),
                TestVariableLevelEnumerableFactory.Create(OperationKinds),
                TestVariableLevelEnumerableFactory.Create(OperationKinds),
                TestVariableLevelEnumerableFactory.Create(OperationKinds),
                ((operationKindOne,
                  operationKindTwo,
                  operationKindThree,
                  operationKindFour,
                  operationKindFive) =>
                 CreateOperationsAccordingToSequenceOfKinds
                     (new[]
                          {
                              operationKindOne, operationKindTwo, operationKindThree,
                              operationKindFour, operationKindFive
                          },
                      key,
                      randomBehaviour)));
        }

        private const Int32 SequenceLength = 5;

        private const Int32 CombinationStrength = 3;

        ///<summary>
        ///</summary>
        [Test]
        public void ManipulateIndexedSortedDictionary()
        {
            var numberOfKeys = Keys.Count;
            var randomBehaviour = new Random(892893767);

            var totalNumberOfOperations = numberOfKeys * SequenceLength;

            var testCasesEnumerableFactory = MakeTestCaseEnumerableFactory(randomBehaviour, SequenceLength, Keys);

            var operations = new Operation[totalNumberOfOperations];

            var numberOfTestCases = 0ul;

            testCasesEnumerableFactory.ExecuteParameterisedUnitTestForAllTypedTestCases(CombinationStrength,
                                                                                        testCase
                                                                                        =>
                                                                                        ExerciseTestCase(
                                                                                            testCase,
                                                                                            totalNumberOfOperations,
                                                                                            operations,
                                                                                            ref numberOfTestCases));

            System.Diagnostics.Debug.Print("Number of test cases: {0}.\n", numberOfTestCases);
        }

        ///<summary>
        ///</summary>
        [Test]
        public void ReproduceFailureQuickly()
        {
            var numberOfKeys = Keys.Count;
            var randomBehaviour = new Random(892893767);

            var totalNumberOfOperations = numberOfKeys * SequenceLength;

            var testCasesEnumerableFactory = MakeTestCaseEnumerableFactory(randomBehaviour, SequenceLength, Keys);

            var operations = new Operation[totalNumberOfOperations];

            var numberOfTestCases = 0ul;

            const string reproductionString =
                "1090764779116129690923515858308014520222336185700694896976936400046940578111983112055989629000774433035533486068550533022050434118313487168281935739867417093859214666702622185680854690920792366308346801502341102104791669567068489846501138965598186388213004543697669795062454808628218854975401499294582299660019848783083937284842433875698992003869361173950368049939283422201283431500534314646405123093902572611305574975273253618404214436035332972904429107025527812924016143091522120111065405814720119831860702440964097642207233000861251447303137353773334074755528281991379172351571133523536155122143514512522383976701375986937228871803364271167562885534758744817339903091811910512739823187208506998747503060178100613962060360444772404024313739493297927858396240721030140492470103357047936834346132464036182437087170078501000684109986471279861917999591896108862562390012971951057050012075432713193683803876796197084280258336654918894800363034021269058378795720392390578650187065164540156994402244927110261562540259941947112035193302179458312579762656161886005839413204573285998582929202183055517423806152053711536672660710286311473872993764902751137075153515040308513410335851141018398956648431259553343117355097715103380069001848844134945111727805289923909566198141698885942046212828772382281233607388430521341291288749480420005289750948127596302248886450488346351194724726406359454567530950204893548438810296165819635671013870819998039900637698919336112584536603278238351651506699334362357470012971977314027054319976080930112017145037780575224655789108248709042322056855343967628440283369435484324673083658025858328083400735766763942389746704985223746546997365576000593138187854360290689007537973251142762840999432884894358083285521586072459421877752684016784827019217046294951759187261946302369751970154054761479564538898022435827825534772610734542189908947402889635366737805737144972576893718528050290391267672480275214334163703582188746586995496270938203644679193688129434102383381736534990452158190702968883951610080513324923606579060719935390474279719336648791089582686686083958958040855218963172888629694739957177302287360225125498508350037906763485416542524021814083097114549316290605257317417730876321368941382473034150289234129964670435403742355755299046922759543343132785257922101460982138759307022490864160851181451734277030111231600949027333911546177597073424304399689149654353831050380247495002720234833866803715973080927654375613256989752730830805925888";

            testCasesEnumerableFactory.ExecuteParameterisedUnitTestForReproducedTypedTestCase(testCase
                                                                                              =>
                                                                                              ExerciseTestCase(
                                                                                                  testCase,
                                                                                                  totalNumberOfOperations,
                                                                                                  operations,
                                                                                                  ref numberOfTestCases),
                                                                                              reproductionString);
        }

        private static void ExerciseTestCase(PlacementOfOperationsIntoFinalOrder testCase,
                                             Int32 totalNumberOfOperations, IList<Operation> operations,
                                             ref UInt64 numberOfTestCases)
        {
            testCase
                (totalNumberOfOperations,
                 index => index,
                 operations);

            var
                indexedSortedDictionary
                    =
                    new IndexedSortedDictionary
                        <Int32,
                            Value>();

            ++numberOfTestCases;

            foreach (
                var operation in
                    operations)
            {
                operation(indexedSortedDictionary);
                Assert.IsTrue(BargainBasement.IsSorted(indexedSortedDictionary.Keys));
                Assert.IsTrue(BargainBasement.IsSorted(from index in Enumerable.Range(0, indexedSortedDictionary.Count) select indexedSortedDictionary[index]));
            }
        }
    }
}