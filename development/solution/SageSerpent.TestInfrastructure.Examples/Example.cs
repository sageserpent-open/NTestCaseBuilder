using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using SageSerpent.Infrastructure;
using Key = System.UInt32;
using Value = System.String;
using Operation =
    System.Action<SageSerpent.TestInfrastructure.Examples.IndexedSortedDictionary<System.UInt32, System.String>>;


namespace SageSerpent.TestInfrastructure.Examples
{
    ///<summary>
    ///</summary>
    [TestFixture]
    public class SimpleExample
    {
        private static readonly Key[] LevelsOne = {0u, 56u, 789u, 789u}, LevelsTwo = {100u, 123u};

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
            private const UInt32 MaximumValueRepresentation = 20U;
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
                    (delegate(IndexedSortedDictionary<Key, Value> indexedSortedDictionary1)
                         {
                             Assert.IsTrue(indexedSortedDictionary1.ContainsKey(_key));
                             Assert.IsTrue(indexedSortedDictionary1[_key] == _value);
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
                    (delegate(IndexedSortedDictionary<Key, Value> indexedSortedDictionary)
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

                             stringBuilder.AppendFormat
                                 ("Should not have been able to insert with key {0} as it already has an entry in the dictionary {1} of {2}",
                                  _key,
                                  indexedSortedDictionary,
                                  _value);

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

        private delegate UInt32 MappingToAvoidPreviouslyChosenIndices(UInt32 index);

        private delegate void PlacementOfOperationsIntoFinalOrder(UInt32 numberOfIndicesToChooseFrom,
                                                                  MappingToAvoidPreviouslyChosenIndices
                                                                      mappingToAvoidPreviouslyChosenIndices,
                                                                  IList<Operation> operationsPlacedIntoFinalOrder);

        private static void BaseCaseForPlacementOfOperationsIntoFinalOrder
            (UInt32 numberOfIndicesToChooseFrom,
             MappingToAvoidPreviouslyChosenIndices mappingToAvoidPreviouslyChosenIndices,
             IList<Operation> operationsPlacedIntoFinalOrder)
        {
        }

        private static PlacementOfOperationsIntoFinalOrder BuildInductiveCaseForPlacementOfOperationsIntoFinalOrder
            (IList<Operation> operationsPertainingToTheSameKeyToPlaceIntoFinalOrder,
             UInt32 combinationSelector,
             PlacementOfOperationsIntoFinalOrder placementOfOperationsForRemainingKeysIntoFinalOrder)
        {
            return
                (numberOfIndicesToChooseFrom, mappingToAvoidPreviouslyChosenIndices, operationsPlacedIntoFinalOrder) =>
                    {
                        var numberOfIndicesToChoose1 =
                            (UInt32)
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
            (IEnumerable<UInt32> indicesToPlaceAt,
             MappingToAvoidPreviouslyChosenIndices mappingToAvoidPreviouslyChosenIndices)
        {
            var mappingAvoidingJustTheIndicesChosenInThisCall = BargainBasement.MappingAvoidingIndices(indicesToPlaceAt);

            return
                index =>
                mappingToAvoidPreviouslyChosenIndices(mappingAvoidingJustTheIndicesChosenInThisCall.Invoke(index));
        }

        private static void PlaceOperations
            (IList<Operation> operationsPertainingToTheSameKeyToPlaceIntoFinalOrder,
             IEnumerable<UInt32> indicesToPlaceAt,
             MappingToAvoidPreviouslyChosenIndices mappingToAvoidPreviouslyChosenIndices,
             IList<Operation> operationsPlacedIntoFinalOrder)
        {
            var placementIndices =
                indicesToPlaceAt.Select(index => mappingToAvoidPreviouslyChosenIndices(index)).ToArray();

            for (var operationIndex = 0;
                 operationIndex < operationsPertainingToTheSameKeyToPlaceIntoFinalOrder.Count;
                 ++operationIndex)
            {
                operationsPlacedIntoFinalOrder[(Int32) placementIndices[operationIndex]] =
                    operationsPertainingToTheSameKeyToPlaceIntoFinalOrder[operationIndex];
            }
        }

        private static IEnumerable<UInt32> ChooseIndicesToPlaceAt
            (UInt32 numberOfIndicesToChooseFrom,
             UInt32 numberOfIndicesToChoose,
             UInt32 combinationSelector)
        {
            var indicesToChooseFrom = new UInt32[numberOfIndicesToChooseFrom];

            for (var index = 0U; index < numberOfIndicesToChooseFrom; ++index)
            {
                indicesToChooseFrom[index] = index;
            }

            return CombinatoricUtilities.GenerateCombinationOfGivenSizePreservingOrder
                (numberOfIndicesToChoose, indicesToChooseFrom, combinationSelector);
        }

        private static readonly C5.IList<Key> Keys = new C5.ArrayList<UInt32> {0u, 1u};

        private static readonly IEnumerable<OperationKind> OperationKinds =
            ((IList<Int32>) Enum.GetValues(typeof (OperationKind))).Select(constant => (OperationKind) constant);


        private static TypedTestCaseEnumerableFactory<PlacementOfOperationsIntoFinalOrder> MakeTestCaseEnumerableFactory
            (Random randomBehaviour,
             UInt32 sequenceLength,
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
                (sequenceLength * (UInt32) keys.Count, sequenceLength);

            var testVariableLevelFactoryForIndexCombinationEnumerable =
                MakeTestVariableLevelFactoryForIndexCombinationEnumerable(numberOfCombinations);

            var factoryDealingWithRemainingKeys = MakeTestCaseEnumerableFactory
                (randomBehaviour, sequenceLength, keys.View(1, keys.Count - 1));

            return MakeRecursionInductiveCaseFactory
                (synthesizingFactoryForOperationSequenceEnumerable,
                 testVariableLevelFactoryForIndexCombinationEnumerable,
                 factoryDealingWithRemainingKeys);
        }

        private static TypedTestCaseEnumerableFactory<UInt32> MakeTestVariableLevelFactoryForIndexCombinationEnumerable
            (UInt32 numberOfCombinations)
        {
            var combinationSelector = numberOfCombinations;

            var combinationSelectors = new List<UInt32>();

            while (0U != combinationSelector--)
            {
                combinationSelectors.Add(combinationSelector);
            }
            return TestVariableLevelEnumerableFactory.Create(combinationSelectors);
        }

        private static TypedTestCaseEnumerableFactory<PlacementOfOperationsIntoFinalOrder>
            MakeRecursionInductiveCaseFactory
            (TypedTestCaseEnumerableFactory<IList<Operation>> synthesizingFactoryForOperationSequence,
             TypedTestCaseEnumerableFactory<UInt32> testVariableLevelFactoryForFinalOperationsListIndexCombinations,
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

        private const UInt32 SequenceLength = 5U;

        private const UInt32 CombinationStrength = 3U;

        ///<summary>
        ///</summary>
        [Test]
        public void ManipulateIndexedSortedDictionary()
        {
            var numberOfKeys = Keys.Count;
            var randomBehaviour = new Random(892893767);

            var totalNumberOfOperations = (UInt32) numberOfKeys * SequenceLength;

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

            var totalNumberOfOperations = (UInt32) numberOfKeys * SequenceLength;

            var testCasesEnumerableFactory = MakeTestCaseEnumerableFactory(randomBehaviour, SequenceLength, Keys);

            var operations = new Operation[totalNumberOfOperations];

            var numberOfTestCases = 0ul;

            const string reproductionString =
                "1090764779116129690923515858308014520222336185700694897600592132531120782152518768159051493870023049168112275087193563737654121247279695301706768284208758765325435482632361366471740702028151986034200556078609774343674977374006857504170101744902417360968258795559135760928061626740575880752857804639157248048691316106429511825216773571848030480135349160011179547541527180424901562931258732913329802652273344399988194353423566274712375834395236970663424343007122339584907679961137784484322476753003746370601724359729497704446760160760882389566843954823774444665999657889860067518039205543371272692827407459960512308555611393589094063823858033064965088628813903934585888650060784457616394307194846633862799639782623357974239346723281302847993832484143520503065951929561647537410024880247351721805122670333565211146140787016461074222496743597998812763792665481954523790157479482152135824354515394114667921400618705137415115653460790445326528310975513010933514774853285524375045918567943805470355877751028386081733979113284642768540649071150126391933852893944853938361775663476256655132019133983821810233469483716404812971670822714506368057138070763953797059304802607083765161050993559266766923754290144080418929479983092045447890424803724945122270325041362132066805287735623337726887120094867227210889090774668046917884642365760854931526380385701653695274730591609999003518361430459428085544205149564966622007115984760404477955493179680572867345724688985948425188763010181330644600693907463555196961286112036188683500416809316385579232983180343298935210039233051036884012107849946283848570382946922430954280636205186566281154461358958033922589175952628233216807672680491581731586511768867235049278101122672667196383055660181907628710099464378220674480308793529615780282892159567297270435672946734414981218648686727779276023739426842722158602247558578067092503538679095995953787512265890297017235796982795166652042404785251924913759709134528023763117331531831561224094676110366001858307864376647016424475387356746077903219022865656120818956993085891197730201835069437080856727973541527144579039010019162755909467186332106813538625839223151615758634715789918520865686556228277634783692431661138646283403718158122021286961748486174447463328076321046676292595164718616837850786584711501458877515277701579356627816410426343112788351499539595568279546533979888923587815278584054990729654306576857209257092167315652877546168220870931869071038726650968915624418399993206965010432";

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
                                             UInt32 totalNumberOfOperations, IList<Operation> operations,
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
                        <UInt32,
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