using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using SageSerpent.Infrastructure;
using Wintellect.PowerCollections;
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
            var placementIndices = Algorithms.ToArray
                (Algorithms.Convert(indicesToPlaceAt, index => mappingToAvoidPreviouslyChosenIndices(index)));

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

        private static readonly IList<Key> Keys = new List<UInt32> {0u, 1u};

        private static readonly IEnumerable<OperationKind> OperationKinds = Algorithms.Convert
            ((IList<Int32>) Enum.GetValues(typeof (OperationKind)), constant => (OperationKind) constant);


        private static TypedTestCaseEnumerableFactory<PlacementOfOperationsIntoFinalOrder> MakeTestCaseEnumerableFactory
            (Random randomBehaviour,
             UInt32 sequenceLength,
             IList<Key> keys)
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
                (randomBehaviour, sequenceLength, Algorithms.Range(keys, 1, keys.Count - 1));

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
                SynthesizedTestCaseEnumerableFactory.Create
                    <IList<Operation>, UInt32, PlacementOfOperationsIntoFinalOrder, PlacementOfOperationsIntoFinalOrder>
                    (synthesizingFactoryForOperationSequence,
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

            foreach (PlacementOfOperationsIntoFinalOrder placementOfOperationsIntoFinalOrder in
                testCasesEnumerableFactory.CreateEnumerable(CombinationStrength))
            {
                placementOfOperationsIntoFinalOrder(totalNumberOfOperations, index => index, operations);

                var indexedSortedDictionary = new IndexedSortedDictionary<UInt32, Value>();

                ++numberOfTestCases;

                foreach (var operation in operations)
                {
                    //operation(indexedSortedDictionary);
                    //Assert.IsTrue(BargainBasement.IsSorted(indexedSortedDictionary.Keys));
                }
            }

            System.Diagnostics.Debug.Print("Number of test cases: {0}.\n", numberOfTestCases);
        }
    }
}