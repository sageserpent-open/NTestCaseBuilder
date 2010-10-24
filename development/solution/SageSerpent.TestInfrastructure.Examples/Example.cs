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
        private delegate Value Synthesis(Key resultOne,
                                         Key resultTwo);

        private static readonly Key[] LevelsOne = {0u, 56u, 789u}, LevelsTwo = {100u, 123u};

        ///<summary>
        ///</summary>
        [Test]
        public void TakeEnumeratorOutForTestDrive()
        {
            var a = TestVariableLevelEnumerableFactory.Create(LevelsOne);
            var b = TestVariableLevelEnumerableFactory.Create(LevelsTwo);

            var c = SynthesizedTestCaseEnumerableFactory.Create
                (new[] {a, b},
                 (Synthesis) ((resultOne,
                               resultTwo) => string.Format("{0}, {1}", resultOne.ToString(), resultTwo.ToString())));
            var d = TestVariableLevelEnumerableFactory.Create(LevelsOne);

            var e = InterleavedTestCaseEnumerableFactory.Create(new List<ITestCaseEnumerableFactory> {d, c});

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
            NoOperation,
            Insertion,
            Deletion,
            Replacement,
            Query
        }

        private class OperationListBuilder
        {
            private const Key MaximumValueRepresentation = 20U;
            private readonly Key _key;

            private readonly IDictionary<OperationKind, OperationCreator>
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists =
                    new Dictionary<OperationKind, OperationCreator>(),
                _operationKindToOperationCreatorMapWhereNoEntryExists = new Dictionary<OperationKind, OperationCreator>
                    ();

            private readonly RandomBehaviour _randomBehaviour;
            private Value _value;

            public OperationListBuilder
                (Key key,
                 RandomBehaviour randomBehaviourInitialState)
            {
                Operations = new List<Operation>();
                _key = key;
                _randomBehaviour = new RandomBehaviour(randomBehaviourInitialState);

                AddStateTransitionsForWhenAnEntryAlreadyExists();

                AddStateTransitionsForWhenNoEntryExists();
            }

            public IList<Operation> Operations { get; private set; }

            private void AddStateTransitionsForWhenNoEntryExists()
            {
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
                if (OperationKind.NoOperation == operationKind) return;

                if (null != _value)
                {
                    _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists[operationKind]();
                }
                else
                {
                    _operationKindToOperationCreatorMapWhereNoEntryExists[operationKind]();
                }
            }

            private void AddQueryOperationThatShouldFail() { Operations.Add(indexSortedDictionary => Assert.IsFalse(indexSortedDictionary.ContainsKey(_key))); }

            private void AddQueryOperationThatShouldSucceed()
            {
                Operations.Add
                    (delegate(IndexedSortedDictionary<Key, Value> indexedSortedDictionary1)
                     {
                         Assert.IsTrue(indexedSortedDictionary1.ContainsKey(_key));
                         Assert.IsTrue(indexedSortedDictionary1[_key] == _value);
                     });
            }

            private void AddDeletionOperationThatShouldFail() { Operations.Add(indexedSortedDictionary => Assert.IsFalse(indexedSortedDictionary.Remove(_key))); }

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

            private Value MakeRandomValue() { return _randomBehaviour.ChooseAnyNumberFromOneTo.Invoke(MaximumValueRepresentation).ToString(); }

            #region Nested type: OperationCreator

            private delegate void OperationCreator();

            #endregion
        }

        private static IList<Operation> CreateOperationsAccordingToSequenceOfKinds
            (IEnumerable<OperationKind> kinds,
             Key key,
             RandomBehaviour randomBehaviour)
        {
            var operationsListBuilder = new OperationListBuilder(key, randomBehaviour);

            foreach (var operationKind in kinds)
            {
                operationsListBuilder.AppendNewOperationOfKind(operationKind);
            }

            return operationsListBuilder.Operations;
        }

        private delegate IList<Operation> OperationsCreator(OperationKind operationKindOne,
                                                            OperationKind operationKindTwo,
                                                            OperationKind operationKindThree,
                                                            OperationKind operationKindFour);

        private delegate UInt32 MappingToAvoidPreviouslyChosenIndices(UInt32 index);

        private delegate void PlacementOfOperationsIntoFinalOrder(UInt32 numberOfIndicesToChooseFrom,
                                                                  MappingToAvoidPreviouslyChosenIndices
                                                                      mappingToAvoidPreviouslyChosenIndices,
                                                                  IList<Operation> operationsPlacedIntoFinalOrder);

        private delegate PlacementOfOperationsIntoFinalOrder InductiveCasePlacementBuilder(
            IList<Operation> operationsPertainingToTheSameKeyToPlaceIntoFinalOrder,
            UInt32 combinationSelector,
            PlacementOfOperationsIntoFinalOrder placementOfOperationsForRemainingKeysIntoFinalOrder);

        private delegate PlacementOfOperationsIntoFinalOrder BaseCasePlacementBuilder();

        private static void BaseCaseForPlacementOfOperationsIntoFinalOrder
            (UInt32 numberOfIndicesToChooseFrom,
             MappingToAvoidPreviouslyChosenIndices mappingToAvoidPreviouslyChosenIndices,
             IList<Operation> operationsPlacedIntoFinalOrder) { }

        private static PlacementOfOperationsIntoFinalOrder BuildInductiveCaseForPlacementOfOperationsIntoFinalOrder
            (IList<Operation> operationsPertainingToTheSameKeyToPlaceIntoFinalOrder,
             UInt32 combinationSelector,
             PlacementOfOperationsIntoFinalOrder placementOfOperationsForRemainingKeysIntoFinalOrder)
        {
            PlacementOfOperationsIntoFinalOrder result = delegate(UInt32 numberOfIndicesToChooseFrom,
                                                                  MappingToAvoidPreviouslyChosenIndices
                                                                      mappingToAvoidPreviouslyChosenIndices,
                                                                  IList<Operation> operationsPlacedIntoFinalOrder)
                                                         {
                                                             var numberOfIndicesToChoose =
                                                                 (UInt32)
                                                                 operationsPertainingToTheSameKeyToPlaceIntoFinalOrder.
                                                                     Count;
                                                             if (numberOfIndicesToChooseFrom < numberOfIndicesToChoose)
                                                             {
                                                                 throw new LogicErrorException
                                                                     ("Test has an internal logic error - should have eventually reached base case"
                                                                      +
                                                                      " where all indices for placement had been chosen without having to look for more");
                                                             }

                                                             var indicesToPlaceAt = ChooseIndicesToPlaceAt
                                                                 (numberOfIndicesToChooseFrom,
                                                                  numberOfIndicesToChoose,
                                                                  combinationSelector);

                                                             PlaceOperations
                                                                 (operationsPertainingToTheSameKeyToPlaceIntoFinalOrder,
                                                                  indicesToPlaceAt,
                                                                  mappingToAvoidPreviouslyChosenIndices,
                                                                  operationsPlacedIntoFinalOrder);

                                                             var composedMappingToAvoidAllIndices =
                                                                 ComposeMappingToAvoidAllIndices
                                                                     (indicesToPlaceAt,
                                                                      mappingToAvoidPreviouslyChosenIndices);

                                                             if (numberOfIndicesToChooseFrom > numberOfIndicesToChoose)
                                                             {
                                                                 placementOfOperationsForRemainingKeysIntoFinalOrder
                                                                     (numberOfIndicesToChooseFrom
                                                                      - numberOfIndicesToChoose,
                                                                      composedMappingToAvoidAllIndices,
                                                                      operationsPlacedIntoFinalOrder);
                                                             }
                                                         };


            return result;
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

        private static readonly IList<Key> Keys = new List<UInt32> {0u, 1u};//, 2u, 67u, 68u, 387786776u, 45u};

        private static readonly IEnumerable<OperationKind> OperationKinds = Algorithms.Convert
            ((IList<Int32>) Enum.GetValues(typeof (OperationKind)), constant => (OperationKind) constant);


        private static ITestCaseEnumerableFactory MakeTestCaseEnumerableFactory
            (RandomBehaviour randomBehaviour,
             UInt32 sequenceLength,
             IList<Key> keys)
        {
            if (0 == keys.Count)
            {
                return MakeRecursionBaseFactory();
            }

            var key = keys[0];

            var synthesizingFactoryForOperationSequence = MakeSynthesizingFactoryForOperationSequence
                (sequenceLength, key, randomBehaviour);

            var testVariableLevelFactoryForFinalOperationsListIndexCombinations =
                MakeFactoryForFinalOperationsListIndexCombinations(keys, sequenceLength);

            var factoryDealingWithRemainingKeys = MakeTestCaseEnumerableFactory
                (randomBehaviour, sequenceLength, Algorithms.Range(keys, 0, keys.Count - 1));

            return MakeRecursionInductiveCaseFactory
                (synthesizingFactoryForOperationSequence,
                 testVariableLevelFactoryForFinalOperationsListIndexCombinations,
                 factoryDealingWithRemainingKeys);
        }

        private static ITestCaseEnumerableFactory MakeRecursionInductiveCaseFactory
            (ITestCaseEnumerableFactory synthesizingFactoryForOperationSequence,
             ITestCaseEnumerableFactory testVariableLevelFactoryForFinalOperationsListIndexCombinations,
             ITestCaseEnumerableFactory factoryDealingWithRemainingKeys)
        {
            return SynthesizedTestCaseEnumerableFactory.Create
                (new[]
                     {
                         synthesizingFactoryForOperationSequence,
                         testVariableLevelFactoryForFinalOperationsListIndexCombinations, factoryDealingWithRemainingKeys
                     },
                 (InductiveCasePlacementBuilder) BuildInductiveCaseForPlacementOfOperationsIntoFinalOrder);
        }

        private static ITestCaseEnumerableFactory MakeRecursionBaseFactory()
        {
            return SynthesizedTestCaseEnumerableFactory.Create
                (new ITestCaseEnumerableFactory[] {},
                 (BaseCasePlacementBuilder) (() => BaseCaseForPlacementOfOperationsIntoFinalOrder));
        }

        private static ITestCaseEnumerableFactory MakeFactoryForFinalOperationsListIndexCombinations
            (ICollection<Key> keys,
             UInt32 sequenceLength)
        {
            var combinationSelector = BargainBasement.NumberOfCombinations
                (sequenceLength, sequenceLength * (UInt32) keys.Count);

            var combinationSelectors = new List<UInt32>();

            while (0U != combinationSelector--)
            {
                combinationSelectors.Add(combinationSelector);
            }

            return TestVariableLevelEnumerableFactory.Create(combinationSelectors);
        }

        private static ITestCaseEnumerableFactory MakeSynthesizingFactoryForOperationSequence
            (UInt32 sequenceLength,
             Key key,
             RandomBehaviour randomBehaviour)
        {
            var testVariableLevelFactoriesForOperationKinds = new List<ITestCaseEnumerableFactory>();

            var numberOfFactoriesLeftToCreate = sequenceLength;

            while (0U != numberOfFactoriesLeftToCreate--)
            {
                testVariableLevelFactoriesForOperationKinds.Add
                    (TestVariableLevelEnumerableFactory.Create(OperationKinds));
            }

            return SynthesizedTestCaseEnumerableFactory.Create
                (testVariableLevelFactoriesForOperationKinds,
                 (OperationsCreator) ((operationKindOne,
                                       operationKindTwo,
                                       operationKindThree,
                                       operationKindFour) =>
                                      CreateOperationsAccordingToSequenceOfKinds
                                          (new[]
                                               {
                                                   operationKindOne, operationKindTwo, operationKindThree,
                                                   operationKindFour
                                               },
                                           key,
                                           randomBehaviour)));
        }

        private const UInt32 SequenceLength = 4U;

        private const UInt32 CombinationStrength = 3U;

        ///<summary>
        ///</summary>
        [Test]
        public void ManipulateIndexedSortedDictionary()
        {
            var numberOfKeys = Keys.Count;
            var randomBehaviour = new RandomBehaviour(892893767);

            var totalNumberOfOperations = (UInt32) numberOfKeys * SequenceLength;

            var testCasesEnumerableFactory = MakeTestCaseEnumerableFactory(randomBehaviour, SequenceLength, Keys);

            var operations = new Operation[totalNumberOfOperations];

            foreach (PlacementOfOperationsIntoFinalOrder placementOfOperationsIntoFinalOrder in
                testCasesEnumerableFactory.CreateEnumerable(CombinationStrength))
            {
                placementOfOperationsIntoFinalOrder(totalNumberOfOperations, index => index, operations);

                var indexedSortedDictionary = new IndexedSortedDictionary<UInt32, Value>();

                foreach (var operation in operations)
                {
                    operation(indexedSortedDictionary);
                    Assert.IsTrue(BargainBasement.IsSorted(indexedSortedDictionary.Keys));
                }
            }
        }
    }
}