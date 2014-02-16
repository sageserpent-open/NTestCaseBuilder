using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using SageSerpent.Infrastructure;
using Key = System.Int32;
using Value = System.String;
using Operation = System.Action<System.Collections.Generic.IDictionary<System.Int32, System.String>>;


namespace NTestCaseBuilder.Examples
{
    ///<summary>
    ///</summary>
    [TestFixture]
    public class TestDictionary
    {
        private enum OperationKind
        {
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
                _operationKindToOperationCreatorMapWhereNoEntryExists =
                    new Dictionary<OperationKind, OperationCreator>();

            private readonly Random _randomBehaviour;
            private Value _value;

            public OperationListBuilder(Key key, Random randomBehaviourInitialState)
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
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add(OperationKind.Insertion,
                                                                          AddInsertionOperationThatShouldSucceed);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add(OperationKind.Deletion,
                                                                          AddDeletionOperationThatShouldFail);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add(OperationKind.Replacement,
                                                                          AddReplacementOperation);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add(OperationKind.Query,
                                                                          AddQueryOperationThatShouldFail);
            }

            private void AddStateTransitionsForWhenAnEntryAlreadyExists()
            {
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add(OperationKind.Insertion,
                                                                                 AddInsertionOperationThatShouldFail);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add(OperationKind.Deletion,
                                                                                 AddDeletionOperationThatShouldSucceed);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add(OperationKind.Replacement,
                                                                                 AddReplacementOperation);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add(OperationKind.Query,
                                                                                 AddQueryOperationThatShouldSucceed);
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

            private void AddQueryOperationThatShouldFail()
            {
                Operations.Add(indexedSortedDictionary =>
                                   {
                                       Console.WriteLine("Querying with key: {0} - this should fail.", _key);
                                       Assert.IsFalse(indexedSortedDictionary.ContainsKey(_key));
                                   });
            }

            private void AddQueryOperationThatShouldSucceed()
            {
                var fixedValue = _value;
                Operations.Add(indexedSortedDictionary =>
                                   {
                                       Console.WriteLine(
                                           "Querying with key: {0} - this should succeed and yield: {1}.", _key,
                                           fixedValue);
                                       Assert.IsTrue(indexedSortedDictionary.ContainsKey(_key));
                                       Assert.IsTrue(indexedSortedDictionary[_key] == fixedValue);
                                   });
            }

            private void AddDeletionOperationThatShouldFail()
            {
                Operations.Add(indexedSortedDictionary =>
                                   {
                                       Console.WriteLine("Deleting key: {0} - this should fail.", _key);
                                       Assert.IsFalse(indexedSortedDictionary.Remove(_key));
                                   });
            }

            private void AddDeletionOperationThatShouldSucceed()
            {
                Operations.Add(indexedSortedDictionary =>
                                   {
                                       Console.WriteLine("Deleting key: {0} - this should succeed.", _key);
                                       Assert.IsTrue(indexedSortedDictionary.Remove(_key));
                                   }
                    );
                _value = null;
            }

            private void AddInsertionOperationThatShouldSucceed()
            {
                _value = MakeRandomValue();

                var fixedValue = _value;
                Operations.Add(indexedSortedDictionary =>
                                   {
                                       Console.WriteLine("Adding key: {0} with value: {1} - this should succeed.", _key,
                                                         fixedValue);
                                       indexedSortedDictionary.Add(_key, fixedValue);
                                   });
            }

            private void AddInsertionOperationThatShouldFail()
            {
                var fixedValue = _value;
                Operations.Add(indexedSortedDictionary =>
                                   {
                                       try
                                       {
                                           var newValue = MakeRandomValue();
                                           Console.WriteLine("Adding key: {0} with value: {1} - this should fail.", _key,
                                                             newValue);

                                           indexedSortedDictionary.Add(_key, newValue);
                                       }
                                       catch (ArgumentException)
                                       {
                                           return;
                                       }

                                       var stringBuilder = new StringBuilder();


                                       stringBuilder.AppendFormat(
                                           "Should not have been able to insert with key {0} as it already has an entry in the dictionary {1} of {2}",
                                           _key, indexedSortedDictionary, fixedValue);

                                       Assert.Fail(stringBuilder.ToString());
                                   });
            }

            private void AddReplacementOperation()
            {
                _value = MakeRandomValue();
                var fixedValue = _value;
                Operations.Add(indexedSortedDictionary =>
                                   {
                                       Console.WriteLine("Replacing value for key: {0} with value: {1}.", _key,
                                                         fixedValue);
                                       indexedSortedDictionary[_key] = fixedValue;
                                   });
            }

            private Value MakeRandomValue()
            {
                return _randomBehaviour.ChooseAnyNumberFromOneTo(MaximumValueRepresentation).ToString();
            }

            #region Nested type: OperationCreator

            private delegate void OperationCreator();

            #endregion
        }

        private static Boolean FilterOutThreeOrMoreConsecutiveIdenticalOperationKinds(
            IDictionary<Int32, Tuple<Int32, Object>> testVariableIndexToLevelDictionary)
        {
            var testVariableIndices = testVariableIndexToLevelDictionary.Keys;

            var numberOfTestVariables = testVariableIndices.Count;

            var sortedTestVariableIndices = new Int32[numberOfTestVariables];

            testVariableIndices.CopyTo(sortedTestVariableIndices, 0);

            Array.Sort(sortedTestVariableIndices);

            if (1 >= numberOfTestVariables)
            {
                return true;
            }

            var preceedingTestVariableIndexAndConsecutiveCount = Tuple.Create(sortedTestVariableIndices.First(), 1);

            foreach (var index in Enumerable.Range(1, numberOfTestVariables - 1))
            {
                var testVariableIndex = sortedTestVariableIndices[index];

                var preceedingTestVariableIndex = preceedingTestVariableIndexAndConsecutiveCount.Item1;

                if (1 + preceedingTestVariableIndex == testVariableIndex
                    &&
                    testVariableIndexToLevelDictionary[preceedingTestVariableIndex].Item1 ==
                    testVariableIndexToLevelDictionary[testVariableIndex].Item1)
                {
                    var consecutiveCount = preceedingTestVariableIndexAndConsecutiveCount.Item2;

                    if (2 == consecutiveCount)
                    {
                        return false;
                    }

                    preceedingTestVariableIndexAndConsecutiveCount =
                        Tuple.Create(testVariableIndex, 1 + consecutiveCount);
                }
                else
                {
                    preceedingTestVariableIndexAndConsecutiveCount =
                        Tuple.Create(testVariableIndex, 1);
                }
            }

            return true;
        }

        [Test]
        public void TestStandardDictionaryWithJustOneKey()
        {
            var keyFactory = TestVariable.Create(Enumerable.Range(-2, 5));

            var operationFactory = TestVariable.Create(
                from operationKind in ((IEnumerable<OperationKind>) Enum.GetValues(typeof (OperationKind)))
                select operationKind);

            const Int32 numberOfOperations = 10;

            var randomBehaviour = new Random(0);

            var operationKindSequenceFactory =
                Synthesis.Create(
                    Enumerable.Repeat(operationFactory, numberOfOperations)).WithFilter(
                        FilterOutThreeOrMoreConsecutiveIdenticalOperationKinds);

            var operationListBuilderFactory =
                Synthesis.Create(keyFactory,
                                 operationKindSequenceFactory,
                                 (key, operationKindSequence) =>
                                     {
                                         var result = new OperationListBuilder(key,
                                                                               randomBehaviour);

                                         foreach (
                                             var operationKind in operationKindSequence)
                                         {
                                             result.AppendNewOperationOfKind(operationKind);
                                         }

                                         return result;
                                     });
            const Int32 strength = 4;

            var numberOfTestCasesExercised =
                operationListBuilderFactory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                                                                                        ParameterisedUnitTestForStandardDictionaryWithJustOneKey);

            Console.Out.WriteLine("Exercised {0} test cases.", numberOfTestCasesExercised);
        }

        private static void ParameterisedUnitTestForStandardDictionaryWithJustOneKey(
            OperationListBuilder operationListBuilder)
        {
            IDictionary<Key, Value> systemUnderTest = new Dictionary<Key, Value>();

            Console.WriteLine("**** New Test Case ****");

            foreach (var operation in operationListBuilder.Operations)
            {
                operation(systemUnderTest);
            }
        }
    }
}