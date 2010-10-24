using System;
using System.Collections;
using System.Collections.Generic;
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
        private delegate System.String Synthesis(System.UInt32 resultOne,
                                                 System.UInt32 resultTwo);

        ///<summary>
        ///</summary>
        [Test]
        public void TakeEnumeratorOutForTestDrive()
        {
            var a = TestVariableLevelEnumerableFactory.Create(LevelsOne);
            var b = TestVariableLevelEnumerableFactory.Create(LevelsTwo);

            var c = SynthesizedTestCaseEnumerableFactory.Create(new[] {a, b},
                                                                (Synthesis)
                                                                ((resultOne,
                                                                  resultTwo) =>
                                                                 string.Format("{0}, {1}",
                                                                               resultOne.ToString(),
                                                                               resultTwo.ToString())));
            var d = TestVariableLevelEnumerableFactory.Create(LevelsOne);

            var e = InterleavedTestCaseEnumerableFactory.Create(new List<ITestCaseEnumerableFactory> {d, c});

            var strength = e.MaximumStrength;

            do
            {
                foreach (var u in e.CreateEnumerable(strength))
                {
                    System.Console.WriteLine(u.ToString());
                }

                Console.WriteLine("****************");
            } while (--strength != 0U);
        }

        private static readonly System.UInt32[] LevelsOne = {0u, 56u, 789u},
                                                LevelsTwo = {100u, 123u};
    }

    ///<summary>
    ///</summary>
    [TestFixture]
    public class ComplexExample
    {
        private enum OperationKind
        {
            Insertion,
            Deletion,
            Replacement,
            Query
        }

        private class OperationSequenceBuilder
        {
            public OperationSequenceBuilder
                (Key key,
                 Int32 seed)
            {
                _key = key;
                _randomBehaviour = new RandomBehaviour(seed);

                AddStateTransitionsForWhenAnEntryAlreadyExists();

                AddStateTransitionsForWhenNoEntryExists();
            }

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

            private delegate void OperationCreator();

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
                _operations.Add(indexSortedDictionary => Assert.IsFalse(indexSortedDictionary.ContainsKey(_key)));
            }

            private void AddQueryOperationThatShouldSucceed()
            {
                _operations.Add(delegate(IndexedSortedDictionary<Key, Value> indexedSortedDictionary1)
                                    {
                                        Assert.IsTrue(indexedSortedDictionary1.ContainsKey(_key));
                                        Assert.IsTrue(indexedSortedDictionary1[_key] == _value);
                                    });
            }

            private void AddDeletionOperationThatShouldFail()
            {
                _operations.Add(indexedSortedDictionary => Assert.IsFalse(indexedSortedDictionary.Remove(_key)));
            }

            private void AddDeletionOperationThatShouldSucceed()
            {
                _operations.Add(indexedSortedDictionary => Assert.IsTrue(indexedSortedDictionary.Remove(_key)));
                _value = null;
            }

            private void AddInsertionOperationThatShouldSucceed()
            {
                _value = MakeRandomValue();
                _operations.Add(indexedSortedDictionary => indexedSortedDictionary.Add(_key,
                                                                                       _value));
            }

            private void AddInsertionOperationThatShouldFail()
            {
                _operations.Add(delegate(IndexedSortedDictionary<Key, Value> indexedSortedDictionary)
                                    {
                                        try
                                        {
                                            indexedSortedDictionary.Add(_key,
                                                                        MakeRandomValue());
                                        }
                                        catch (System.ArgumentException)
                                        {
                                            return;
                                        }

                                        var stringBuilder = new StringBuilder();

                                        stringBuilder.AppendFormat(
                                            "Should not have been able to insert with key {0} as it already has an entry in the dictionary {1}",
                                            _key,
                                            indexedSortedDictionary);

                                        Assert.Fail(stringBuilder.ToString());
                                    });
            }

            private void AddReplacementOperation()
            {
                _value = MakeRandomValue();
                _operations.Add(indexedSortedDictionary => indexedSortedDictionary[_key] = _value);
            }

            private Value MakeRandomValue()
            {
                return _randomBehaviour.ChooseAnyNumberFromOneTo.Invoke(MaximumValueRepresentation).ToString();
            }

            private IEnumerable<Operation> Operations
            {
                get { return _operations; }
            }

            private readonly Key _key;
            private readonly IList<Operation> _operations = new List<Operation>();
            private readonly RandomBehaviour _randomBehaviour;
            private Value _value = null;

            private readonly IDictionary<OperationKind, OperationCreator>
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists =
                    new Dictionary<OperationKind, OperationCreator>(),
                _operationKindToOperationCreatorMapWhereNoEntryExists =
                    new Dictionary<OperationKind, OperationCreator>();


            private const UInt32 MaximumValueRepresentation = 20U;
        }

        private static void ManipulateIndexedSortedDictionary
            (UInt32 sequenceLength,
             UInt32 numberOfKeys)
        {
        }
    }
}