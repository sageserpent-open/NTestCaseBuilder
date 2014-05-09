using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.FSharp.Collections;
using NUnit.Framework;
using SageSerpent.Infrastructure;

namespace NTestCaseBuilder.Examples
{
    [TestFixture]
    public class ComplicatedFilter

    {
        public ITypedFactory<ThingHistory> ThingFactory(int thingRecordIndex, int maximumNumberOfItems)
        {
            var itemIndexFactory = TestVariable.Create(Enumerable.Range(0, maximumNumberOfItems));
            var numberOfRejectionsFactory = TestVariable.Create(Enumerable.Range(0, 3));
            return Synthesis.Create(itemIndexFactory, numberOfRejectionsFactory, numberOfRejectionsFactory,
                (itemIndex, numberOfRejectionsBeforeAcceptance, numberOfRejectionsBeforeWithdrawal) =>
                    new ThingHistory(thingRecordIndex, itemIndex, numberOfRejectionsBeforeAcceptance,
                        numberOfRejectionsBeforeWithdrawal));
        }

        public Filter ThingsShareItemsEfficiently(int numberOfThings)
        {
            return testVariableIndexToLevelNumberAndItemIndexMap =>
            {
                const int numberOfTestVariablesDefiningAThing = 3;
                const int offsetOfTestVariableIndexForAnItemIndexLevel = 0;
                var itemIndexAndPositionPairsFromHistories =
                    (from testVariableIndexToLevelNumberAndLevelValuePair in
                        testVariableIndexToLevelNumberAndItemIndexMap
                        let testVariableLevelIndex = testVariableIndexToLevelNumberAndLevelValuePair.Key
                        where
                            offsetOfTestVariableIndexForAnItemIndexLevel ==
                            testVariableLevelIndex % numberOfTestVariablesDefiningAThing
                        select
                            new
                            {
                                ItemIndex = (int) testVariableIndexToLevelNumberAndLevelValuePair.Value.Item2,
                                PositionOfItemIndex = testVariableLevelIndex / numberOfTestVariablesDefiningAThing
                            })
                        .ToList();

                var itemIndicesFromHistories =
                    (from itemIndexAndPosition in itemIndexAndPositionPairsFromHistories
                        select itemIndexAndPosition.ItemIndex).ToList();

                var itemIndexPositionsFromHistories =
                    (from itemIndexAndPosition in itemIndexAndPositionPairsFromHistories
                        select itemIndexAndPosition.PositionOfItemIndex).ToList();

                if (!itemIndicesFromHistories.Any())
                {
                    return true;
                }

                if (!BargainBasement.IsSorted(itemIndicesFromHistories))
                {
                    return false;
                }

                var minimumItemIndex = itemIndicesFromHistories.Min();
                var maximumItemIndex = itemIndicesFromHistories.Max();

                var minimumItemIndexPosition = itemIndexPositionsFromHistories.Min();
                var maximumItemIndexPosition = itemIndexPositionsFromHistories.Max();

                var itemIndicesCanBeAligned =
                    itemIndicesFromHistories.Zip(itemIndexPositionsFromHistories, (index, position) => index <= position)
                        .All(outcome => outcome);

                if (!itemIndicesCanBeAligned)
                {
                    return false;
                }

                var itemIndicesAreEitherContiguousOrCanMadeSoWithInsertions =
                    SeqModule.Pairwise(itemIndicesFromHistories)
                        .Select(pairOfAdjacentIndices => pairOfAdjacentIndices.Item2 - pairOfAdjacentIndices.Item1)
                        .Zip(
                            SeqModule.Pairwise(itemIndexPositionsFromHistories)
                                .Select(
                                    pairOfAdjacentPositions =>
                                        pairOfAdjacentPositions.Item2 - pairOfAdjacentPositions.Item1),
                            (indexDelta, indexPositionDelta) => indexDelta <= indexPositionDelta)
                        .All(outcome => outcome);

                if (!itemIndicesAreEitherContiguousOrCanMadeSoWithInsertions)
                {
                    return false;
                }

                var numberOfItemIndexPositions = itemIndexPositionsFromHistories.Count();
                var noPositionGapsToPlaceExtraItemIndices = numberOfItemIndexPositions ==
                                                            1 + maximumItemIndexPosition - minimumItemIndexPosition;

                var itemIndicesAreContiguous = itemIndicesFromHistories.Distinct().Count() ==
                                               1 + maximumItemIndex - minimumItemIndex;

                if (!itemIndicesAreContiguous && noPositionGapsToPlaceExtraItemIndices)
                {
                    return false;
                }

                if (itemIndicesAreContiguous && noPositionGapsToPlaceExtraItemIndices)
                {
                    if (numberOfItemIndexPositions == numberOfThings)
                    {
                        return
                            BargainBasement.IsSorted(
                                itemIndicesFromHistories.GroupBy(itemIndex => itemIndex).Select(group => group.Count()));
                    }

                    if (numberOfItemIndexPositions > 2)
                    {
                        return
                            BargainBasement.IsSorted(
                                itemIndicesFromHistories.Skip(1)
                                    .Take(numberOfItemIndexPositions - 2)
                                    .GroupBy(itemIndex => itemIndex)
                                    .Select(group => group.Count()));
                    }
                }

                return true;
            };
        }

        public ITypedFactory<IEnumerable<ThingHistory>> HistoriesForSeveralThingsFactory(int minimumNumberOfThings = 1)
        {
            var historiesForSeveralThingsFactory = Synthesis.Create(
                from thingRecordIndex in Enumerable.Range(0, minimumNumberOfThings)
                select ThingFactory(thingRecordIndex, minimumNumberOfThings))
                .WithFilter(ThingsShareItemsEfficiently(minimumNumberOfThings));

            return
                Interleaving.Create(new[]
                {
                    historiesForSeveralThingsFactory,
                    Deferral.Create(() => HistoriesForSeveralThingsFactory(1 + minimumNumberOfThings))
                });
        }

        [Test]
        public void TestStuff()
        {
            const int deferralBudget = 4;
            var historiesForSeveralThingsFactory = HistoriesForSeveralThingsFactory();

            var randomBehaviourSeedFactory = TestVariable.Create(new[] {67, 890478236, 1123789, 892367});

            var topLevelTestCaseFactory =
                Synthesis.Create(historiesForSeveralThingsFactory, randomBehaviourSeedFactory, Tuple.Create)
                    .WithDeferralBudgetOf(deferralBudget);

            const int strength = 3;

            var numberOfTestCases = topLevelTestCaseFactory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                topLevelTestCase =>
                {
                    Console.WriteLine("***************");
                    foreach (var thingHistory in topLevelTestCase.Item1)
                    {
                        Console.WriteLine("Thing record index: {0} => item index: {1}", thingHistory.ThingRecordIndex,
                            thingHistory.ItemIndex);
                    }
                });

            Console.WriteLine("Number of test cases validated: {0}", numberOfTestCases);
        }
    }

    public class ThingHistory
    {
        public int ThingRecordIndex { get; set; }
        public int ItemIndex { get; set; }

        public ThingHistory(int thingRecordIndex, int itemIndex, int numberOfRejectionsBeforeAcceptance,
            int numberOfRejectionsBeforeWithdrawal)
        {
            ThingRecordIndex = thingRecordIndex;
            ItemIndex = itemIndex;
        }
    }
}