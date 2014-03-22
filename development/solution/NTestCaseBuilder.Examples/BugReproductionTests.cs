using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using SageSerpent.Infrastructure;

namespace NTestCaseBuilder.Examples
{
    [TestFixture]
    public class BugReproductionTests

    {
        public ITypedFactory<ThingHistory> ThingFactory(int thingRecordIndex)
        {
            var itemIndexFactory = TestVariable.Create(Enumerable.Range(0, 5));
            var numberOfRejectionsFactory = TestVariable.Create(Enumerable.Range(0, 3));
            return Synthesis.Create(itemIndexFactory, numberOfRejectionsFactory, numberOfRejectionsFactory,
                (itemIndex, numberOfRejectionsBeforeAcceptance, numberOfRejectionsBeforeWithdrawal) =>
                    new ThingHistory(thingRecordIndex, itemIndex, numberOfRejectionsBeforeAcceptance,
                        numberOfRejectionsBeforeWithdrawal));
        }

        public LevelCombinationFilter ThingsShareItemsEfficiently(int numberOfThings)
        {
            return testVariableIndexToLevelNumberAndItemIndexMap =>
            {
                const int numberOfTestVariablesDefiningAnThing = 3;
                const int offsetOfTestVariableIndexForAnItemIndexLevel = 0;
                var itemIndicesFromHistories =
                    from testVariableIndexToLevelNumberAndLevelValuePair in
                        testVariableIndexToLevelNumberAndItemIndexMap
                    let testVariableLevelIndex = testVariableIndexToLevelNumberAndLevelValuePair.Key
                    where
                        offsetOfTestVariableIndexForAnItemIndexLevel ==
                        testVariableLevelIndex % numberOfTestVariablesDefiningAnThing
                    select (int) testVariableIndexToLevelNumberAndLevelValuePair.Value.Item2;
                var allBetsAreOff = !itemIndicesFromHistories.Any();
                return allBetsAreOff ||
                       BargainBasement.IsSorted(itemIndicesFromHistories) &&
                       BargainBasement.IsSorted(
                           itemIndicesFromHistories.GroupBy(itemIndex => itemIndex).Select(group => group.Count())) &&
                       0 == itemIndicesFromHistories.Min() &&
                       itemIndicesFromHistories.Distinct().Count() == 1 + itemIndicesFromHistories.Max() &&
                       itemIndicesFromHistories.All(itemIndex => itemIndex < numberOfThings);
            };
        }

        public ITypedFactory<IEnumerable<ThingHistory>> HistoriesForSeveralThingsFactory(int minimumNumberOfThings = 1)
        {
            var historiesForSeveralThingsFactory =
                Synthesis.Create(from thingRecordIndex in Enumerable.Range(0, minimumNumberOfThings)
                    select ThingFactory(thingRecordIndex))
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
            const int deferralBudget = 5;
            var historiesForSeveralThingsFactory = HistoriesForSeveralThingsFactory();

            var randomBehaviourSeedFactory = TestVariable.Create(new[] {67, 890478236, 1123789, 892367});

            var topLevelTestCaseFactory =
                Synthesis.Create(historiesForSeveralThingsFactory, randomBehaviourSeedFactory, Tuple.Create)
                    .WithDeferralBudgetOf(deferralBudget);

            const int strength = 4;

            var numberOfTestCases = topLevelTestCaseFactory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                topLevelTestCase => { });

            Console.WriteLine("Number of test cases validated: {0}", numberOfTestCases);
        }
    }

    public class ThingHistory
    {
        public ThingHistory(int thingRecordIndex, int itemIndex, int numberOfRejectionsBeforeAcceptance,
            int numberOfRejectionsBeforeWithdrawal)
        {
        }
    }
}