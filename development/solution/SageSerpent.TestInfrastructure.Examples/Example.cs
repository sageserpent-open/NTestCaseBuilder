using System;
using System.Collections.Generic;
using NUnit.Framework;

using SageSerpent.TestInfrastructure;

namespace SageSerpent.TestInfrastructure.Examples
{
    [TestFixture]
    public class Example
    {
        [Test]
        public void TakeEnumeratorOutForTestDrive()
        {
            var a = TestVariableLevelEnumerableFactory.Create(_levelsOne);
            var b = TestVariableLevelEnumerableFactory.Create(_levelsTwo);

            var c = SynthesizedTestCaseEnumerableFactory.Create(new List<ITestCaseEnumerableFactory> {a, b},
                                                                (Func<System.UInt32, System.UInt32, System.String>)
                                                                    ((resultOne, resultTwo) => resultOne.ToString() + resultTwo.ToString()));
            var d = TestVariableLevelEnumerableFactory.Create(_levelsOne);

            var e = InterleavedTestCaseEnumerableFactory.Create(new List<ITestCaseEnumerableFactory> {d, c});



            foreach (System.Object u in e.CreateEnumerable(e.MaximumStrength))
            {
                System.Console.WriteLine(u.ToString());
            }
        }

        private static readonly System.UInt32 [] _levelsOne = {0u, 56u, 789u},
                                                 _levelsTwo = {100u, 123u};
    }
}
