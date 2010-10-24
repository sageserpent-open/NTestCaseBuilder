using System;
using System.Collections.Generic;
using NUnit.Framework;
using SageSerpent.TestInfrastructure;

namespace SageSerpent.TestInfrastructure.Examples
{
    ///<summary>
    ///</summary>
    [TestFixture]
    public class Example
    {
        private delegate System.String Synthesis(System.UInt32 resultOne,
                                                 System.UInt32 resultTwo);

        ///<summary>
        ///</summary>
        [Test]
        public void TakeEnumeratorOutForTestDrive()
        {
            var a = TestVariableLevelEnumerableFactory.Create(_levelsOne);
            var b = TestVariableLevelEnumerableFactory.Create(_levelsTwo);

            var c = SynthesizedTestCaseEnumerableFactory.Create(new List<ITestCaseEnumerableFactory> {a, b},
                                                                (Synthesis)
                                                                ((resultOne,
                                                                  resultTwo) =>
                                                                 string.Format("{0} {1}",
                                                                               resultOne.ToString(),
                                                                               resultTwo.ToString())));
            var d = TestVariableLevelEnumerableFactory.Create(_levelsOne);

            var e = InterleavedTestCaseEnumerableFactory.Create(new List<ITestCaseEnumerableFactory> {d, c});

            System.UInt32 strength = e.MaximumStrength;

            do
            {
                foreach (System.Object u in e.CreateEnumerable(strength))
                {
                    System.Console.WriteLine(u.ToString());
                }

                System.Console.WriteLine("****************");
            } while (--strength != 0U);
        }

        private static readonly System.UInt32[] _levelsOne = {0u, 56u, 789u},
                                                _levelsTwo = {100u, 123u};
    }
}