using System;
using System.Collections.Generic;
using NUnit.Framework;
using SageSerpent.Infrastructure;


namespace NTestCaseBuilder.Examples
{
    ///<summary>
    ///</summary>
    [TestFixture]
    public class SimpleExample
    {
        private static readonly Int32[] LevelsOne = {0, 56, 789, 789}, LevelsTwo = {100, 123};

        ///<summary>
        ///</summary>
        [Test]
        public void TakeEnumeratorOutForTestDrive()
        {
            var a = TestVariable.Create(LevelsOne);
            var b = TestVariable.Create(LevelsTwo);

            var c = Synthesis.Create(a, b,
                                     ((resultOne, resultTwo) =>
                                      string.Format("{0}, {1}", resultOne.ToString(), resultTwo.ToString())));

            var d = Synthesis.Create(TestVariable.Create(LevelsOne), input => input.ToString());

            var dWithATwist = Synthesis.Create(new List<IFactory> {d}, (Converter<Object, Object>) (thing => thing));

            var e = Interleaving.Create(new List<IFactory> {dWithATwist, c});

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
}