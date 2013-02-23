using System;
using NTestCaseBuilder;

namespace NTestCaseBuilder.MinimalClient
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            TypedTestCaseEnumerableFactory<int> something = InterleavedTestCaseEnumerableFactory.Create(new[]
                                                                                                            {
                                                                                                                SingletonTestCaseEnumerableFactory
                                                                                                                    .
                                                                                                                    Create
                                                                                                                    (56),
                                                                                                                TestVariableLevelEnumerableFactory
                                                                                                                    .
                                                                                                                    Create
                                                                                                                    (new[
                                                                                                                         ]
                                                                                                                         {
                                                                                                                             2
                                                                                                                             ,
                                                                                                                             3
                                                                                                                         })
                                                                                                            }
                );

            foreach(var item in something.CreateEnumerable(3u))
            {
                Console.Out.WriteLine(item);
            }
        }
    }
}
