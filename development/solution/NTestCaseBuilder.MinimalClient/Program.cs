using System;

namespace NTestCaseBuilder.MinimalClient
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            ITypedFactory<int> something =
                Interleaving.Create(new[] {Singleton.Create(56), TestVariable.Create(new[] {2, 3})});

            foreach (var item in something.CreateEnumerable(3))
            {
                Console.Out.WriteLine(item);
            }
        }
    }
}