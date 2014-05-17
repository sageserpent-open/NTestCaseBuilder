using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;

namespace NTestCaseBuilder.WorkedExample
{
    [TestFixture]
    public class TestEncodedFormat
    {
        private const Char MinimumLevel = 'a';
        private const Char MaximumLevel = 'z';

        private readonly ITypedFactory<String> _emptyStringFactory = Singleton.Create(String.Empty);

        private readonly ITypedFactory<Char> _factoryForSingleCharacters =
            TestVariable.Create(
                Enumerable.Range(0, 1 + MaximumLevel - MinimumLevel)
                    .Select(index => (Char) (MinimumLevel + index)));

        public ITypedFactory<String> BuildFactoryRecursively(Int32 maximumStringLength)
        {
            if (0 == maximumStringLength)
            {
                return _emptyStringFactory;
            }

            var simplerFactoryForShorterStrings = BuildFactoryRecursively(maximumStringLength - 1);

            var factoryForNonEmptyStrings = Synthesis.Create(_factoryForSingleCharacters,
                simplerFactoryForShorterStrings,
                (leftmostCharacterToPrepend, shorterString) =>
                    leftmostCharacterToPrepend + shorterString);

            return Interleaving.Create(new[] {_emptyStringFactory, factoryForNonEmptyStrings});
        }

        public ITypedFactory<String> BuildFactoryRecursivelyUsingDeferral()
        {
            var simplerFactoryForShorterStrings =
                Deferral.Create(BuildFactoryRecursivelyUsingDeferral);

            var factoryForNonEmptyStrings = Synthesis.Create(_factoryForSingleCharacters,
                simplerFactoryForShorterStrings,
                (leftmostCharacterToPrepend, shorterString) =>
                    leftmostCharacterToPrepend + shorterString);

            return Interleaving.Create(new[] {_emptyStringFactory, factoryForNonEmptyStrings});
        }

        public void ParameterisedUnitTestForEncodingAndDecodingRoundtrip(String testCase)
        {
            var histogramFromTestCase = BuildHistogramOfCharacterFrequencies(testCase);

            var encodedFormat = new EncodedFormatStage1(testCase);

            var builderForPartiallyDecodedString = new StringBuilder();

            var expectedSizeOfHistogramFromPartiallyDecodedString = 0;

            var decoder = encodedFormat.CreateNewDecoder();

            while (!decoder.DecodeIntoAndReportIfCompleted(builderForPartiallyDecodedString))
            {
                // Compute histogram for decoded text: each maplet should be contained in the original histogram, and the number of bins in the histogram should grow by one each time.

                var histogramFromPartiallyDecodedString =
                    BuildHistogramOfCharacterFrequencies(builderForPartiallyDecodedString.ToString());

                foreach (var character in histogramFromPartiallyDecodedString.Keys)
                {
                    Assert.IsTrue(histogramFromTestCase.ContainsKey(character));
                    Assert.IsTrue(histogramFromTestCase[character] ==
                                  histogramFromPartiallyDecodedString[character]);
                }

                ++expectedSizeOfHistogramFromPartiallyDecodedString;

                Assert.IsTrue(histogramFromPartiallyDecodedString.Count ==
                              expectedSizeOfHistogramFromPartiallyDecodedString);
            }

            var decodedString = builderForPartiallyDecodedString.ToString();

            Assert.IsTrue(decodedString == testCase);
        }

        private static IDictionary<Char, Int32> BuildHistogramOfCharacterFrequencies(
            IEnumerable<Char> stringDistribution)
        {
            var result = new Dictionary<Char, Int32>();

            foreach (var character in stringDistribution)
            {
                Int32 count;
                if (result.TryGetValue(character, out count))
                {
                    result[character] = 1 + count;
                }
                else
                {
                    result.Add(character, 1);
                }
            }

            return result;
        }

        [Test]
        public void TestEncodingAndDecodingRoundtripStage1()
        {
            ParameterisedUnitTestForEncodingAndDecodingRoundtrip(String.Empty);
        }

        [Test]
        public void TestEncodingAndDecodingRoundtripStage2()
        {
            var factory = Singleton.Create(String.Empty);
            const Int32 strength = 3;

            factory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                ParameterisedUnitTestForEncodingAndDecodingRoundtrip);
        }

        [Test]
        public void TestEncodingAndDecodingRoundtripStage3()
        {
            const Int32 maximumStringLength = 5;

            var factory = BuildFactoryRecursively(maximumStringLength);
            const Int32 strength = 3;

            var numberOfTestCases = factory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                ParameterisedUnitTestForEncodingAndDecodingRoundtrip);

            Console.Out.WriteLine("The parameterised unit test passed for all {0} test cases.",
                numberOfTestCases);
        }

        [Test]
        public void TestEncodingAndDecodingRoundtripStage4()
        {
            const Int32 maximumStringLength = 5;

            var factory =
                BuildFactoryRecursivelyUsingDeferral().WithDeferralBudgetOf(maximumStringLength);
            const Int32 strength = 3;

            var numberOfTestCases = factory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                ParameterisedUnitTestForEncodingAndDecodingRoundtrip);

            Console.Out.WriteLine("The parameterised unit test passed for all {0} test cases.",
                numberOfTestCases);
        }
    }
}