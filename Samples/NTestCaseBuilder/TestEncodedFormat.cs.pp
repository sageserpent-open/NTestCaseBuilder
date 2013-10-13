using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;

using NTestCaseBuilder;

namespace $rootnamespace$.Samples.NTestCaseBuilder
{
    [TestFixture]
    public class TestEncodedFormat
    {
        private const Char MinimumLevel = 'a';
        private const Char MaximumLevel = 'z';

        private readonly ITypedFactory<String> _emptyStringFactory = Singleton.Create(String.Empty);

        private readonly ITypedFactory<Char> _factoryForSingleCharacters =
            TestVariable.Create(
                Enumerable.Range(0, 1 + MaximumLevel - MinimumLevel).Select(index => (Char) (MinimumLevel + index)));

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

            Console.Out.WriteLine("The parameterised unit test passed for all {0} test cases.", numberOfTestCases);
        }

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

        public void ParameterisedUnitTestForEncodingAndDecodingRoundtrip(String testCase)
        {
            IDictionary<Char, Int32> histogramFromTestCase = BuildHistogramOfCharacterFrequencies(testCase);

            var encodedFormat = new EncodedFormatStage1(testCase);

            var builderForPartiallyDecodedString = new StringBuilder();

            Int32 expectedSizeOfHistogramFromPartiallyDecodedString = 0;

            EncodedFormatStage1.ProgressiveDecoder decoder = encodedFormat.CreateNewDecoder();

            while (!decoder.DecodeIntoAndReportIfCompleted(builderForPartiallyDecodedString))
            {
                // Compute histogram for decoded text: each maplet should be contained in the original histogram, and the number of bins in the histogram should grow by one each time.

                IDictionary<Char, Int32> histogramFromPartiallyDecodedString =
                    BuildHistogramOfCharacterFrequencies(builderForPartiallyDecodedString.ToString());

                foreach (var character in histogramFromPartiallyDecodedString.Keys)
                {
                    Assert.IsTrue(histogramFromTestCase.ContainsKey(character));
                    Assert.IsTrue(histogramFromTestCase[character] == histogramFromPartiallyDecodedString[character]);
                }

                ++expectedSizeOfHistogramFromPartiallyDecodedString;

                Assert.IsTrue(histogramFromPartiallyDecodedString.Count ==
                              expectedSizeOfHistogramFromPartiallyDecodedString);
            }

            String decodedString = builderForPartiallyDecodedString.ToString();

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
    }
}