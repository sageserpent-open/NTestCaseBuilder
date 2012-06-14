using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;

namespace SageSerpent.TestInfrastructure.WorkedExample
{
    [TestFixture]
    public class TestEncodedFormat
    {
        [Test]
        public void TestEncodingAndDecodingRoundtripStage1()
        {
            ParameterisedUnitTestForEncodingAndDecodingRoundtrip(String.Empty);
        }

        [Test]
        public void TestEncodingAndDecodingRoundtripStage2()
        {
            var factory = SingletonTestCaseEnumerableFactory.Create(String.Empty);
            const Int32 strength = 3;

            factory.ExecuteParameterisedUnitTestForAllTypedTestCases(strength, ParameterisedUnitTestForEncodingAndDecodingRoundtrip);
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
