using System;
using System.Text;

namespace NTestCaseBuilder.WorkedExample
{
    public class EncodedFormatStage1
    {
        ///<summary>
        /// Constructs an encoding of a string.
        ///</summary>
        ///<param name="stringToBeEncoded">Non-null string to encode: may be an empty string.</param>
        public EncodedFormatStage1(String stringToBeEncoded)
        {
            throw new NotImplementedException();
        }

        public class ProgressiveDecoder
        {
            /// <summary>
            /// Carries out a step of the progressive decoding of the format.
            /// At each step, all of the occurrances of some character in the original encoded
            /// string will be decoded and placed into a string builder at their original
            /// locations. Each step deals with a unique character in the original string.
            /// </summary>
            /// <param name="builderForPartiallyDecodedString">A non-null string builder for the partially decoded result. This is modified on each call, and is intended to be reused across successive calls to this method to achieve a progressive decoding. Can be set up arbitrarily; will be resized to accomodate the need for additional characters, or will be trimmed if too long. Any existing characters not placed into the buffer by a previous call to this method will eventually be overwritten or truncated over a progressive series of calls.</param>
            /// <returns>True if 'builderForPartiallyDecodedString' contains the completely decoded string, false if there is more decoding to follow.</returns>
            public Boolean DecodeIntoAndReportIfCompleted(StringBuilder builderForPartiallyDecodedString)
            {
                throw new NotImplementedException();
            }
        }

        /// <summary>
        /// Creates a new decoder.
        /// </summary>
        /// <returns>A freshly-created decoder set to start progressive decoding at the first character.</returns>
        public ProgressiveDecoder CreateNewDecoder()
        {
            throw new NotImplementedException();
        }
    }
}