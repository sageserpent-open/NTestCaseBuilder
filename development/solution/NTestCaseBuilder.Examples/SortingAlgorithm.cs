using System;
using System.Collections.Generic;

namespace NTestCaseBuilder.Examples
{
    class SortingAlgorithmModule
    {
        /// <summary>
        /// Sorts a sequence of items into ascending order, using the intrinsic ordering of the TItem type.
        /// </summary>
        /// <typeparam name="TItem">Any type with an intrinsic ordering given by implementing IComparable&ltTItem&gt.</typeparam>
        /// <param name="unsorted">Sequence of items to be sorted. This is left unchanged by the call.</param>
        /// <returns>The items sorted into ascending order, as a new collection.</returns>
        public static IEnumerable<TItem> Sort<TItem>(IEnumerable<TItem> unsorted) where TItem: IComparable<TItem>
        {
            throw new NotImplementedException();
        }
    }
}

