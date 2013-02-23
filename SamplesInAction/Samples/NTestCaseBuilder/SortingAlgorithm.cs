using System;
using System.Collections.Generic;
using NTestCaseBuilder;

namespace SageSerpent.SamplesInAction.Samples.NTestCaseBuilder
{
    ///<summary>
    ///  Module for holding static methods used to do sorting.
    ///</summary>
    public class SortingAlgorithmModule
    {
        /// <summary>
        ///   Sorts a sequence of items into ascending order, using the intrinsic ordering of the TItem type.
        ///   Has a bug in its implementation - can you spot it?
        /// </summary>
        /// <typeparam name = "TItem">Any type with an intrinsic ordering given by implementing IComparable&lt;TItem&gt;.</typeparam>
        /// <param name = "unsorted">Sequence of items to be sorted. This is left unchanged by the call.</param>
        /// <returns>The items sorted into ascending order, as a new collection.</returns>
        public static IEnumerable<TItem> SortWithBug<TItem>(IEnumerable<TItem> unsorted)
            where TItem : IComparable<TItem>
        {
            return new SortedSet<TItem>(unsorted);
        }

        /// <summary>
        ///   Sorts a sequence of items into ascending order, using the intrinsic ordering of the TItem type.
        /// </summary>
        /// <typeparam name = "TItem">Any type with an intrinsic ordering given by implementing IComparable&lt;TItem&gt;.</typeparam>
        /// <param name = "unsorted">Sequence of items to be sorted. This is left unchanged by the call.</param>
        /// <returns>The items sorted into ascending order, as a new collection.</returns>
        public static IEnumerable<TItem> SortThatWorks<TItem>(IEnumerable<TItem> unsorted)
            where TItem : IComparable<TItem>
        {
            var result = new List<TItem>(unsorted);
            result.Sort();
            return result;
        }
    }
}