namespace NTestCaseBuilder

    open System
    open System.Collections.Generic

    type FilterInput =
        IDictionary<Int32, Int32 * Object>

    /// <summary>Delegate for a filter that can be applied to a factory.</summary>
    /// <remarks>The test variable index keys map to pairs of the
    /// test variable level index and the corresponding
    /// test variable level for the key's test variable.</remarks>
    type LevelCombinationFilter =
        delegate of FilterInput -> Boolean

    /// <summary>Alternative interface for an input passed to a call on a LevelCombinationFilter allowing breakdown of the input by tags.</summary>
    /// <remarks>The interface is accessed by casting the filter's input within its
    /// implementation - it is always available as an option for filters to use.</remarks>
    type ITaggedFilterInputs =
        /// <summary>For the filter that received the input this method is being called in, examine the
        /// tree of factories whose root is the factory the filter was applied to. For each factory in that
        /// tree marked with the given tag, produce a restricted filter input for that factory.</summary>
        /// <remarks>The outer dictionary's key in the result is the index of the tagged factory
        /// within the group of factories tagged within a filter's tree, ordering the
        /// group according to a pre-order depth-first search of all of the the tree's
        /// factories.</remarks>
        /// <remarks>An inner dictionary in the result is a filter input, only restricted to
        /// the specific/ tagged factory picked out by the tag and the index within the tag's
        /// group. The test variable indices are taken relative to that factory, as opposed
        /// to the factory that the filter was applied to.</remarks>
        /// <remarks>An inner dictionary in the result does not support the interface ITaggedFilterInputs.</remarks>
        abstract FilterInputsForTag: Object -> IDictionary<Int32, FilterInput>

        abstract FilterInputsForTag: Object * Func<Object, Object, Boolean> -> IDictionary<Int32, FilterInput>