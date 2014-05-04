namespace NTestCaseBuilder

    open System
    open System.Collections.Generic

    type IFilterInput =
        IDictionary<Int32, Int32 * Object>

    /// <summary>Delegate for a filter that can be applied to a factory.</summary>
    /// <remarks>The test variable index keys map to pairs of the
    /// test variable level index and the corresponding
    /// test variable level for the key's test variable.</remarks>
    type Filter =
        delegate of IFilterInput -> Boolean

    /// <summary>Interface for an input passed to a call on a Filter allowing breakdown of the input by tags.</summary>
    type ITaggedFilterInputs =
        /// <summary>For the filter that received the input this method is being called in, examine the
        /// tree of factories whose root is the factory the filter was applied to. For each factory in that
        /// tree marked with a matching tag, produce a pair of the matching tag and a restricted filter input
        /// for that factory.</summary>
        /// <remarks>The result dictionary's key in the result is the index of the tagged factory
        /// within the group of factories whose tags match within a filter's tree, ordering the
        /// group according to a *post-order* depth-first search of all of the the tree's
        /// factories. When calculating the indices, *all* matching tagged factories are considered; this
        /// also includes those factories that are excluded in the test case by interleaves.</remarks>
        /// <remarks>A predicate is used to define what tags match, so changing
        /// the predicate permits tags to be flexibly grouped.</remarks>
        /// <remarks>If a matching tagged factory is excluded by an interleave
        /// for a test case being considered, it will not have an entry in the result's
        /// dictionary. This will create gaps in the dequence of the result's dictionary's key indices.</remarks>
        /// <remarks>If a matching tagged factory is included by an interleave for the test case being considered,
        /// but none of its test variable's have levels assigned, this will be represented by an empty filter input
        /// to distinguish it from an excluded factory whose tag matched.</remarks>
        /// <remarks>Each tagged filter input in the result dictionary is restricted to
        /// the factory whose tag matches. The test variable indices are taken relative
        /// to that factory, as opposed to the factory that the filter was applied to.</remarks>
        /// <remarks>If no tagged factory is found, an empty dictionary is returned.</remarks>
        abstract FilterInputsForMatchingTags: (Object -> Boolean) -> IDictionary<Int32, Object * IFilterInput>

    /// <summary>Delegate for a filter that can be applied to a factory; the filter uses tags to categorise its inputs.</summary>
    type FilterUsingTaggedInputs =
        delegate of ITaggedFilterInputs -> Boolean