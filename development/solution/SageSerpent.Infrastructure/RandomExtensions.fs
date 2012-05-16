[<System.Runtime.CompilerServices.Extension>]
module SageSerpent.Infrastructure.RandomExtensions
    open System
    open Wintellect.PowerCollections
    open Microsoft.FSharp.Collections

    module RandomDetail =
        let inline require preconditionResult =
            if not preconditionResult
            then
                raise (PreconditionViolationException "Precondition was violated.")

        let inline assume assumptionResult =
            if not assumptionResult
            then
                raise (InternalAssertionViolationException "Internal assertion was violated.")

        let inline firstOrElseSecond first second =
            match first with
                Some _ -> first
              | None -> second

        type BinaryTreeNode =
            InteriorNode of InteriorNode
          | EmptySubtree

            member this.InclusiveLowerBoundForAllItemsInSubtree =
                match this with
                    InteriorNode
                    {
                        LowerBoundForItemRange = lowerBoundForItemRange
                        LesserSubtree = lesserSubtree
                    } -> firstOrElseSecond lesserSubtree.InclusiveLowerBoundForAllItemsInSubtree (Some lowerBoundForItemRange)
                  | EmptySubtree -> None

            member this.ExclusiveUpperBoundForAllItemsInSubtree =
                match this with
                    InteriorNode
                    {
                        UpperBoundForItemRange = upperBoundForItemRange
                        GreaterSubtree = greaterSubtree
                    } -> firstOrElseSecond greaterSubtree.ExclusiveUpperBoundForAllItemsInSubtree (Some upperBoundForItemRange)
                  | EmptySubtree -> None

            member this.NumberOfInteriorNodesInSubtree =
                match this with
                    InteriorNode
                    {
                        LesserSubtree = lesserSubtree
                        GreaterSubtree = greaterSubtree
                    } -> 1 + lesserSubtree.NumberOfInteriorNodesInSubtree + greaterSubtree.NumberOfInteriorNodesInSubtree
                  | EmptySubtree -> 0

            member this.NumberOfItemsInSubtree =
                match this with
                    InteriorNode
                    {
                        LowerBoundForItemRange = lowerBoundForItemRange
                        UpperBoundForItemRange = upperBoundForItemRange
                        LesserSubtree = lesserSubtree
                        GreaterSubtree = greaterSubtree
                    } ->
                        let numberOfItemsInRange = 1 + upperBoundForItemRange - lowerBoundForItemRange
                        numberOfItemsInRange + lesserSubtree.NumberOfItemsInSubtree + greaterSubtree.NumberOfItemsInSubtree
                  | EmptySubtree -> 0

            member this.NumberOfVacantSlotsInSubtreeWithinRange inclusiveLowerBound
                                                                exclusiveUpperBound =
                require (inclusiveLowerBound >= 0)
                require (inclusiveLowerBound <= exclusiveUpperBound)

                match this with
                    InteriorNode
                    {
                        LowerBoundForItemRange = lowerBoundForItemRange
                        UpperBoundForItemRange = upperBoundForItemRange
                        LesserSubtree = lesserSubtree
                        GreaterSubtree = greaterSubtree
                    } ->
                        require (match this.InclusiveLowerBoundForAllItemsInSubtree with
                                    Some inclusiveLowerBoundForAllItemsInSubtree -> inclusiveLowerBound <= inclusiveLowerBoundForAllItemsInSubtree)

                        require(match this.ExclusiveUpperBoundForAllItemsInSubtree with
                                    Some exclusiveUpperBoundForAllItemsInSubtree -> exclusiveUpperBoundForAllItemsInSubtree <= exclusiveUpperBound)

                  | EmptySubtree ->
                        require this.InclusiveLowerBoundForAllItemsInSubtree.IsNone
                        require this.ExclusiveUpperBoundForAllItemsInSubtree.IsNone

                exclusiveUpperBound - inclusiveLowerBound - this.NumberOfItemsInSubtree

            member this.AddNewItemInTheVacantSlotAtIndex(indexOfVacantSlotAsOrderedByMissingItem, inclusiveLowerBound, exclusiveUpperBound) =
                require (indexOfVacantSlotAsOrderedByMissingItem >= 0)

                require (inclusiveLowerBound >= 0)
                require (inclusiveLowerBound < exclusiveUpperBound)

                match this with
                    InteriorNode
                    {
                        LowerBoundForItemRange = lowerBoundForItemRange
                        UpperBoundForItemRange = upperBoundForItemRange
                        LesserSubtree = lesserSubtree
                        GreaterSubtree = greaterSubtree
                    } as thisAsInteriorNode ->
                        require (inclusiveLowerBound <= lowerBoundForItemRange)
                        require (exclusiveUpperBound > upperBoundForItemRange)
        
                        let effectiveIndexAssociatedWithThisInteriorNode = lesserSubtree.NumberOfVacantSlotsInSubtreeWithinRange inclusiveLowerBound lowerBoundForItemRange

                        let recurseOnLesserSubtree () =
                          let (lesserSubtreeResult, modifiedItemResult) =
                            lesserSubtree.AddNewItemInTheVacantSlotAtIndex(indexOfVacantSlotAsOrderedByMissingItem, inclusiveLowerBound, lowerBoundForItemRange)

                          (match lesserSubtreeResult with
                            InteriorNode
                            {
                                LowerBoundForItemRange = lowerBoundForItemRangeFromLesserSubtree
                                UpperBoundForItemRange = upperBoundForItemRangeFromLesserSubtree
                                LesserSubtree = lesserSubtreeFromLesserSubtree
                                GreaterSubtree = EmptySubtree
                            } when 1 + upperBoundForItemRangeFromLesserSubtree = lowerBoundForItemRange ->
                                InteriorNode.Construct
                                    {
                                        LowerBoundForItemRange = lowerBoundForItemRangeFromLesserSubtree
                                        UpperBoundForItemRange = upperBoundForItemRange
                                        LesserSubtree = lesserSubtreeFromLesserSubtree
                                        GreaterSubtree = greaterSubtree
                                    }

                          | InteriorNode
                            {
                                LowerBoundForItemRange = lowerBoundForItemRangeFromLesserSubtree
                                UpperBoundForItemRange = upperBoundForItemRangeFromLesserSubtree
                                LesserSubtree = lesserSubtreeFromLesserSubtree
                                GreaterSubtree = greaterSubtreeFromLesserSubtree
                            } ->
                                InteriorNode.Construct
                                    {
                                        LowerBoundForItemRange = lowerBoundForItemRangeFromLesserSubtree
                                        UpperBoundForItemRange = upperBoundForItemRangeFromLesserSubtree
                                        LesserSubtree = lesserSubtreeFromLesserSubtree
                                        GreaterSubtree = InteriorNode.Construct
                                                            {
                                                                LowerBoundForItemRange = lowerBoundForItemRange
                                                                UpperBoundForItemRange = upperBoundForItemRange
                                                                LesserSubtree = greaterSubtreeFromLesserSubtree
                                                                GreaterSubtree = greaterSubtree
                                                            }
                                    }

                          | _ ->
                                InteriorNode.Construct
                                    {
                                        LowerBoundForItemRange = lowerBoundForItemRange
                                        UpperBoundForItemRange = upperBoundForItemRange
                                        LesserSubtree = lesserSubtreeResult
                                        GreaterSubtree = greaterSubtree
                                    })
                          , modifiedItemResult

                        let recurseOnGreaterSubtree () =
                          let greaterSubtreeResult
                              , modifiedItemResult =
                            greaterSubtree.AddNewItemInTheVacantSlotAtIndex ((indexOfVacantSlotAsOrderedByMissingItem - effectiveIndexAssociatedWithThisInteriorNode), (1 + upperBoundForItemRange), exclusiveUpperBound)

                          (match greaterSubtreeResult with
                            InteriorNode
                            {
                                LowerBoundForItemRange = lowerBoundForItemRangeFromGreaterSubtree
                                UpperBoundForItemRange = upperBoundForItemRangeFromGreaterSubtree
                                LesserSubtree = EmptySubtree
                                GreaterSubtree = greaterSubtreeFromGreaterSubtree
                            } when 1 + upperBoundForItemRange = lowerBoundForItemRangeFromGreaterSubtree ->
                                InteriorNode.Construct
                                    {
                                        LowerBoundForItemRange = lowerBoundForItemRange
                                        UpperBoundForItemRange = upperBoundForItemRangeFromGreaterSubtree
                                        LesserSubtree = lesserSubtree
                                        GreaterSubtree = greaterSubtreeFromGreaterSubtree
                                    }

                          | InteriorNode
                            {
                                LowerBoundForItemRange = lowerBoundForItemRangeFromGreaterSubtree
                                UpperBoundForItemRange = upperBoundForItemRangeFromGreaterSubtree
                                LesserSubtree = lesserSubtreeFromGreaterSubtree
                                GreaterSubtree = greaterSubtreeFromGreaterSubtree
                            } ->
                                InteriorNode.Construct
                                    {
                                        LowerBoundForItemRange = lowerBoundForItemRangeFromGreaterSubtree
                                        UpperBoundForItemRange = upperBoundForItemRangeFromGreaterSubtree
                                        LesserSubtree = InteriorNode.Construct
                                                            {
                                                                LowerBoundForItemRange = lowerBoundForItemRange
                                                                UpperBoundForItemRange = upperBoundForItemRange
                                                                LesserSubtree = lesserSubtree
                                                                GreaterSubtree = lesserSubtreeFromGreaterSubtree
                                                            }
                                        GreaterSubtree = greaterSubtreeFromGreaterSubtree
                                    }

                          | _ ->
                                InteriorNode.Construct
                                    {
                                        LowerBoundForItemRange = lowerBoundForItemRange
                                        UpperBoundForItemRange = upperBoundForItemRange
                                        LesserSubtree = lesserSubtree
                                        GreaterSubtree = greaterSubtreeResult
                                    })
                          , modifiedItemResult

                        let lesserSubtreeCanBeConsidered inclusiveLowerBound = inclusiveLowerBound < lowerBoundForItemRange

                        let greaterSubtreeCanBeConsidered exclusiveUpperBound = 1 + upperBoundForItemRange < exclusiveUpperBound



                        match lesserSubtreeCanBeConsidered inclusiveLowerBound
                              , greaterSubtreeCanBeConsidered exclusiveUpperBound with
                            true
                            , false ->
                                recurseOnLesserSubtree ()

                          | false
                            , true ->
                                assume (0 = inclusiveLowerBound) // NOTE: in theory this case can occur for other values of 'inclusiveLowerBound', but range-fusion prevents this happening in practice.
                                recurseOnGreaterSubtree ()

                          | true
                            , true ->
                                if 0 > compare indexOfVacantSlotAsOrderedByMissingItem effectiveIndexAssociatedWithThisInteriorNode
                                then
                                    recurseOnLesserSubtree ()
                                else
                                    recurseOnGreaterSubtree ()

                  | EmptySubtree ->
                        let generatedItem = inclusiveLowerBound + indexOfVacantSlotAsOrderedByMissingItem
        
                        assume (generatedItem < exclusiveUpperBound)
        
                        InteriorNode.TreeForSingleItem generatedItem
                        , generatedItem



            member this.AddNewItemInTheVacantSlotAtIndex(indexOfVacantSlotAsOrderedByMissingItem, exclusiveLimit) = this.AddNewItemInTheVacantSlotAtIndex(indexOfVacantSlotAsOrderedByMissingItem, 0, exclusiveLimit)

        and InteriorNode = 
            {
                LowerBoundForItemRange: Int32
                UpperBoundForItemRange: Int32
                LesserSubtree: BinaryTreeNode
                GreaterSubtree: BinaryTreeNode
            }

            static member Construct
                                ({
                                    LowerBoundForItemRange = lowerBoundForItemRange
                                    UpperBoundForItemRange = upperBoundForItemRange
                                    LesserSubtree = lesserSubtree
                                    GreaterSubtree = greaterSubtree
                                 } as interiorNode) =
                match lesserSubtree with
                    InteriorNode
                    {
                        UpperBoundForItemRange = upperBoundForItemRangeFromLesserSubtree
                    } -> require(upperBoundForItemRangeFromLesserSubtree + 1 < lowerBoundForItemRange)
                  | _ -> ()
            
                match greaterSubtree with
                    InteriorNode
                    {
                        LowerBoundForItemRange = lowerBoundForItemRangeFromGreaterSubtree
                    } -> require(upperBoundForItemRange + 1 < lowerBoundForItemRangeFromGreaterSubtree)
                  | _ -> ()

                InteriorNode interiorNode

            static member TreeForSingleItem singleItem =
                InteriorNode.Construct
                    {
                        LowerBoundForItemRange = singleItem
                        UpperBoundForItemRange = singleItem
                        LesserSubtree = EmptySubtree
                        GreaterSubtree = EmptySubtree
                    }

    open RandomDetail
                
    type Random with
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("BuildRandomSequenceOfDistinctIntegersFromZeroToOneLessThan")>]
        member this.BuildRandomSequenceOfDistinctIntegersFromZeroToOneLessThan exclusiveLimit =
            let rec chooseAndRecordUniqueItems exclusiveLimitOnVacantSlotIndex (previouslyChosenItemsAsBinaryTree: BinaryTreeNode) =
                if 0 = exclusiveLimitOnVacantSlotIndex
                    then
                        Seq.empty
                else
                    let (chosenItemsAsBinaryTree, chosenItem) =
                        previouslyChosenItemsAsBinaryTree.AddNewItemInTheVacantSlotAtIndex(this.ChooseAnyNumberFromZeroToOneLessThan(uint32 exclusiveLimitOnVacantSlotIndex) |> int32, exclusiveLimit)

                    seq
                        {
                            yield uint32 chosenItem
                            yield! chooseAndRecordUniqueItems (exclusiveLimitOnVacantSlotIndex - 1) chosenItemsAsBinaryTree
                        }

            chooseAndRecordUniqueItems exclusiveLimit EmptySubtree

        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseAnyNumberFromZeroToOneLessThan")>]
        member this.ChooseAnyNumberFromZeroToOneLessThan exclusiveLimit =
            exclusiveLimit
            |> int32
            |> this.Next
            |> uint32
            
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseAnyNumberFromOneTo")>]
        member this.ChooseAnyNumberFromOneTo inclusiveLimit =
            this.ChooseAnyNumberFromZeroToOneLessThan inclusiveLimit + 1u
            
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("HeadsItIs")>]
        member this.HeadsItIs () =
            this.ChooseAnyNumberFromZeroToOneLessThan 2u = 0u
            
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseSeveralOf")>]
        member this.ChooseSeveralOf ((candidates: #seq<_>), numberToChoose) =
            if numberToChoose > uint32 (Seq.length candidates)
            then
                raise (PreconditionViolationException "Insufficient number of candidates to satisfy number to choose.")
            else
                let candidatesWithRandomAccess = candidates |> Array.ofSeq

                let numberOfCandidates = candidatesWithRandomAccess.Length

                let permutationOfIndicesOfOriginalOrderOfCandidates = this.BuildRandomSequenceOfDistinctIntegersFromZeroToOneLessThan(numberOfCandidates)

                [|
                        for permutedIndex in permutationOfIndicesOfOriginalOrderOfCandidates |> Seq.take (int32 numberToChoose) do
                            yield candidatesWithRandomAccess.[int32 permutedIndex]
                 |]

                                          
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseOneOf")>]
        member this.ChooseOneOf (candidates: #seq<_>) =
            (this.ChooseSeveralOf (candidates, 1u)).[0]
            
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("Shuffle")>]
        member this.Shuffle (items: #seq<_>) =
            Algorithms.RandomShuffle (items,
                                      this)




        

        

        
        


