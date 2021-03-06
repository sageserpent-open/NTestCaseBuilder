﻿namespace SageSerpent.Infrastructure

    open System.Collections
    open System.Collections.Generic
    open System

    [<CustomComparison; StructuralEquality>]
    type Unbounded<'X> when 'X: comparison=
        Finite of 'X
      | PositiveInfinity
      | NegativeInfinity

        interface IComparable with
            member this.CompareTo another =
                    (this :> IComparable<Unbounded<'X>>).CompareTo (another :?> Unbounded<'X>)

        interface IComparable<Unbounded<'X>> with
            member this.CompareTo another =
                match this
                      , another with
                    Finite lhsUnlifted
                    , Finite rhsUnlifted ->
                        compare lhsUnlifted rhsUnlifted
                  | PositiveInfinity
                    , PositiveInfinity ->
                        0
                  | NegativeInfinity
                    , NegativeInfinity ->
                        0
                  | _
                    , PositiveInfinity ->
                        -1
                  | NegativeInfinity
                    , _ ->
                        -1
                  | PositiveInfinity
                    , _ ->
                        1
                  | _
                    , NegativeInfinity ->
                        1