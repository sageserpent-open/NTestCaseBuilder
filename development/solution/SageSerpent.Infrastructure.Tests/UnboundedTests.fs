namespace SageSerpent.Infrastructure.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure

    [<TestFixture>]
    type UnboundedTestFixture() =
        let fortyFive =
            Finite 45

        let negativeInfinity =
            NegativeInfinity

        let positiveInfinity =
            PositiveInfinity

        let twentyThree =
            Finite 23

        let wrap x =
            Finite x


        [<Test>]
        member this.TestFinitesAndInfinitesInCombination () =
            Assert.IsTrue (negativeInfinity < fortyFive)

            Assert.IsTrue (negativeInfinity < Finite 45)

            Assert.IsTrue (NegativeInfinity < fortyFive)

            Assert.IsTrue (NegativeInfinity < Finite 45)

            Assert.IsTrue ((Finite 45) > negativeInfinity)

            Assert.IsTrue ((Finite 45) > NegativeInfinity)

            Assert.IsTrue (NegativeInfinity < wrap 45)

            Assert.IsTrue((wrap 45) > NegativeInfinity)

            Assert.IsTrue (NegativeInfinity < twentyThree)

            Assert.IsTrue (positiveInfinity > fortyFive)

            Assert.IsTrue (positiveInfinity > Finite 45)

            Assert.IsTrue (PositiveInfinity > fortyFive)

            Assert.IsTrue (PositiveInfinity > Finite 45)

            Assert.IsTrue((Finite 45) < positiveInfinity)

            Assert.IsTrue((Finite 45) < PositiveInfinity)

            Assert.IsTrue (PositiveInfinity > wrap 45)

            Assert.IsTrue((wrap 45) < PositiveInfinity)

            Assert.IsTrue (PositiveInfinity > twentyThree)

        [<Test>]
        member this.TestFinites() =
            Assert.IsTrue (negativeInfinity < twentyThree)

            Assert.IsTrue (twentyThree < fortyFive)

            Assert.IsTrue (not (twentyThree > fortyFive))

            Assert.IsTrue((Finite 23) = twentyThree)

        [<Test>]
        member this.TestInfinites() =
            let shouldBeTrue =
                NegativeInfinity = NegativeInfinity
            Assert.IsTrue shouldBeTrue

            Assert.IsTrue (not (NegativeInfinity > NegativeInfinity || NegativeInfinity < NegativeInfinity))
            Assert.IsTrue (NegativeInfinity <= NegativeInfinity && NegativeInfinity >= NegativeInfinity)

            let shouldBeTrue =
                PositiveInfinity = PositiveInfinity
            Assert.IsTrue shouldBeTrue

            Assert.IsTrue (not (PositiveInfinity > PositiveInfinity || PositiveInfinity < PositiveInfinity))
            Assert.IsTrue (PositiveInfinity <= PositiveInfinity && PositiveInfinity >= PositiveInfinity)

            Assert.IsTrue (NegativeInfinity <> PositiveInfinity)
            Assert.IsTrue (NegativeInfinity < PositiveInfinity)
            Assert.IsTrue (PositiveInfinity > NegativeInfinity)
