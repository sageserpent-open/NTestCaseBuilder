NTestCaseBuilder, for .NET Testing
==================================

This is a .NET library that generates sets of test cases, for use by parameterised tests.

Think of it as being a DSL for making 'object mothers', whose generated data can be used as test cases (or for any other purpose, for that matter).

Each test case is built up progressively from smaller pieces of data that are combined together.

The smallest pieces are referred to as 'test variables'. Each test variable is constrained to take values from its own set of allowed 'levels'.

Changing each test variable independently to a new level produces a new test case.

If a test requires a certain 'strength' of coverage of combinations of levels from different test variables, the library produces a stream of test cases that honour this guarantee while avoiding getting swamped with a combinatoric explosion of test cases.

If a parameterised test fails with an exception for a specific test case, the library creates a signature that allows it to reproduce the failing test case immediately in a separate test; this is to aid debugging.

The sources are written in F#, but the API can be used just as comfortably from C# as from F#.


Some background and a full worked example are given below after the following sample. A note on a related .NET utility, Pex, is also provided at the towards the end along with a link to a thought-provoking article relevant to this library.

Chapters
--------

NTestCaseBuilder, for .NET Testing

Chapters

Use It

Short Sample

Longer Sample

License

Background

Walk me through an example!

Advanced Stuff: Deferrals

More Advanced Stuff: Filters

Stuff I didn't Document

How do I install this thing?

A Thought-Provoking Article You Should Read

Can this possibly be improved?

Use It
------

NTestCaseBuilder is available as NuGet binary package over at:  [NTestCaseBuilder](http://www.nuget.org/packages/NTestCaseBuilder/ "NTestCaseBuilder")

Install it at project level - it will add references to the NTestCaseBuilder assembly and its dependencies.

NTestCaseBuilder.Samples is a standard NuGet source samples package at: [NTestCaseBuilder.Samples](http://www.nuget.org/packages/NTestCaseBuilder.Samples/ "NTestCaseBuilder.Samples")

Install it at project level - it will create (if necessary) a Samples\NTestCaseBuilder directory and solution folder for some snippets of source code that can be built and run.

**NOTE:** for both NuGet packages, C5 is installed as a dependency of NTestCaseBuilder; you will need to remove either the C5.Mono.dll or the C5.dll assembly before you can build your project, as the C5 NuGet package installs both by default.

Short Sample
------------

Let's build some test strings to feed to a *very* simple calculator.

This calculator can do arithmetic using binary operators - addition, subtraction, multiplication and division.

It can't do unary negation yet and it doesn't understand operator precedence either - read on and you'll see a more complete example, but for now let's keep it nice and easy.

So we can hit it with:-

	"2"					==> "2"
	"2 + 1"				==> "3"
	"0 - 2"				==> "-2"
	"1 + (2 * 2)"		==> "5"

Rather than get distracted with implementing the calculator and verifying expectations, let's just print out the test strings...

    [TestFixture]
    internal class TestBinaryOperatorExpressions
    {
        private static readonly ITypedFactory<Char> BinaryOperatorFactory =
            TestVariable.Create(new[] {'+', '-', '*', '/'});

        private static readonly ITypedFactory<String> ConstantFactory =
            TestVariable.Create(new[] {"0", "1", "2"});

        private static ITypedFactory<String> BuildExpressionFactoryRecursively()
        {
            var subexpressionFactory =
                Interleaving.Create(new[]
                {
                    ConstantFactory,
                    Synthesis.Create(Deferral.Create(BuildExpressionFactoryRecursively),
                        expression => String.Format("({0})", expression))
                });

            var binaryOperatorExpressionFactory = Synthesis.Create(subexpressionFactory, BinaryOperatorFactory,
                subexpressionFactory,
                (lhsOperand, binaryOperator, rhsOperand) =>
                    String.Format("{0} {1} {2}", lhsOperand, binaryOperator, rhsOperand));

            return Interleaving.Create(new[] {ConstantFactory, binaryOperatorExpressionFactory});
        }

        [Test]
        public void FireUpBinaryOperatorExpressions()
        {
            const Int32 maximumDepth = 2;

            var expressionFactory = BuildExpressionFactoryRecursively().WithDeferralBudgetOf(maximumDepth);

            const Int32 strength = 2;

            var numberOfTestCasesExercised =
                expressionFactory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                    (testCase => Console.Out.WriteLine(testCase)));
            Console.Out.WriteLine("Exercised {0} test cases.", numberOfTestCasesExercised);
        }
    }

Running this will print out progressively more complex test strings:-

	0 + 0
	2 * 1
	1 / 1
	2 - 1
	0 / 1
	1 - 0
	0 * 2
	2 / 0
	0 - 2
	1 / 2
	1 + 1
	1 * 0
	2 + 2
	0
	1
	2
	0 + (1 - 2)
	(1 / 2) - 2
	(2 + 0) + (0 - 0)
	0 * (2 / 1)
	(0 - 1) + (2)
	(0) * (0 - 1)
	etc...
	
Look carefully at the start - you will see that for any combination of the left hand number and the operator, or the operator and the right hand number, or the left and right hand number, the generated sequence *covers* it. This is because we set the *strength* to be 2 - so any pair of items that can be combined in our resulting test case will be covered.

Note how the sequence gets more complex overall, but jumps around various possibilities within each band of complexity - NTestCaseBuilder tries to decorrelate test cases so that they **don't clump together like this...**

	0
	1
	2
	0 + 0
	0 + 1
	0 + 2
	0 * 0
	0 * 1
	0 * 2
	
... you can see why this might not be a good idea if we are waiting to see if *1 / 0* blows up - and consider the likes of *2 / (2 / (1 - 1))*!
	

Longer Sample
-------------

Let's thoroughly test a couple of sorting algorithms.

First, the algorithms to test - I'll cheat and just wrap up some existing functionality:-

    /// <summary>
    ///     Module for holding static methods used to do sorting.
    /// </summary>
    public static class SortingAlgorithmModule
    {
        /// <summary>
        ///     Sorts a sequence of items into ascending order, using the intrinsic ordering of the TItem type.
        ///     Has a bug in its implementation - can you spot it?
        /// </summary>
        /// <typeparam name="TItem">Any type with an intrinsic ordering given by implementing IComparable&lt;TItem&gt;.</typeparam>
        /// <param name="unsorted">Sequence of items to be sorted. This is left unchanged by the call.</param>
        /// <returns>The items sorted into ascending order, as a new collection.</returns>
        public static IEnumerable<TItem> SortWithBug<TItem>(IEnumerable<TItem> unsorted)
            where TItem : IComparable<TItem>
        {
            return new SortedSet<TItem>(unsorted);
        }

        /// <summary>
        ///     Sorts a sequence of items into ascending order, using the intrinsic ordering of the TItem type.
        /// </summary>
        /// <typeparam name="TItem">Any type with an intrinsic ordering given by implementing IComparable&lt;TItem&gt;.</typeparam>
        /// <param name="unsorted">Sequence of items to be sorted. This is left unchanged by the call.</param>
        /// <returns>The items sorted into ascending order, as a new collection.</returns>
        public static IEnumerable<TItem> SortThatWorks<TItem>(IEnumerable<TItem> unsorted)
            where TItem : IComparable<TItem>
        {
            var result = new List<TItem>(unsorted);
            result.Sort();
            return result;
        }
    }

OK, great. Now for the test:-

    /// <summary>
    ///     Test fixture for class 'SortingAlgorithmModule'.
    /// </summary>
    [TestFixture]
    public class TestSortingAlgorithm
    {
        /// <summary>
        ///     A test case to apply sorting to. Provides a sequence of integers in some unspecified order
        ///     - may or may not be sorted in ascending order. Some of the integers in the sequence may be
        ///     duplicated; the duplicates may or may not be adjacent to each other.
        ///     The sequence is generated by permuting a sequence of integers that is known by construction
        ///     to be monotonic increasing, with any duplicates arranged into runs of adjacent duplicated
        ///     values. This base sequence is also made available to check the expected results from any sorting
        ///     algorithm.
        /// </summary>
        public class TestCase
        {
            /// <summary>
            ///     Constructor for use by synthesizing factory.
            /// </summary>
            /// <param name="leastItemInSequence">
            ///     The lowest value that starts off <cref>OriginalMonotonicIncreasingSequence</cref>
            /// </param>
            /// <param name="nonNegativeDeltas">
            ///     Sequence of non-negative deltas that will be used to build up
            ///     <cref>OriginalMonotonicIncreasingSequence</cref>
            /// </param>
            /// <param name="permutation">
            ///     A permutation that is used to shuffle <cref>OriginalMonotonicIncreasingSequence</cref> to give
            ///     <cref>PermutedSequence</cref>
            /// </param>
            public TestCase(Int32 leastItemInSequence, IEnumerable<Int32> nonNegativeDeltas,
                Permutation<Int32> permutation)
            {
                var originalMonotonicIncreasingSequence = new List<Int32>();

                var runningSum = leastItemInSequence;

                foreach (var nonNegativeDelta in nonNegativeDeltas)
                {
                    originalMonotonicIncreasingSequence.Add(runningSum);
                    runningSum += nonNegativeDelta;
                }

                originalMonotonicIncreasingSequence.Add(runningSum);

                OriginalMonotonicIncreasingSequence = originalMonotonicIncreasingSequence;

                PermutedSequence = permutation(originalMonotonicIncreasingSequence);
            }

            /// <summary>
            ///     Parameterless constructor that represents the trivial empty sequence case.
            /// </summary>
            public TestCase()
            {
                OriginalMonotonicIncreasingSequence = new List<Int32>();

                PermutedSequence = new List<Int32>();
            }

            /// <summary>
            ///     The sequence to be used as input to a sorting algorithm.
            /// </summary>
            public IEnumerable<Int32> PermutedSequence { get; set; }

            /// <summary>
            ///     The expected result of sorting <cref>PermutedSequence</cref>.
            /// </summary>
            public IEnumerable<Int32> OriginalMonotonicIncreasingSequence { get; set; }
        }

        private static ITypedFactory<TestCase> BuildTestCaseFactory()
        {
            var factoryForLeastItemInSequence = TestVariable.Create(Enumerable.Range(-3, 10));

            const int maximumNumberOfDeltas = 4;

            var factoryForNonNegativeDeltasAndPermutation =
                Interleaving.Create(from numberOfDeltas in Enumerable.Range(0, 1 + maximumNumberOfDeltas)
                    select BuildNonNegativeDeltasAndPermutationFactory(numberOfDeltas));

            var testCaseFactoryForTrivialCase = Singleton.Create(new TestCase());

            var testCaseFactoryForNonTrivialCases = Synthesis.Create(factoryForLeastItemInSequence,
                factoryForNonNegativeDeltasAndPermutation,
                (leastItemInSequence, nonNegativeDeltasAndItsPermutation) =>
                    new TestCase(leastItemInSequence, nonNegativeDeltasAndItsPermutation.Item1,
                        nonNegativeDeltasAndItsPermutation.Item2));

            return
                Interleaving.Create(new[] {testCaseFactoryForTrivialCase, testCaseFactoryForNonTrivialCases});
        }

        private static ITypedFactory<Tuple<IEnumerable<Int32>, Permutation<Int32>>>
            BuildNonNegativeDeltasAndPermutationFactory(int numberOfDeltas)
        {
            var factoryForNonNegativeDelta =
                TestVariable.Create(from signedDelta in Enumerable.Range(0, 5) select signedDelta);
            return
                Synthesis.CreateWithPermutation<Int32, Int32>(Enumerable.Repeat(factoryForNonNegativeDelta,
                    numberOfDeltas));
        }

        /// <summary>
        ///     Parameterised unit test for <cref>SortingAlgorithmModule.SortWithBug</cref>.
        /// </summary>
        /// <remarks>
        ///     This is expected to fail.
        /// </remarks>
        /// <param name="testCase"></param>
        public static void
            ParameterisedUnitTestForReassemblyOfPermutedMonotonicIncreasingSequenceByBuggySortingAlgorithm(
            TestCase testCase)
        {
            Console.WriteLine("[{0}]", String.Join(", ", testCase.PermutedSequence));

            var sortedSequence = SortingAlgorithmModule.SortWithBug(testCase.PermutedSequence);

            Assert.IsTrue(sortedSequence.SequenceEqual(testCase.OriginalMonotonicIncreasingSequence));
        }

        /// <summary>
        ///     Parameterised unit test for <cref>SortingAlgorithmModule.SortThatWorks</cref>.
        /// </summary>
        /// <remarks>
        ///     This is expected to succeed.
        /// </remarks>
        /// <param name="testCase"></param>
        public static void
            ParameterisedUnitTestForReassemblyOfPermutedMonotonicIncreasingSequenceByCorrectSortingAlgorithm(
            TestCase testCase)
        {
            Console.WriteLine("[{0}]", String.Join(", ", testCase.PermutedSequence));

            var sortedSequence = SortingAlgorithmModule.SortThatWorks(testCase.PermutedSequence);

            Assert.IsTrue(sortedSequence.SequenceEqual(testCase.OriginalMonotonicIncreasingSequence));
        }

        /// <summary>
        ///     Unit test for <cref>SortingAlgorithmModule.SortWithBug</cref>.
        /// </summary>
        [Test]
        public void TestReassemblyOfPermutedMonotonicIncreasingSequenceByBuggySortingAlgorithm()
        {
            var factory = BuildTestCaseFactory();
            const Int32 strength = 3;

            var howManyTestCasesWereExecuted = factory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                ParameterisedUnitTestForReassemblyOfPermutedMonotonicIncreasingSequenceByBuggySortingAlgorithm);

            Console.WriteLine("Executed {0} test cases successfully.", howManyTestCasesWereExecuted);
        }

        /// <summary>
        ///     Unit test for <cref>SortingAlgorithmModule.SortWithBug</cref>.
        /// </summary>
        [Test]
        public void TestReassemblyOfPermutedMonotonicIncreasingSequenceByCorrectSortingAlgorithm()
        {
            var factory = BuildTestCaseFactory();
            const Int32 strength = 3;

            var howManyTestCasesWereExecuted = factory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                ParameterisedUnitTestForReassemblyOfPermutedMonotonicIncreasingSequenceByCorrectSortingAlgorithm);

            Console.WriteLine("Executed {0} test cases successfully.", howManyTestCasesWereExecuted);
        }

        /// <summary>
        ///     Reproduce the test failure from
        ///     <cref>TestReassemblyOfPermutedMonotonicIncreasingSequenceByBuggySortingAlgorithm</cref>.
        /// </summary>
        [Test]
        public void TestThatQuicklyReproducesTheFailureFromTheBuggyTest()
        {
            const string reproduction =
                // This is cut and paste from the exception thrown by test TestReassemblyOfPermutedMonotonicIncreasingSequenceByBuggySortingAlgorithm.
                "1090764779116129690923515858308014520222336185700694896976936400046940578111983112055989629000774433035533486068550533022050440563758532034744094390335385597493640149399285518641151929556092665584402288546355440347730368088836771627466259556412021922627725184177788154110107097070732172385860911454891134069325355752405254360949593534879521837273025056195093106165870371041268817411831127077924824920514034566098697648181040760873045194504973952223951724813225504499983047118862663287945210705883307688057054025916951667043265570775146944395249011036178056877575401364350157809719147995239162387828809111641197498761314743201387695694648950056523648142393681967482016898244952046671875614901026969586459303447635896866101123266210751713646802033809707609168906792115491183477974610728150992273068596408688241773251963923138429130520691098243248893419892952443542052191199628809010572675447824551593304497482463813697355082304187386823950217095809276435625749534789299526459222114593592266686209126390984795821501381781745708132091435924939833210114218265084665947358428911669581585498376927407493552466725446760623701467939560074187223831230919745620371755213508831636067167684360039211127262835836349843341641597785732280357370537268744363822886739938694773173734590922835035954079750112107318950146026524038408134904444586730532951131436902225568662170090521600282819666798575526991783952385901520151082949249355302351810143145633498325010249964328245271752986380701194391795177417832062797941842182732805832732335710897274060061770940391780122474565229191756062364516816599581273317348176499228496927992511523072853252075332731890715264392205033020620778478135070528984576870458536756429551045411434100804134681784802530326234271197330772618947349597030925327266034680958734216931873495784114303377681612871232138727564369215981205645526032669238082470024868794218096297605262460037102638172004995808152171391862902260941117558050337516270511463749491604762977109930535373246706986172899428371745042253317918107906121040873486737512994890357293396689164960629846996988387599881819260138835140665303070639472747816265470162847876957484029766979691843665957532773395930568939163631862895529691637676354435952393085610138558008072722760071077361553157249091500252449648886446114577130960581145642226501720172722101650580698819067818352781376634324409182402883078422358854325278360944098195948662827735052082866490169948823637943612256334313998270759439470885372690432";

            var factory = BuildTestCaseFactory();

            factory.ExecuteParameterisedUnitTestForReproducedTestCase(
                ParameterisedUnitTestForReassemblyOfPermutedMonotonicIncreasingSequenceByBuggySortingAlgorithm,
                reproduction);
        }
    }

License
-------

The MIT License (MIT)

Copyright (c) 2014 Gerard Murphy, SageSerpent Ltd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Background
----------

Tests come in different shapes and sizes - unit tests, integration tests, smoke tests for example.

Learning to write tests takes practice and has different goals at different stages.

A common path is to start with the the basics of writing one-off smoke tests; these simply run some code without any test expectations at all:-

	///<summary>
	/// NUnit smoke test.
	///</summary>
	[Test]
	public void TestThatTheComputerDoesntCatchFire()
	{
		var componentUnderTest = new Foo();

		componentUnderTest.DoThis();

		componentUnderTest.DoThat();

		var wasted = componentUnderTest.Bar;	// Hopefully the smoke alarm is still quiet after this executes.
	}

This progresses to writing unit tests that do have expectations, but which are really just 'executable documentation' for the API of the system under test:-

	///<summary>
	/// NUnit unit test serving as documentation of behaviour.
	///</summary>
	[Test]
	public void TestWithSomeNaivelyCodedTestExpectations()
	{
		var componentUnderTest = new Foo();

		Assert.IsFalse(componentUnderTest.Bar.IsOpen);

		componentUnderTest.DoThis();

		Assert.IsTrue(componentUnderTest.Bar.IsOpen);
		Assert.IsFalse(componentUnderTest.Bar.IsAtLastOrders);

		componentUnderTest.DoThat();

		Assert.IsTrue(componentUnderTest.Bar.IsOpen);
		Assert.IsTrue(componentUnderTest.Bar.IsAtLastOrders);
	}

I've avoided the use of mocking and fluent testing above to keep things simple. The important point is that this style of test is generally not sufficient for a statement of correctness of the code; rather, its purpose is to illustrate the API of the system under test by way of example.

Once you've read this test, you know:-

1. The existence of the 'DoThis()' and 'DoThat()' methods.
2. The existence of the property 'Bar'.
3. The 'Bar' instance is either created by or somehow retrieved by the 'Foo' instance, at least when using the parameterless constructor for 'Foo'.
4. The 'DoThis' opens up the 'Bar' for business.
5. The 'DoThat' prepares the 'Bar' for closing.

What you don't know is whether all 'Foo' instances have their own 'Bar' instances - perhaps they share them?
You don't know anything about constructing 'Foo' any other way - can we supply a 'Bar'? What if the 'Bar' is already open?
What happens if we call 'DoThat()' without calling 'DoThis()' first. A no-operation? Precondition exception thrown, perhaps?
Can we cycle back by calling 'DoThis()' *after* 'DoThat()'? Thereby causing a lock-in at the bar, our British readers might expect?

Now, one can (and hopefully does) write additional unit tests in this style to explore the various edge-cases and flesh out the parts of the behaviour that *must* be specified to client code. (Remember, it may be the case that client code doesn't care about all of these edge cases, although in this situation it is better to design the API so as to make these irrelevant cases impossible to exercise).

However, these tests are really about documenting behaviour - while they *do* act as a basic safety net against obvious breakage of the component as the implementation is refactored, extended or otherwise modified, they aren't good enough to consider signing off against.


There is also another problem in that this style of unit test doesn't scale well as we move away from the lowest level of components that form the roots of an application's software component dependencies, proceeding up the dependency chains to test components that are composed of, or otherwise rely on, simpler components.

It becomes progressively more difficult to set up the component under test because of the increasing number of dependency components that have to be correctly set up for the component under test to run. In effect, a unit test for a higher-level component is a kind of integration test for that component's dependencies.

The traditional solution to this to mock the dependency components, which ensures that the higher-level component really is being unit-tested. This approach is well and good, and can also point the way to effective design of the dependencies between an application's components.

What is doesn't do however, is to provide confidence that the higher-level component is being genuinely tested - most tests using mocks tend to exaggerate the problem above, where only a specific corner of the component's behaviour is tested. Because most mocks rely on canned recordings of method calls (for behavioural mocks) or have deliberately trivial state (for state-based mocks), any variation in behaviour due to the component's dependencies is swept under the carpet.

So mocks are good for being able to quickly write 'executable documentation' unit tests for higher-level components, and they are good for highlighting a path to expressing dependencies in an application's design. They are however found wanting when it comes to using tests to help prove correctness.


This leads to the next stage, which is to write parameterised unit tests...

A parameterised unit test is written as a function with a single parameter passed to it: the parameter contains all the information needed to completely set up both the component under test and the sequence of actions to be performed on it. This also includes a prescription for setting up any component dependencies required by the component under test.

The idea is to repeatedly run the function with variations of the parameter in order to flush out breakages of the unit test that only occur for some state of the component under test, or the dependency components that it is built from / interacts with in the test, or for some specific variation of the sequence of operations performed by the test.

Needless to say, such a parameter is going to be a complex object for it to be able to carry all of this information.

So for the example above, our parameter object would describe:-

1. Whether or not the 'Foo' is created with a 'Bar'.
2. If the latter, whether the 'Bar' is initially open or not. If open, whether it is at last orders or not.
3. The sequence of operations applied to the 'Foo' - for each stage in the sequence, whether to call either 'DoThis', or 'DoThat' and then carry on to a following operation, or alternatively to simply finish the test.

Omitting some of the detail, the example above becomes the following code snippet:-

	///<summary>
	/// NUnit unit test driving a parameterised unit test.
	///</summary>
	[Test]
	public void TestUsingParametricApproach()
	{
		var sequenceOfTestCases = TestCasesSequence();

		foreach (var testCase in sequenceOfTestCases)
		{
			TestUsingParameter(testCase);
		}
	}

	enum State
	{
		Closed,
		NormalBusiness,
		LastOrders
	}

	enum Operation
	{
		DoThis,
		DoThat
	}

	private static void TestUsingParameter(TestCase testCase)
	{
		foreach (var operation in testCase.Operations)
		{
			var stateBeforeOperationIsApplied = StateDueTo(testCase);

			switch (stateBeforeOperationIsApplied)
			{
				case State.Closed:
					Assert.IsFalse(testCase.Foo.Bar.IsAtLastOrders);	// 'Last orders' is in a sense
																		// a sub-state of being open.

					switch (operation)
					{
						case Operation.DoThis:
							testCase.Foo.DoThis();
							Assert.IsTrue(StateDueTo(testCase) == State.NormalBusiness);
							break;

						case Operation.DoThat:
							Assert.Throws<System.Exception>(() => testCase.Foo.DoThat());
							Assert.IsTrue(StateDueTo(testCase) == stateBeforeOperationIsApplied);
							break;
					}

					break;

				case State.NormalBusiness:
					switch (operation)
					{
						case Operation.DoThis:
							Assert.Throws<System.Exception>(() => testCase.Foo.DoThis());
							Assert.IsTrue(StateDueTo(testCase) == stateBeforeOperationIsApplied);
							break;

						case Operation.DoThat:
							testCase.Foo.DoThat();
							Assert.IsTrue(StateDueTo(testCase) == State.LastOrders);
							break;
					}

					break;

				case State.LastOrders:
					switch (operation)
					{
						case Operation.DoThis:
							testCase.Foo.DoThis();
							Assert.IsTrue(StateDueTo(testCase) == State.NormalBusiness);
							break;

						case Operation.DoThat:
							Assert.Throws<System.Exception>(() => testCase.Foo.DoThat());
							Assert.IsTrue(StateDueTo(testCase) == stateBeforeOperationIsApplied);
							break;
					}

					break;
			}
		}
	}

	private static State StateDueTo(TestCase testCase)
	{
		return testCase.Foo.Bar.IsOpen
				   ? (testCase.Foo.Bar.IsAtLastOrders ? State.LastOrders : State.NormalBusiness)
				   : State.Closed;
	}

	private IEnumerable<TestCase> TestCasesSequence()
	{
		throw new NotImplementedException();
	}

	private class TestCase
	{
		public Foo Foo { get; private set; }
		public IEnumerable<Operation> Operations { get; private set; }
	}

Our parameterised unit test in this example has become a state transition machine, relying on the parameter object to provide the initial state of the 'Foo' instance (possibly from a pre-supplied 'Bar' object), as well as a sequence of operations to apply to it.

It is easy to realise that parameter objects can become quite complex very quickly, especially when the component under test is high-level, because of the need to specify all of the dependency components.

To take the high road of writing parameterised unit tests requires two lots of effort - the first is to build the parameterised test function itself, the second to supply test cases to repeated calls to that function.

The first task is essentially specific to the component under test and in my experience, at least on a par with implementing the component itself. Consequently, NTestCaseBuilder doesn't pretend to automate this part - writing a test is an activity of some sophistication that is for the author of the component's specification to think carefully about and realise manually.

The second task is what NTestCaseBuilder addresses.

### Technical Terms ###

Let's go back to our 'Foo' test example.

We have a *test case* - the data that parameterises our *parameterised unit test*.

Each test case has to be built up from smaller pieces of data. Here, we have a breakdown of:-

	TestCase
			(Foo

			OR

			Foo
					Bar)

			AND

			sequence of
					Operation - either a DoThis or a DoThat

We say that a TestCase is *synthesized* from a Foo and a sequence of Operation instances - this synthesis might be a plain constructor call, passing the two items as parameters, or might be some more involved process requiring the subsequent setting of properties.

How is the 'Foo' instance built up? We have a choice here - we could use the parameterless constructor for 'Foo', or perhaps build up our own 'Bar' instance and supply it to an alternative constructor for 'Foo' (not shown in the code snippet above). So to cover both situations, we need to express an *interleaving* where a 'Foo' can be built in more than one way.

Looking at the sequence of 'Operation' instances, the sequence itself can be built up by taking a fixed number of operations and synthesizing a sequence from them. For now, let's fix this at 4 operations; we will see later how to loosen this rather arbitrary choice to get sequences of varying length.

This leaves the operations themselves - for each of the 4 slots in the sequence, there is a choice of whether the operation is a 'DoThis' or a 'DoThat' enumeration constant. In effect, each slot is a *test variable* that can be set to one of two distinct *levels*.

How do we make our alternate 'Foo' instances? One way is to call the parameterless constructor for 'Foo'. In this case, there is nothing to vary - we just get a 'Foo' without being able to play with its configuration: we can think of this as being a data *singleton*, because any overall test case that uses one of these 'Foo' instances should get the same value of 'Foo' each time (we assume that 'Foo' is well-behaved, so successive calls to the parameterless constructor yield what are effectively copies of the same object, sharing the same subsequent behaviour).

Note that unlike the usual usage of the work 'singleton', we do not insist that the singleton object actually be the same object reference each time - merely we that can't get any initial variation when we ask for one.

The other way of making a 'Foo' is to supply a 'Bar' - if we have a constructor (again, not shown in the code snippet above) for 'Bar' that takes a parameter describing whether the 'Bar' is closed, doing normal business or is taking last orders, then we could represent this as a synthesis of a 'Foo' from a 'Bar' which in turn is synthesized from a test variable with three levels.

Taking these italicised terms and applying them to the breakdown above yields a conceptual tree:

	Synthesis (of a TestCase)
		-	Interleaving (of a Foo)
				-	Singleton (of a Foo created with the parameterless constructor)
				-	Synthesis (of a Foo)
						-	Synthesis (of a Bar)
								-	Test Variable (of the 'Bar' constructor parameter with 3 levels
												   - closed, normal business and taking last orders)
		-	Synthesis (of a sequence of Operation)
				- 	TestVariable (of an 'Operation' with two levels - execute 'DoThis()' and execute 'DoThat()')
				-	TestVariable (ditto)
				-	TestVariable (ditto)
				-	TestVariable (ditto)

NTestCaseBuilder realises such conceptual trees as trees of *test case factories* - each factory can be either a *synthesizing factory*, an *interleaving factory*, a *test variable factory* or a *singleton factory*.

A factory has two roles:-

1. It produces a sequence of varying test cases, for direct consumption in a parameterised unit test. This is what we see in the example above for 'TestCase'.

2. It can participate in the composite design pattern, becoming part of a larger tree of factories whose root produces more complex test cases. We see this above for all the subtree nodes.


Moving on, let's look at the test variables that contribute to the making of a 'TestCase' above - we have 4 test variables * 2 levels for the operation slots, with an additional 1 test variable * 3 levels for the parameterised 'Bar' constructor. Note that the singleton doesn't count - as it always yields a non-varying data value, there is essentially no test variable for it.

That means there are potentially 2^4 * (1 + 3) = 64 different test cases to produce. If you're paying attention, you'll note that I added in the '1 +' in the calculation - this is because we have an interleave that contributes 3 possible variable levels via one alternative, and 1 singleton via the other. So although singletons don't count as test variables, in this case we still get a distinct contribution to be counted in because of the parent interleave.

That wasn't really a complex example - one can realistically imagine, say 20 test variables with 5 levels feeding into a synthesis - meaning 5^20 = 95367431640625 test cases would result.

That's rather a lot of test cases - do we really need all of them?


Let's think about how likely a bug is going to be manifest when we run our parameterised test repeatedly over all 95367431640625 test cases. Let's say that we've already fixed the low-hanging bugs that occur straight away for just about any choice of test case.

When I say 'low-hanging bugs', I mean things like:-

1. Forgetting to implement the stubbed code you wrote when you were coding 'test-first' in a language with static typing - or just plain forgetting to write the code if your language uses dynamic typing.
2. The division by zero one of your colleagues put in for a joke while you took a comfort break.
3. The off by one error in a loop that was definitely your fault.
4. The lack of initialisation that the compiler warned you about, but of course, we all know better than to trust diagnostic advice from a compiler.
5. The silly mistake in the implementation that was caused by a cosmic ray hitting a brain cell while typing a line of code.

Typically, a smoke test will expose most of these kinds of errors (and this is why it's still worth writing smoke tests in the early stages of implementation, assuming you haven't written any kind of test with expectations).

So at this point we have an implementation for our component under test that seems to work - it would certainly pass a smoke test coded in the style shown right at the beginning, and would probably survive an 'executable-documentation' unit test too.

We then expect that our parameterised unit test will be repeatedly called with each new test case, and will repeatedly succeed, until at some point - **KERBOOM**: test failure!

So the failing test case contains a magic combination of levels for its test variables that cause the component to fail. Is it likely that **all** of the test variables contribute to the failure? All 20 of them?

It's safe to assume - at least as a starting point - that maybe only one, two, three or four test variables contribute the magic combination of levels that cause the failure. All of the other test variables could have their levels set to whatever choices we'd like and we'd still get a failure.

So we could thin out the number of test cases considerably - say we had a failure requiring a specific combination of levels from four test variables - we could find the failing test case within 5^4 = 625 repeats at most - much better!

The problem is that we don't know which of the 20 test variables are the ones that can act in concert to expose the bug.

However, imagine that we could generate a sequence of 'incomplete' test cases using only the first four test variables - that would give us 625 test cases with holes in them where we haven't yet decided what levels to set the remaining test variables to.

Perhaps we could then build another parallel sequence of incomplete test cases using the next four test variables - again that would give is 625 test cases with different holes in them; these holes would include the test variables that we filled in in the previous sequence (and vice-versa).

So merging the two sequences of test cases together would provide a new sequence of test cases that would include all combinations of test levels from the first four test variables and all combinations of test levels from the next four test variables, with fewer holes. Because we merge corresponding test cases from the two sequences, the resulting sequence length is still 625!

Carrying on with this procedure would yield a sequence of 625 test cases that would cover combinations of test levels from successive groups of four test variables - so for the much lower effort of examining up to 625 test cases, we have some chance of finding the same bug that we would otherwise have had to trawl through 95367431640625 test cases to find!

Great - but there is still a problem. I forgot to mention that possibility that it might be combinations of levels from test variables from *across the groups of four* that cause the bug. In other words, maybe it is test variable #2 with level #5, test variable #9 with level #3, test variable #18 with level #3 and test variable #20 with level #1 that exposes the bug.

The procedure above is only systematic about combinations of levels from test variables in the same group, so it won't necessarily generate the magic test case; but it is a tantalising idea: could we somehow produce a much smaller sequence of test cases than the full 95367431640625, and still guarantee that *for any choice of one, two, three or four test variables*, the sequence would *guarantee that every combination of levels for those variables* would be completely covered.

Think about that last statement - the guarantee isn't merely that there is *some* choice of test variables whose level combinations are covered - it is saying that *any* choice of test variables you ask for has its level combinations covered; from within the same sequence of test cases.

This is the guarantee provided by any of the factories mentioned above - if given a *strength*, a factory will produce a sequence of test cases that provides that guarantee for any number of test variables in combination up to and including the given strength.

As the strength is increased, the factory has to work harder to meet this guarantee - so if we set the strength all the way up to 20 in this example, we are back to generating all 95367431640625 test cases. In practice, strengths of up to 4 are probably good enough.

Note that when we use factories as part of the composite design pattern, there is no obvious relationship between the sequences of simpler test cases produced by factories in subtrees and the sequence of complex test cases produced by the overall root factory. This is because for a given strength, as we start combining more and more subtrees together, there are an increasing number of opportunities for combinations of test variable levels to be overlaid into the same test case - there are more test variables relative to the strength, therefore holes to fill in, therefore more opportunities to pack things together.

The exception is when we ask for the full strength including all test variables and we only use synthesizing factories: in this case we can think of the sequence of the test cases made by the root factory as being a cross-product of the levels taken from all the test variables at the leaves of the tree of factories.

However, we can say that the closer to the root factory we go, the more test cases in the sequence we get; fortunately, the number of test cases gets increasingly 'thinner' taken relative to the full cross-product of levels taken from all test variables. The amount of thinning as we go up the tree is more dramatic for lower strengths.


So we've met the players - to recap, we have:-

1. A test case: structured data that drives a parameterised unit test, composed out of simpler pieces.

2. A test variable: the simplest piece of data that can show variation between one test case and another.

3. Levels: these are the values that a test variable is allowed to take.

4. A singleton: the simplest piece of data that can never vary from one test case to another.

5. Factories: these produce sequences of test cases that guarantee coverage of combinations of test variable levels according to a given strength. These can be combined according to the composite design pattern to make a tree structure.

6. A synthesizing factory: puts together the kinds of test cases produced by its child factories to make more complex test cases. It should be an interior node in any tree of factories.

7. An interleaving factory: interleaves the kinds of test cases produced by its child factories to give alternatives in its sequence. It should be an interior node in any tree of factories.

8. A test variable level factory: introduces a test variable and its levels into a factory tree. It is always a leaf node in any tree of factories.

9. A singleton factory: introduces a singleton data value into a factory tree. Does not affect the strength guarantee because it has no test variable - which is why it is preferable to making a faux test variable factory with a single level. It is always a leaf node in any tree of factories.

10. A strength - the strength of guarantee a factory makes about the coverage of combinations of levels from different test variables. The higher the strength, the more test variables a combination can refer to in the guarantee - and the longer the sequence of test cases will have to be to meet that guarantee.

NOTE: testing with a strength of 2 is commonly known as 'pairwise testing'.


Oh, one last thing: when a test case is produced that exposes a bug, it is inconvenient to have to repeatedly re-run the entire unit test when restarting a debugging session; one has to wait patiently while the parameterised unit test is presented all over again with a sequence of successful test cases leading up to the one where the test failure occurs.

There is a harness utility that will trap any exceptions propagated out of the parameterised unit test run under its control; when it traps an exception, the harness will create a *signature* that can be used to completely synthesize the test case exposing the bug. The signature and the exception are packaged into a special wrapper exception - one can copy the text of the signature from the initial debug session and then write a special one-off unit test that instructs the factory to go directly to creating the failing test case; so this one-off test can be used to perform further debugging without having to wait around on each re-run.

Walk me through an example!
---------------------------

Let's test a component that encodes text strings. A reverse decoding of the encoded format back to the original string is also supported.

The encoded format will support the ability to progressively reconstruct the original string as the sequence is received; for each character occurring at least once in the original string, the reconstruction will fill in all of the occurrences of that character in the decoded string in each progressive step.

So the string, "Madam, I'm Adam" would be reconstructed as:-

1. "???????????????"
1. "?????? ??? ????"
2. "?????? ?'? ????"
3. "?????, ?'? ????"
4. "?????, ?'? A???"
5. "?????, I'? A???"
6. "M????, I'? A???"
7. "Ma?a?, I'? A?a?"
8. "Mada?, I'? Ada?"
9. "Madam, I'm Adam"

Where the question mark denotes a placeholder for a missing character.

We'll write the test for this up-front as a parameterised unit test, and design the encoding and decoding API at the same time.

The objective is for you to see some source code that uses NTestCaseBuilder to generate test cases.


Our parameterised unit test simply takes a string as its parameter - for each string, it encodes it into the encoded format, then progressively decodes the format, checking the partially decoded result against the original string. We know how many steps the progressive decoding will take, because we can count the number of occurrences of each character in the original string in a histogram.

Our API just needs to create an encoded representation from a string, and then allow progressive decoding. How about this:-

    public class EncodedFormatStage1
    {
        /// <summary>
        ///     Constructs an encoding of a string.
        /// </summary>
        /// <param name="stringToBeEncoded">Non-null string to encode: may be an empty string.</param>
        public EncodedFormatStage1(String stringToBeEncoded)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        ///     Creates a new decoder.
        /// </summary>
        /// <returns>A freshly-created decoder set to start progressive decoding at the first character.</returns>
        public ProgressiveDecoder CreateNewDecoder()
        {
            throw new NotImplementedException();
        }

        public class ProgressiveDecoder
        {
            /// <summary>
            ///     Carries out a step of the progressive decoding of the format.
            ///     At each step, all of the occurrances of some character in the original encoded
            ///     string will be decoded and placed into a string builder at their original
            ///     locations. Each step deals with a unique character in the original string.
            /// </summary>
            /// <param name="builderForPartiallyDecodedString">
            ///     A non-null string builder for the partially decoded result. This is
            ///     modified on each call, and is intended to be reused across successive calls to this method to achieve a progressive
            ///     decoding. Can be set up arbitrarily; will be resized to accomodate the need for additional characters, or will be
            ///     trimmed if too long. Any existing characters not placed into the buffer by a previous call to this method will
            ///     eventually be overwritten or truncated over a progressive series of calls.
            /// </param>
            /// <returns>
            ///     True if 'builderForPartiallyDecodedString' contains the completely decoded string, false if there is more
            ///     decoding to follow.
            /// </returns>
            public Boolean DecodeIntoAndReportIfCompleted(StringBuilder builderForPartiallyDecodedString)
            {
                throw new NotImplementedException();
            }
        }
    }

Our parameterised unit test looks like this:-

	[Test]
	public void TestEncodingAndDecodingRoundtripStage1()
	{
		ParameterisedUnitTestForEncodingAndDecodingRoundtrip(String.Empty);
	}

	public void ParameterisedUnitTestForEncodingAndDecodingRoundtrip(String testCase)
	{
		var histogramFromTestCase = BuildHistogramOfCharacterFrequencies(testCase);

		var encodedFormat = new EncodedFormatStage1(testCase);

		var builderForPartiallyDecodedString = new StringBuilder();

		var expectedSizeOfHistogramFromPartiallyDecodedString = 0;

		var decoder = encodedFormat.CreateNewDecoder();

		while (!decoder.DecodeIntoAndReportIfCompleted(builderForPartiallyDecodedString))
		{
			// Compute histogram for decoded text: each maplet should be contained in the original histogram, and the number of bins in the histogram should grow by one each time.

			var histogramFromPartiallyDecodedString =
				BuildHistogramOfCharacterFrequencies(builderForPartiallyDecodedString.ToString());

			foreach (var character in histogramFromPartiallyDecodedString.Keys)
			{
				Assert.IsTrue(histogramFromTestCase.ContainsKey(character));
				Assert.IsTrue(histogramFromTestCase[character] ==
							  histogramFromPartiallyDecodedString[character]);
			}

			++expectedSizeOfHistogramFromPartiallyDecodedString;

			Assert.IsTrue(histogramFromPartiallyDecodedString.Count ==
						  expectedSizeOfHistogramFromPartiallyDecodedString);
		}

		var decodedString = builderForPartiallyDecodedString.ToString();

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

For this parameterised unit test, the test case is exactly the system under test - a string to be encoded and decoded. We don't need to add any operations into the test case, because I've deliberately made the API simple enough for the unit test to completely cover the possibilities.

(Actually, not quite - can you spot the untested possibility? It is one whose testing could reasonably be neglected by making an obvious implementation decision - I'll leave it for you to think about what's missing and whether you would write a test for it, or just design it out in the implementation. Hint: look at 'CreateNewDecoder()' and think about how it could be misused. Where is the decoding state maintained?)

The method 'ParameterisedUnitTestForEncodingAndDecodingRoundtrip()' is the actual parameterised unit test; the method 'TestEncodingAndDecodingRoundtripStage1' is a simple NUnit test that serves as a driver for it. To start with, we only have one test case - the empty string.

The driver seems a bit anaemic - just one test case of an empty string. Let's do better and put NTestCaseBuilder to work...

First things first - let's introduce NTestCaseBuilder to the test driver. Like this:-

	[Test]
	public void TestEncodingAndDecodingRoundtripStage2()
	{
		var factory = Singleton.Create(String.Empty);
		const Int32 strength = 3;

		factory.ExecuteParameterisedUnitTestForAllTypedTestCases(strength, ParameterisedUnitTestForEncodingAndDecodingRoundtrip);
	}

Our parameterised unit test remains unchanged, but the driver now creates a factory, and then uses that factory to execute the parameterised unit test as a delegate passed to the method 'ExecuteParameterisedUnitTestForAllTypedTestCases()'.

If we run this test, the stubbed implementation of 'EncodedFormat' promptly throws an exception - if you run under the debugger or use NUnit's logging to capture the output, you will see that NTestCaseBuilder has intercepted the exception and wrapped it inside a 'TestCaseReproductionException'.

This exception refers internally to the original exception - in this case, a 'NotImplementedException' that came from the stubbed implementation. It also adds a *reproduction string*, so that you can reproduce the test failure without having to re-run through all of the preceding test cases that did succeed.

In this case however, we only have one test case to start with - this is the empty string that was used to construct the singleton factory. So we'll forget about the error message for now and press on.



OK, that was nice, but all that I really did was to replicate our original anaemic driver test. Let's add some sophistication in by generating more than one test case - which means that we need to be able to vary the test case; which in turn means we need test variables.

So what are our test variables? Well, our test cases are strings, and a string is essentially a sequence of characters - so we can vary the choice of character at each position in the string. So in some way, each position on the string will have an associated test variable whose levels are the possible values the character at that position can take.

We have to be a bit careful here - for .NET, a character is a **Unicode** character - so we have 2^16 levels to play with. Even doing just pairwise combinations will overwhelm us with at least 2^16^2 combinations, i.e. 4 Gigabytes, regardless of any additional combinatoric complexity that NTestCaseBuilder can actually avoid.

So we need to limit the number of levels, and being a native English speaker, please indulge me in choosing the Roman alphabet lower-case letters 'a'..'z'. Just 26 levels, then.

If our test variables correspond to positions on a string test case, then the number of test variables imposes an upper limit on the string length. So how long should our strings get?

Let's do some very approximate estimation - say we set the maximum length to 5. Forget about shorter strings for now to keep the mathematics simple.

If we do a full cross product of all levels from all test variables, we'll have to churn through 26^5 = 11881376 test cases - nearly 12 million, then. That's too many.

If we set our sights on a strength of three, we might hope to get as low as 26^3 = 17576 combinations - over 17 thousand, then. That's a saving of a factor of almost 700 - not bad, and we could seriously consider this on one of today's modern electronic computing machines. Ahem.

In reality, I was being a bit too optimistic - I just counted the combinations of three of the 5 test variables and forgot the others. Let's try to estimate for doing a strength of three for all 5 of the test variables. Prepare for sloppy mathematics...

So, number of ways we can cover a particular choice of three variables from out of all five of them = 26^3. We just stated this above.

Number of choices of 3 variables taken from 5 = (5 Combination 3) = 5 * 4 / 2. This is from combinatoric theory: if this isn't familiar, go read up on permutations, combinations, factorials and Pascal's triangle. Or just trust me.

In a perfect world we could pack several combinations of different variables into the same test case. Pretending that none of the combinations ever share the same test variables (so we just fill out empty space in a test case), we can estimate a thinning out due to packing = 3 / 5, also conveniently disregarding the fact that 3 doesn't evenly divide 5.

I said this was sloppy mathematics.

So we estimate 26^3 * 5 * 4 / 2 * 3 / 5 = 105456: over 105 thousand. This is a factor of 6 worse than our optimistic calculation, but still a factor of 113 better than slogging through the entire cross product.

I'll wager my mathematics was too sloppy - perhaps NTestCaseBuilder can do better. So let's give it a try with strings of length up to 5 and a strength of 3!



So far so good - but what about the fact that strings can come in various lengths - including the empty string case?

We can deal with this by thinking inductively - if we start with a factory that produces strings from length 0 (the empty string) up to length N, then we can make this factory a child of a synthesizing factory, and add a sibling that is a test variable level factory. This test variable will correspond to the leftmost position in the strings produced by the synthesizing factory: its levels will be all of the allowable character values. The synthesizing factory will then prepend a character from such a level on to a test case taken from the original factory. Thus adding a character to the front of an existing string of some length between 0 and N inclusive.

So now, we have a synthesizing factory that produces strings of length 1 up to length N+1. If we then take **this** factory and make it a child of an interleaving factory, adding in a sibling that is a singleton factory producing the empty string, then you can see that the resulting tree of factories will produce either an empty string - length 0, or some string of length between 1 and N+1. In other words, we have inductively constructed a factory that produces strings of length 0 to length N+1 out of the original one.

Repeating the induction gives us progressively more complex trees for strings of length N+2, N+3, N+4 ... any positive length.

As a base case to this induction, we choose the simplest possible factory to get things started - again, the singleton factory producing empty strings.

To wire up the factories, we can stand the inductive process on its head, giving us a recursive build of the tree of factories. Like this:-

	[Test]
	public void TestEncodingAndDecodingRoundtripStage3()
	{
		const Int32 maximumStringLength = 5;

		var factory = BuildFactoryRecursively(maximumStringLength);
		const Int32 strength = 3;

		var numberOfTestCases = factory.ExecuteParameterisedUnitTestForAllTestCases(strength,
			ParameterisedUnitTestForEncodingAndDecodingRoundtrip);

		Console.Out.WriteLine("The parameterised unit test passed for all {0} test cases.",
			numberOfTestCases);
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
			(leftmostCharacterToPrepend, shorterString) => leftmostCharacterToPrepend + shorterString);

		return Interleaving.Create(new[] {_emptyStringFactory, factoryForNonEmptyStrings});
	}

Notice how I'm now using the return value of the call to 'ExecuteParameterisedUnitTestForAllTypedTestCases' - it tells me how many test cases I got through *if the parameterised unit test succeeded for each test case made by NTestCaseBuilder*.

I'll do this so I can make good on that wager I made beforehand when I've finished. In the meantime, of course, if any test case provokes a failure of the parameterised unit test, then the call will throw an exception and obviously I won't get any return value at all.

It's really just there as a 'feel-good-factor' when you've got your system under test debugged to the point where all the test cases go through with a pass. The 'green-bar moment', if you know what I mean.

Alright, here are the spoilers: once the parameterised unit test passes for all the test cases, NTestCaseBuilder reports back that it got coverage with 38446 test cases - a lot better than my sloppy estimate of 105456, and way better than the original brute force cost of 11881376.

Don't forget that this also includes shorter strings of length < 5 (which the brute force cost doesn't - it would have been a whopping 308915775 if we'd included those as well).

To summarise:-

	NTestCaseBuilder - strings of length <= 5 ----->  38 thousand
	Sloppy estimate  - strings of length == 5 -----> 105 thousand
	Brute force      - strings of length == 5 ----->  11 million
	Brute force      - strings of length <= 5 -----> 308 million

Advanced Stuff: Deferrals
-------------------------

Looking at the encoding / decoding example above, we had to make sure that we didn't build strings longer than 5 characters - specifically, we had to stop the recursion in 'BuildFactoryRecursively' from running out of control.

So we kept track of the recursion depth via the argument 'maximumStringLength' and wrote some guard logic to bottom out the recursion. This is correct, but rather annoying - conceptually at least, we should be able to imagine the recursion carrying on and on, making ever more complex test cases. Whether one wants all these extra test cases is debatable once a certain length has been reached, but the extra guard logic is annoying - it would be nicer to simply state that a string can be an empty string or 1 character prepended on to a shorter string, and just leave it at that.

NTestCaseBuilder could then be told to generate test cases up to some limit we would apply as a control parameter to the factory - so no more guard logic!

We can do this - we simply write the recursion out without any guard logic whatsoever (so the recursion doesn't appear to terminate) - the trick is to wrap the recursion within a *deferral*.

Like this:-

	[Test]
	public void TestEncodingAndDecodingRoundtripStage4()
	{
		const Int32 maximumStringLength = 5;

		var factory = BuildFactoryRecursivelyUsingDeferral().WithDeferralBudgetOf(maximumStringLength);
		const Int32 strength = 3;

		var numberOfTestCases = factory.ExecuteParameterisedUnitTestForAllTestCases(strength,
			ParameterisedUnitTestForEncodingAndDecodingRoundtrip);

		Console.Out.WriteLine("The parameterised unit test passed for all {0} test cases.",
			numberOfTestCases);
	}

	public ITypedFactory<String> BuildFactoryRecursivelyUsingDeferral()
	{
		var simplerFactoryForShorterStrings = Deferral.Create(BuildFactoryRecursivelyUsingDeferral);

		var factoryForNonEmptyStrings = Synthesis.Create(_factoryForSingleCharacters,
			simplerFactoryForShorterStrings,
			(leftmostCharacterToPrepend, shorterString) => leftmostCharacterToPrepend + shorterString);

		return Interleaving.Create(new[] {_emptyStringFactory, factoryForNonEmptyStrings});
	}
	
See how the code for building the factory has simplified - there is no guard logic at the start to terminate recursion.

A new kind of factory is introduced here - a deferral factory. This has a single argument: a delegate or lambda expression that itself has no arguments and creates a factory when invoked; this represents a factory whose creation is deferred until necessary.

What NTestCaseBuilder will do is to start working with the factory tree without the deferred part, creating simple test cases - in this case, an empty string.

Once the empty string has been created, NTestCaseBuilder steps up the complexity of the test cases by creating the deferred factory - it calls the delegate or lambda passed as parameter to the deferral factory.

This adds a new section of child factories on to the overall factory tree - including a second deferral, because the deferred factory is itself built by recursion. This allows strings of length 1 to be built, so NTestCaseBuilder does that until it exhausts the possibilities.

The next step in complexity introduces strings of length 2 and also brings in a third deferral, and so on - but once strings of length 4 are being generated, NTestCaseBuilder will not exhaustively create them, because of the strength limit of 3 we have placed on the top-level factory that we use to drive the test.

You might think that this test would run on forever, but it does not - the call *'.WithDeferralBudgetOf(maximumStringLength)'* on the top-level factory produced by 'BuildFactoryRecursivelyUsingDeferral()' imposes a cap on the complexity of the test cases - the deferral budget is the maximum number of deferrals that NTestCaseBuilder can 'activate' to deepen the factory tree, counting down from the top-level node.

This is what makes the string length top out at 5 in this example.

If you forget to call 'WithDeferralBudget', don't worry: the cap is zero by default - so you won't see any deferrals activate and your test cases will all be simple ones.

This count is always computed along a path from the top-level factory to whatever deferral is being considered, so we can also use deferrals in parallel on sibling factories. NTestCaseBuilder is smart enough to consider possibilities where one sibling subtree has deepened via an activated deferral, while another one is still pending.

To see what I mean, let's revisit the short sample from before - we'll add in support for negation, and make the use of brackets optional by handling operator precedence:

    [TestFixture]
    internal class TestCalculator
    {
        private static readonly ITypedFactory<Char> BinaryOperatorFactory =
            TestVariable.Create(new[] {'+', '-', '*', '/'});

        private static readonly ITypedFactory<Tuple<Boolean, String>> ConstantFactory =
            Synthesis.Create(TestVariable.Create(new[] {"0", "1", "2"}),
                constant => Tuple.Create(false, constant));

        private static Tuple<Boolean, String> BinaryExpressionFrom(Tuple<Boolean, String> lhs,
            Tuple<Boolean, String> rhs, Char binaryOperator)
        {
            switch (binaryOperator)
            {
                case '*':
                case '/':
                {
                    var lhsWithCorrectPrecendence = lhs.Item1 ? String.Format("({0})", lhs.Item2) : lhs.Item2;
                    var rhsWithCorrectPrecendence = rhs.Item1 ? String.Format("({0})", rhs.Item2) : rhs.Item2;

                    return Tuple.Create(false,
                        String.Format("{0} {1} {2}", lhsWithCorrectPrecendence, binaryOperator,
                            rhsWithCorrectPrecendence));
                }
                default:
                {
                    return Tuple.Create(true,
                        String.Format("{0} {1} {2}", lhs.Item2, binaryOperator, rhs.Item2));
                }
            }
        }

        private static ITypedFactory<Tuple<Boolean, String>> BuildExpressionFactoryRecursively(
            Boolean directlyToTheRightOfABinaryOperator)
        {
            var binaryOperatorExpressionFactory =
                Synthesis.Create(
                    Deferral.Create(
                        () => BuildExpressionFactoryRecursively(directlyToTheRightOfABinaryOperator)),
                    Deferral.Create(() => BuildExpressionFactoryRecursively(true)), BinaryOperatorFactory,
                    BinaryExpressionFrom);

            var negatedExpressionFactory =
                Synthesis.Create(Deferral.Create(() => BuildExpressionFactoryRecursively(true)),
                    expression =>
                        Tuple.Create(expression.Item1,
                            String.Format(directlyToTheRightOfABinaryOperator ? "(-{0})" : "-{0}",
                                expression.Item2)));

            var bracketedExpressionFactory =
                Synthesis.Create(Deferral.Create(() => BuildExpressionFactoryRecursively(false)),
                    expression => Tuple.Create(false, String.Format("({0})", expression.Item2)));
            return
                Interleaving.Create(new[]
                {
                    ConstantFactory, binaryOperatorExpressionFactory, negatedExpressionFactory,
                    bracketedExpressionFactory
                });
        }

        [Test]
        public void FireUpCalculators()
        {
            const Int32 maximumDepth = 3;

            var expressionFactory = BuildExpressionFactoryRecursively(false)
                .WithDeferralBudgetOf(maximumDepth);

            const Int32 strength = 2;

            var numberOfTestCasesExercised =
                expressionFactory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                    (testCase => Console.Out.WriteLine(testCase.Item2)));
            Console.Out.WriteLine("Exercised {0} test cases.", numberOfTestCasesExercised);
        }
    }

NTestCaseBuilder will create test cases in waves of increasing complexity as it increases the deferral budget up to the cap supplied.

Within a wave, the test cases will be decorrelated - the idea is to mix them up so that test levels don't just vary for one test variable at a time as we jump from one test case to the next. Rather, they are mixed up in a deliberately haphazard way.

You can see that in the test cases generated by the example above:-

To start with, no deferrals...

	0
	1
	2

End of the first wave - now allow one level of deferrals to be activated. We see binary operators, simple negation and bracketing competing...

	2 / 1
	0 - 1
	0 + 0
	0 * 2
	1 / 0
	1 - 2
	2 - 0
	1 * 1
	2 / 2
	1 + 1
	2 * 0
	2 + 2
	(0)
	-0
	(1)
	-1
	(2)
	-2
	0 / 0

End of the second wave - now allow two levels of deferrals to be activated. We can see mixing and nesting of negation, binary operator and bracketing in the same test case now...

	-1 - 2			NTestCaseBuilder used the full budget of two deferrals to make the lhs term, but only one for the rhs term.
	-2 * (-1)		Here, both the lhs and rhs use the full budget of two deferrals.
	0 / 1 / 0
	(1) / 1
	2 / (2)			The lhs just uses one deferral, the rhs takes the full budget of two deferrals.
	-1 + 2 + 2
	-0 + 2
	0 - (-0)
	2 * 1 - 2
	
So you can see how trees of deferrals can lead to both 'balanced' and 'lopsided' test cases.	

More Advanced Stuff: Filters
----------------------------

Let's write a parameterised unit test that will test a dictionary - the trusty System.Generic.Collections.Dictionary, to be precise. After all, you never know.

We'll do this in a fairly simple way - we'll take an empty dictionary, some key and a sequence of operations to be applied to the dictionary - addition of a value for that key, deletion of the key and its associated value, querying for the key's value, and replacing the value for a key (which may add the key in for the first time).

We can make this more complex in a while by mixing up operations that pertain to more than one key, but for now, let's press on.

Consider the operations - because they all involve the one shared key, the test can figure out for itself what the dictionary should be doing by maintaining a simple state machine for the key. This works precisely because a correctly working dictionary uses its keys to subdivide its value state into independent pieces - if I modify the stored value under key 1 from 'Fred' to 'Freya', then I know that the value 'Kermit' stored under key -5 will be completely unaffected.

Here we go, this one is quite long:-

	using Key = System.Int32;
	using Value = System.String;
	using Operation = System.Action<System.Collections.Generic.IDictionary<System.Int32, System.String>>;

    [TestFixture]
    public class TestDictionary
    {
        private enum OperationKind
        {
            Insertion,
            Deletion,
            Replacement,
            Query
        }

        private class OperationListBuilder
        {
            private const Int32 MaximumValueRepresentation = 20;
            private readonly Key _key;

            private readonly IDictionary<OperationKind, OperationCreator>
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists =
                    new Dictionary<OperationKind, OperationCreator>(),
                _operationKindToOperationCreatorMapWhereNoEntryExists =
                    new Dictionary<OperationKind, OperationCreator>();

            private readonly Random _randomBehaviour;
            private Value _value;

            public OperationListBuilder(Key key, Random randomBehaviourInitialState)
            {
                Operations = new List<Operation>();
                _key = key;
                _randomBehaviour = new Random(randomBehaviourInitialState.Next());

                AddStateTransitionsForWhenAnEntryAlreadyExists();

                AddStateTransitionsForWhenNoEntryExists();
            }

            public IList<Operation> Operations { get; private set; }

            private void AddStateTransitionsForWhenNoEntryExists()
            {
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add(OperationKind.Insertion,
                    AddInsertionOperationThatShouldSucceed);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add(OperationKind.Deletion,
                    AddDeletionOperationThatShouldFail);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add(OperationKind.Replacement,
                    AddReplacementOperation);
                _operationKindToOperationCreatorMapWhereNoEntryExists.Add(OperationKind.Query,
                    AddQueryOperationThatShouldFail);
            }

            private void AddStateTransitionsForWhenAnEntryAlreadyExists()
            {
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add(OperationKind.Insertion,
                    AddInsertionOperationThatShouldFail);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add(OperationKind.Deletion,
                    AddDeletionOperationThatShouldSucceed);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add(OperationKind.Replacement,
                    AddReplacementOperation);
                _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists.Add(OperationKind.Query,
                    AddQueryOperationThatShouldSucceed);
            }

            public void AppendNewOperationOfKind(OperationKind operationKind)
            {
                if (null != _value)
                {
                    _operationKindToOperationCreatorMapWhereAnEntryAlreadyExists[operationKind]();
                }
                else
                {
                    _operationKindToOperationCreatorMapWhereNoEntryExists[operationKind]();
                }
            }

            private void AddQueryOperationThatShouldFail()
            {
                Operations.Add(indexedSortedDictionary =>
                {
                    Console.WriteLine("Querying with key: {0} - this should fail.", _key);
                    Assert.IsFalse(indexedSortedDictionary.ContainsKey(_key));
                });
            }

            private void AddQueryOperationThatShouldSucceed()
            {
                var fixedValue = _value;
                Operations.Add(indexedSortedDictionary =>
                {
                    Console.WriteLine("Querying with key: {0} - this should succeed and yield: {1}.", _key,
                        fixedValue);
                    Assert.IsTrue(indexedSortedDictionary.ContainsKey(_key));
                    Assert.IsTrue(indexedSortedDictionary[_key] == fixedValue);
                });
            }

            private void AddDeletionOperationThatShouldFail()
            {
                Operations.Add(indexedSortedDictionary =>
                {
                    Console.WriteLine("Deleting key: {0} - this should fail.", _key);
                    Assert.IsFalse(indexedSortedDictionary.Remove(_key));
                });
            }

            private void AddDeletionOperationThatShouldSucceed()
            {
                Operations.Add(indexedSortedDictionary =>
                {
                    Console.WriteLine("Deleting key: {0} - this should succeed.", _key);
                    Assert.IsTrue(indexedSortedDictionary.Remove(_key));
                });
                _value = null;
            }

            private void AddInsertionOperationThatShouldSucceed()
            {
                _value = MakeRandomValue();

                var fixedValue = _value;
                Operations.Add(indexedSortedDictionary =>
                {
                    Console.WriteLine("Adding key: {0} with value: {1} - this should succeed.", _key,
                        fixedValue);
                    indexedSortedDictionary.Add(_key, fixedValue);
                });
            }

            private void AddInsertionOperationThatShouldFail()
            {
                var fixedValue = _value;
                Operations.Add(indexedSortedDictionary =>
                {
                    try
                    {
                        var newValue = MakeRandomValue();
                        Console.WriteLine("Adding key: {0} with value: {1} - this should fail.", _key,
                            newValue);

                        indexedSortedDictionary.Add(_key, newValue);
                    }
                    catch (ArgumentException)
                    {
                        return;
                    }

                    var stringBuilder = new StringBuilder();


                    stringBuilder.AppendFormat(
                        "Should not have been able to insert with key {0} as it already has an entry in the dictionary {1} of {2}",
                        _key, indexedSortedDictionary, fixedValue);

                    Assert.Fail(stringBuilder.ToString());
                });
            }

            private void AddReplacementOperation()
            {
                _value = MakeRandomValue();
                var fixedValue = _value;
                Operations.Add(indexedSortedDictionary =>
                {
                    Console.WriteLine("Replacing value for key: {0} with value: {1}.", _key, fixedValue);
                    indexedSortedDictionary[_key] = fixedValue;
                });
            }

            private Value MakeRandomValue()
            {
                return _randomBehaviour.ChooseAnyNumberFromOneTo(MaximumValueRepresentation).ToString();
            }

            #region Nested type: OperationCreator

            private delegate void OperationCreator();

            #endregion
        }

        private static void ParameterisedUnitTestForStandardDictionaryWithJustOneKey(
            OperationListBuilder operationListBuilder)
        {
            IDictionary<Key, Value> systemUnderTest = new Dictionary<Key, Value>();

            Console.WriteLine("**** New Test Case ****");

            foreach (var operation in operationListBuilder.Operations)
            {
                operation(systemUnderTest);
            }
        }

        [Test]
        public void TestStandardDictionaryWithJustOneKey()
        {
            var keyFactory = TestVariable.Create(Enumerable.Range(-2, 5));

            var operationFactory =
                TestVariable.Create(
                    from operationKind in
                        ((IEnumerable<OperationKind>) Enum.GetValues(typeof (OperationKind)))
                    select operationKind);

            const Int32 numberOfOperations = 10;

            var randomBehaviour = new Random(0);

            var operationKindSequenceFactory =
                Synthesis.Create(Enumerable.Repeat(operationFactory, numberOfOperations));

            var operationListBuilderFactory = Synthesis.Create(keyFactory, operationKindSequenceFactory,
                (key, operationKindSequence) =>
                {
                    var result = new OperationListBuilder(key, randomBehaviour);

                    foreach (var operationKind in operationKindSequence)
                    {
                        result.AppendNewOperationOfKind(operationKind);
                    }

                    return result;
                });
            const Int32 strength = 4;

            var numberOfTestCasesExercised =
                operationListBuilderFactory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                    ParameterisedUnitTestForStandardDictionaryWithJustOneKey);

            Console.Out.WriteLine("Exercised {0} test cases.", numberOfTestCasesExercised);
        }
    }
	
The operations enforce the test expectations as they run against the dictionary - the state machine for a key is part of the implementation of OperationListBuilder.

Note the handy overload of Synthesis.Create that takes a sequence of factories for the same input test case type, and yields a single factory whose output test cases are sequences of that input test case type.

Also, note how the number of operations in a sequence for a key is fixed in this test - if you think about it, there is no purpose in testing shorter sequences, because the act of executing the sequence of operations also tests the prefixes as well.

So, running this test yields output like this:-

	**** New Test Case ****
	Replacing value for key: -2 with value: 18.
	Adding key: -2 with value: 1 - this should fail.
	Adding key: -2 with value: 11 - this should fail.
	Replacing value for key: -2 with value: 19.
	Adding key: -2 with value: 17 - this should fail.
	Replacing value for key: -2 with value: 9.
	Adding key: -2 with value: 12 - this should fail.
	Replacing value for key: -2 with value: 9.
	Adding key: -2 with value: 19 - this should fail.
	Deleting key: -2 - this should succeed.

	etc...
	
	**** New Test Case ****
	Replacing value for key: -1 with value: 5.
	Replacing value for key: -1 with value: 4.
	Adding key: -1 with value: 9 - this should fail.
	Querying with key: -1 - this should succeed and yield: 4.
	Deleting key: -1 - this should succeed.
	Querying with key: -1 - this should fail.
	Querying with key: -1 - this should fail.
	Querying with key: -1 - this should fail.
	Adding key: -1 with value: 15 - this should succeed.
	Replacing value for key: -1 with value: 7.

	etc ...

	**** New Test Case ****
	Replacing value for key: 2 with value: 11.
	Querying with key: 2 - this should succeed and yield: 11.
	Querying with key: 2 - this should succeed and yield: 11.
	Replacing value for key: 2 with value: 20.
	Deleting key: 2 - this should succeed.
	Querying with key: 2 - this should fail.
	Deleting key: 2 - this should fail.
	Deleting key: 2 - this should fail.
	Adding key: 2 with value: 19 - this should succeed.
	Querying with key: 2 - this should succeed and yield: 19.
	**** New Test Case ****
	Querying with key: -2 - this should fail.
	Deleting key: -2 - this should fail.
	Replacing value for key: -2 with value: 16.
	Adding key: -2 with value: 3 - this should fail.
	Replacing value for key: -2 with value: 14.
	Replacing value for key: -2 with value: 6.
	Deleting key: -2 - this should succeed.
	Querying with key: -2 - this should fail.
	Deleting key: -2 - this should fail.
	Deleting key: -2 - this should fail.

NTestCaseBuilder generates 1310 test cases in this example to achieve coverage.

Now this is all very well - but look at the penultimate test case. There is a query for what is stored under key 2, followed straight away by exactly the same query. Both queries are expected to have the same outcome, as nothing else has happened in between.

Similarly, both the penultimate and final test cases have consecutive deletions on the same key when the key has been removed prior to the first attempt - so both fail in the same manner.

One could argue (and I do) that it is worth testing this behaviour - we want to verify that operations that are expected to leave the dictionary unchanged don't mysteriously perturb its internal state in such as way as to cause an externally visible change.

OK, but what about the second test case listed above - it has *three* consecutive queries. Do we need the third one, once we've shown that the second behaves the same way?

There are worse offenders elsewhere in the output, too:

	**** New Test Case ****
	Deleting key: 2 - this should fail.
	Deleting key: 2 - this should fail.
	Deleting key: 2 - this should fail.
	Deleting key: 2 - this should fail.
	Deleting key: 2 - this should fail.
	Deleting key: 2 - this should fail.
	Replacing value for key: 2 with value: 7.
	Deleting key: 2 - this should succeed.
	Adding key: 2 with value: 6 - this should succeed.
	Replacing value for key: 2 with value: 11.

I think you'd agree that the point was made with just the first two deletions!

What we need is some way of keeping the mixing up in any position of different kinds of operation, but we want to prevent more than two consecutive operations being of the same kind for every test case.

What we need is a *filter*.

Let's put one in - I'll just amend the previous code by changing:

        [Test]
        public void TestStandardDictionaryWithJustOneKey()
        {
<i>blah, blah...</i>
			
            var operationKindSequenceFactory =
                Synthesis.Create<IEnumerable<ITypedFactory<OperationKind>>, OperationKind>(
					Enumerable.Repeat(operationFactory, numberOfOperations)).WithFilter(
                        FilterOutThreeOrMoreConsecutiveIdenticalOperationKinds);
			
<i>etc...</i>
		
		}

So now there is a call *'.WithFilter(FilterOutThreeOrMoreConsecutiveIdenticalOperationKinds)'* on the factory for the sequence of operation kinds.
		
I'll also add in a filter function:

        private static Boolean FilterOutThreeOrMoreConsecutiveIdenticalOperationKinds(
            IDictionary<Int32, Tuple<Int32, Object>> testVariableIndexToLevelDictionary)
        {
            var testVariableIndices = testVariableIndexToLevelDictionary.Keys;

            var numberOfTestVariables = testVariableIndices.Count;

            var sortedTestVariableIndices = new Int32[numberOfTestVariables];

            testVariableIndices.CopyTo(sortedTestVariableIndices, 0);

            Array.Sort(sortedTestVariableIndices);

            if (1 >= numberOfTestVariables)
            {
                return true;
            }

            var preceedingTestVariableIndexAndConsecutiveCount =
                Tuple.Create(sortedTestVariableIndices.First(), 1);

            foreach (var index in Enumerable.Range(1, numberOfTestVariables - 1))
            {
                var testVariableIndex = sortedTestVariableIndices[index];

                var preceedingTestVariableIndex = preceedingTestVariableIndexAndConsecutiveCount.Item1;

                if (1 + preceedingTestVariableIndex == testVariableIndex &&
                    testVariableIndexToLevelDictionary[preceedingTestVariableIndex].Item1 ==
                    testVariableIndexToLevelDictionary[testVariableIndex].Item1)
                {
                    var consecutiveCount = preceedingTestVariableIndexAndConsecutiveCount.Item2;

                    if (2 == consecutiveCount)
                    {
                        return false;
                    }

                    preceedingTestVariableIndexAndConsecutiveCount = Tuple.Create(testVariableIndex,
                        1 + consecutiveCount);
                }
                else
                {
                    preceedingTestVariableIndexAndConsecutiveCount = Tuple.Create(testVariableIndex, 1);
                }
            }

            return true;
        }
	
What the filter function does is to vet potential test cases that NTestCaseBuilder is thinking about building up - if it returns 'true', NTestCaseBuilder will carry on as usual. When instead it returns 'false', NTestCaseBuilder is made to reconsider how to combine test levels from the test variables in such as way as to avoid the combination that failed the filter.

Now, think about what I just said - this isn't simply a post-processor that scans through the generated test cases; rather this is way of telling the algorithm inside NTestCaseBuilder to reconsider its optimisation strategy for creating a minimal number of test cases - so the end sequence of test cases is still packed in an optimal sense, even if certain potential combinations were blocked by a filter.

Also, because this isn't a post-processing step, NTestCaseBuilder can (and will) present the filter with partial combinations of test levels that are not complete test cases - these may have gaps at either end, or holes in the middle - the filter might even be presented with just one test variable at a time.

The API reflects this by not working with actual test cases - that would imply that NTestCaseBuilder would have to build complete test cases first, which would in turn force filtering to be a post-processing step, thus not guaranteeing optimal coverage.

Instead, a dictionary is passed to the filter - its keys are indices that denote test variables. The way to interpret these test variable indices is to count the number of factories that occur as leaves in the subtree of whatever factory the filter is attached to, working from left to right.

So test variable index 0 denotes the leftmost test variable or singleton factory in the subtree, 1 is the next one to the right and so on.

The test variable indices in the dictionary are always purely for **test variables** - singleton factories count towards the index, but never actually appear in the dictionary - this is because they are unvarying test cases and as such are not optional - if it is important to include a singleton test case's value in a filter, it should be done explicitly.

The value in the dictionary associated with a test variable index key is a pair of an integer index that denotes the particular level the test variable has taken in the test case being considered, together with the actual level object.

The reason for keeping a level index in addition to the level itself is because it is often more convenient to work with it - rather than worry about whether equality is well-defined for the type of the levels, one can simply compare the level indices to see whether levels from two different test variables clash or not.

Nevertheless, the option is still there to work with the actual level values - just bear in mind that they are typed as 'System.Object', and thus may be boxed - and will probably require potentially unsafe downcasting to a more exact type. The choice is yours - in the example above, level indices are used to detect consecutive runs of test variables whose levels are the same.

Now, if NTestCaseBuilder is calling a filter before it has created complete and final test cases, and if it can reconsider how to repack its test cases, then this implies that a filter should not expect to see a complete set of test variable indices as keys in the dictionary passed to it. Nor should it expect to see even groups of adjacent test variables - the keys may correspond to isolated test variables, or groups of adjacent test variables with gaps between or to the side of them, or both. Indeed, NTestCaseBuilder doesn't even guarantee that it will progressively fill in the test variables for a given combination of levels - it can jump around quite haphazardly and reconsider partially overlapping combinations.

So it is up to the writer of the filter to make sure that a filter is *consistent* - if a filter returns 'true' for some combination of levels from a group of test variables, it is permitted to return 'false' if a new test variable and level are added into the mix. The reverse is *not* true - once a filter has labelled a combination with 'false', it cannot change its mind later, even if it sees new test variables being added to the combination.

As an example:

	Test Variables 2, 5 & 6 have unequal levels ==> True
	Test Variables 2, 5 & 6 have unequal levels, but 7 is set to 'FooBar' ==> False
	Otherwise ==> False

This is OK for a filter, but if I changed that to:

	Test Variables 2, 5 & 6 have unequal levels ==> True
	Test Variables 2, 5 & 6 have unequal levels, but 7 is set to 'FooBar' ==> False
	Test Variables 2, 5 & 6 have unequal levels, but 7 is set to 'FooBar' and 20 has an equal level to 5 ==> True
	Otherwise ==> False

The third line is not consistent with the second one - the filter is changing its mind back again based on additional evidence. Put another way, the filter must implement 'anti-double-jeopardy' - it can retry an innocent test case and find it guilty, but it can't retry a guilty test case to find it innocent again.

The correct way to code the second filter would be:

	Test Variables 2, 5 & 6 have unequal levels ==> True
	Test Variables 2, 5 & 6 have unequal levels, but 7 is set to 'FooBar' ==> True
	Test Variables 2, 5 & 6 have unequal levels, but 7 is set to 'FooBar' and 20 has an unequal level to 5 ==> False
	Otherwise ==> False
	
Which can be immediately optimised into:

	Test Variables 2, 5 & 6 have unequal levels ==> True
	Test Variables 2, 5 & 6 have unequal levels, but 7 is set to 'FooBar' and 20 has an unequal level to 5 ==> False
	Otherwise ==> False

Other than that the rules of the game for filters are best summarised as:-

1. You can put filters on any factory in a tree.
2. You can have more than one factory in a tree with a filter.
3. You can have more than one filter on a factory - they are logically conjoined together - they all have to pass the combination in unison for it to be accepted.
4. The test variable indices used as keys in a dictionary are always counted off from the point of view of the factory the filter was attached to, even if that factory forms part of a larger tree, and even if there are other filters elsewhere in the tree.
5. Singleton test variables never show up implicitly via the dictionary passed to a filter.
6. You have to be consistent or NTestCaseBuilder may launch a nuclear missile strike. You have been warned.
	
With that in mind, what does our revised test do?

Well, it no longer generates daft test cases with three or more consecutive operations:

	**** New Test Case ****
	Replacing value for key: -2 with value: 18.
	Adding key: -2 with value: 11 - this should fail.
	Deleting key: -2 - this should succeed.
	Replacing value for key: -2 with value: 19.
	Adding key: -2 with value: 17 - this should fail.
	Replacing value for key: -2 with value: 9.
	Adding key: -2 with value: 12 - this should fail.
	Replacing value for key: -2 with value: 9.
	Deleting key: -2 - this should succeed.
	Adding key: -2 with value: 1 - this should succeed.
	**** New Test Case ****
	Adding key: -2 with value: 15 - this should succeed.
	Adding key: -2 with value: 17 - this should fail.
	Replacing value for key: -2 with value: 15.
	Querying with key: -2 - this should succeed and yield: 15.
	Adding key: -2 with value: 5 - this should fail.
	Deleting key: -2 - this should succeed.
	Adding key: -2 with value: 14 - this should succeed.
	Deleting key: -2 - this should succeed.
	Adding key: -2 with value: 19 - this should succeed.
	Deleting key: -2 - this should succeed.
	
	etc...

NTestCaseBuilder now generates 1339 test cases in this example to get coverage - so you can see that NTestCaseBuilder is not simply throwing away test cases by postprocessing - in this case it has actually generated a few more test cases to get coverage.

Stuff I didn't Document
-----------------------

Sorry, I try to make the documentation as complete as possible, but I have time pressures. So here's a list of things that I hope you figure out by looking at the NTestCaseBuilder.xml file, or by using Resharper or whatever to view the API.

Hey, you're all bright people, you'll get the idea...

1. WithMaximumStrength is a method on a factory that imposes a local cap on its strength - so regardless of the overall strength used to generate test cases, this factory and (in the absence of further strength caps) its subtree will impose a limit on the strength passed down.

2. WithZeroStrengthCost is a method on a factory that gives it a 'free-pass'; it can explore the combinations from its subtree right up to the limits of its strength without taking away from the strength of its sibling subtrees. The best thing to do is to have a play with this and see what it does!

3. Synthesis.Create has a very handy overload that uses WithZeroStrengthCost to create and / or apply a permutation across a list of test cases generated by the child factories. Again, have a play around.

4. Synthesis.Create has another overload that allows curried F# functions to be used for synthesis.

5. It's a good idea to study the tests for NTestCaseBuilder to see just what the Synthesis API can do, as well as browse the overloads.

6. There are actually two public interfaces for factories - IFactory and ITypedFactory<TestCase>. These are analogous to IEnumerable and IEnumerable<Item>, and couple to the latter classes as well. Browse the interfaces and you'll get the idea.

7. You can make an enumerable of test cases from the corresponding factory interface, if you want to decide how to process the test cases yourself.
	
How do I install this thing?
----------------------------

### Install assemblies via NuGet.

NTestCaseBuilder is available via the public NuGet feed as a package called 'NTestCaseBuilder'. You know the drill, don't you?

If not, head over to the NuGet documentation at [http://docs.nuget.org](http://docs.nuget.org) - it will walk through how to use a NuGet package and any setup steps you may need in Visual Studio.

The current package targets the .Net framework v4.0.

### Build from source.

1. I assume you know how to clone a Git repository, as you're looking at this on GitHub: otherwise, if you go to the main webpage for this project, you should see button commands to either clone in Windows via a nice GUI front-end to Git, or to just download a snapshot of the project as a .zip file.

2. Open up Visual Studio 2010. (There is a branch that is a backport to Visual Studio 2008, but this gets very little attention from me at the time of writing. If this is vital, get in touch with me.)

3. Make sure that the NuGet Visual Studio Extension is installed in Visual Studio and is up to date. If this isn't familiar to you, go see: [http://nuget.codeplex.com/wikipage?title=Getting%20Started](http://nuget.codeplex.com/wikipage?title=Getting%20Started).

4. Open up the solution '...\development\solution\everything.sln' from within your cloned Git repository for this project.

5. Make sure that the option to "Enable NuGet Package Restore" is activated - if it isn't, you will see this as a menu option in the right-click context menu for the solution explorer view.

6. You will probably see yellow warning icons in the solution explorer view for the dependencies managed by NuGet. Don't worry about this.

7. Go ahead and build - NuGet *should* download and install the various third-party dependencies; those yellow icons will eventually disappear.

8. However, if this doesn't work and you get build failures due to missing dependencies, you can use the right-click context menu for the solution explorer view to open up the NuGet package manager - "Manage NuGet Packages for Solution...". This takes you to a dialog which, in this particular situation, will give you a button command to restore missing packages. Do this and retry step #7.

9. You need to use the assemblies built by the project 'NTestCaseBuilder'. *NTestCaseBuilder.dll* is the one that your project will directly reference; it has an accompanying XML file for the API documentation.

A Thought-Provoking Article You Should Read
-------------------------------------------

[http://www.testingeducation.org/wtst5/PairwisePNSQC2004.pdf](http://www.testingeducation.org/wtst5/PairwisePNSQC2004.pdf).

There are several points that the article makes, but ones which can be addressed here are:-

1. If there are certain test cases that are much more probable than others, it is possible to make this explicit by setting up the tree of factories with an interleaving factory. One child of the interleave is the conventional synthesis of a test case from a multitude of test variables and their levels; the other can be a test variable level factory that supplies special cases that are much more probable in practice and would be missed by low-strength testing. These special cases can themselves by synthesized from special-case test variables if desired. What would be nice would be the ability to specify a very high strength of combination to apply to the special case part of the interleave, but revert back to say, a strength of 2 on the ordinary part of the interleave to get pairwise testing for the other test variables - this is on the current todo list below.

2. The number of levels for a test variable may be effectively infinite (think of floating-point numbers), or so large as to preclude putting in every level. This means that even an exhaustive enumeration of the cross-product of all test variable levels taken from finite sets could miss exposing a bug. In this situation, there either needs to be some thought as to whether there are levels for a test variable that are obvious 'trouble-spots' from the specification, or consider the use of a tool such as Pex (discussed below) as a cheap way of generating a good set of levels.

Pex: the 800-pound gorilla?
---------------------------

It is impossible not to refer to the Pex tool developed by Microsoft Research (see: [http://research.microsoft.com/en-us/projects/pex](http://research.microsoft.com/en-us/projects/pex)).

This tool examines compiled IL for .NET components and creates a sequence of test cases automatically that cover paths in the IL - it claims to be able to deal with executables written in C#, F# and Visual Basic (and I have successfully tried it on snippets of both C# and F# code).

As both NTestCaseBuilder and Pex are in the business of producing sequences of test cases with a view to provoking bug detection in parameterised unit tests, there is obviously considerable overlap between the two.

What differentiates the two are:-

									Pex										NTestCaseBuilder
									---										----------------

			Automatically generates test cases that efficiently
			pick out branch paths in code. Not only effective, but
			quite convenient as it avoids having to manually enter
			lots of test variable levels - the tool works them out
			behind the scenes.

			Relies entirely on the implementation to guide it:
			so if the implementation misses some aspect of a
			specification, this may not show up at all.

																Allows test variable levels to be set explicitly,
																so a specification can be used to drive it.

																Requires manual entry of the test levels as code, or
																at least the writing of code to generate test levels
																according to some scheme.

			Only understands IL - so cannot drive a GUI or a network
			connection or a native code component.

																Has no understanding of any kind of code at all - but
																can be used to drive anything that can be described via
																test variables and levels.

			NOTE: Pex can be taught to drive a GUI or a network
			connection or a native component by creating logic in
			the test cases that does the driving - Pex can then
			analyse this logic as IL. This is in fact exactly how
			NTestCaseBuilder would be used.

			The point is that once this step is done, the user
			has had to manually denote the test variables and levels
			in the driver logic - so Pex is not automating their discovery.

			Also, Pex is only examining the test driver logic - so
			once it has covered the driver logic's branches, it stops:
			this may miss generating important test cases in the
			real system being tested.

																Scales up to progressively more complex test cases for
																testing higher-level components.

			Does not scale well for higher-level components: requires
			a cutoff so that it does become overwhelmed by the analysis
			of lower-level component dependencies.

I think there is a synergy that can be exploited between the two approaches: Pex can be used to create test levels for low-level components - so instead of using a tree factory to synthesize a low-level component, Pex can be used to create fully-fledged instances of the low-level component that are in turn treated as test levels by NTestCaseBuilder.

The idea here is that the test variable levels correspond to distinct branches in the low-level component implementation logic - so the effect of combining such levels should stand a good chance of exposing bugs at higher levels as well. What Pex brings is the ability to find these 'interesting' levels automatically. What NTestCaseBuilder brings is the ability to combine these together for higher-level components in a scalable fashion.



Can this possibly be improved?
------------------------------

Yes indeed...

NTestCaseBuilder needs *you*: it would be great for others to fork this repository and take a task from below, or add some bells and whistles of their own.

Tasks:

1. Publish this via NuGet for immediate consumption of binaries in Visual Studio.	*** DONE ***

2. Add the capability to recursively build up a tree of factories, so that the final generated test cases can be arbitrarily 'long'. The encoding example above is a case in point: it has been arbitrarily limited to just ten characters per string test case, but it should be able to produce longer and longer strings in a lazily-evaluated fashion until the test decides that it has run long enough.	*** DONE ***

3. In a similar vein, consider a progressive approach where the strength is increased and again, the sequence is produced via lazy-evaluation; the test can keep going until a time limit is reached.	*** DONE ***

4. Allow local caps on the strength for subtrees within the tree of factories. This is because we may know that some test variables will have largely independent behaviour, so we can trade off a lower strength of combination for just these variables against having a higher overall strength of combination.	*** DONE ***

4. Integrate with Pex - smooth the path for importing Pex-generated test cases as test-levels for higher-level tests, also for integrating with Pex's notion of a parameterised test.

5. Add support for automated permuting of 'operation'-style test cases. Also take into account a variable number of operations; this is currently worked-around using interleaving. *** PARTIALLY DONE ***

6. Carry on with the Scala port of this code at *sageserpent-open/fsharp-to-scala-port-case-study*. **Maybe...**

7. Return an enumerable that computes test cases asynchronously to give better throughput when the parameterised unit test is itself computationally demanding.

8. Extend merged partial test vectors into full test vectors either some or all of the time to give even more early-access full test vectors.

9. Implement exclusions for combinations of specific test levels from across several test variables - sometimes one wants to test combinations of test variables, but there are some levels from separate variables that shouldn't go together, although one would still want to see the other combinations involving these levels. This can be done by filtering, but it would be better to avoid generating the forbidden combinations in the first place - this would open up other possibilities for packing combinations together. This is good for weeding out test cases that are infeasible because of precondition failures. *** DONE ***

10. Extend #9 so that once a reproducible failing test case is obtained, its signature can be used to set the exclusion - so one can see if that test case is the only one that causes the failure. Doing this iteratively can isolate the specific test levels that are interacting to create the failure.

11. Extend the functionality in #5 to allow splicing of an ordered sequence of operations into varying points within a larger sequence, while preserving the order of the spliced sub-sequence. This is motivated by the example shown in the repository - look for test 'ComplexExample' in project 'NTestCaseBuilder.Examples'.

12. Produce an examples NuGet feed based on the examples in the NTestCaseBuilder repository. *** DONE ***

13. Produce either an applicative functor or a monad to make test case building more declarative and concise.
