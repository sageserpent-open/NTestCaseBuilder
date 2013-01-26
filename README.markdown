NTestCaseBuilder, for .NET Testing
==================================

**WARNING: LIVING DOCUMENT - WORK VERY MUCH IN PROGRESS**

This is a .NET library that generates sets of test cases, for use by parameterised tests.

Each test case is built up progressively from smaller pieces of data that are combined together.

The smallest pieces are referred to as 'test variables'. Each test variable is constrained to take values from its own set of allowed 'levels'.

Changing each test variable independently to a new level produces a new test case.

If a test requires a certain 'strength' of coverage of combinations of levels from different test variables, the library produces a stream of test cases that honour this guarantee while avoiding getting swamped with a combinatoric explosion of test cases.

If a parameterised test fails with an exception for a specific test case, the library creates a signature that allows it to reproduce the failing test case immediately in a separate test; this is to aid debugging.

The sources are written in F#, but the API can be used just as comfortably from C# as from F#.


Some introductory background and a full worked example are given below. A note on a related .NET utility, Pex, is also provided at the towards the end along with a link to a thought-provoking article relevant to this library.

License
-------

The MIT License (MIT)

Copyright (c) 2013 Gerard Murphy, SageSerpent Ltd

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

I've avoided the use of mocking and fluent testing above to keep things simple. The important point is that this style of test is generally not sufficient for a statement of correctness of the code; rather, it's purpose is to illustrate the API of the system under test by way of example.

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

It becomes progressively more difficult to set up the component under test because of the increasing number of dependency components that have to be correctly set up for the component under test to run. In effect, a unit test for a higher-level component is kind of integration test for that component's dependencies.

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

Taking these italicesed terms and applying them to the breakdown above yields a conceptual tree:

	Synthesis (of a TestCase)
								-	Interleaving (of a Foo)
															-	Singleton (of a Foo created with the parameterless constructor)
															-	Synthesis (of a Foo)
																					-	Synthesis (of a Bar)
																										-	Test Variable (of the 'Bar' constructor parameter with 3 levels
																											               - closed, normal business and taking last orders)
								-	Synthesis (of a sequence of Operation)
															- 	Test Variable (of an 'Operation' with two levels - execute 'DoThis()' and execute 'DoThat()')
															-	Test Variable (ditto)
															-	Test Variable (ditto)
															-	Test Variable (ditto)

NTestCaseBuilder realises such conceptual trees as trees of *test case factories* - each factory can be either a *synthesizing factory*, an *interleaving factory*, a *test variable level factory* or a *singleton factory*.

A factory has two roles:-

1. It produces a sequence of varying test cases, for direct consumption in a parameterised unit test. This is what we see in the example above for 'TestCase'.

2. It can participate in the composite design pattern, becoming part of a larger tree of factories whose root produces more complex test cases. We see this above for all the subtree nodes.


Moving on, let's look at the test variables that contribute to the making of a 'TestCase' above - we have 4 test variables * 2 levels for the operation slots, with an additional 1 test variable * 3 levels for the parameterised 'Bar' constructor. Note that the singleton doesn't count - as it always yields a non-varying data value, there is essentially no test variable for it.

That means there are potentially 2^4 * (1 + 3) = 64 different test cases to produce. If you're paying attention, you'll note that I added in the '1 +' in the calculation - this is because we have an interleave that contributes 3 possible variable levels via one alternative, and 1 singleton via the other. So although singletons don't count as test variables, in this case we still get a distinct contribution to be counted in because of the parent interleave.

That wasn't really a complex example - one can realistically imagine, say 20 test variables with 5 levels feeding into a synthesis - meaning 5^20 = 95367431640625 test cases would result.

That's rather a lot of test cases - do we really need all of them?


Let's think about how likely a bug is going to be manifest when we run our parameterised test repeatedly over all 95367431640625 test cases. Let's say that we've already fixed the low-hanging bugs that occur straightaway for just about any choice of test case.

When I say 'low-hanging bugs', I mean things like:-

1. Forgetting to implement the stubbed code you wrote when you were coding 'test-first' in a language with static typing - or just plain forgetting to write the code if your language uses dynamic typing.
2. The division by zero one of your colleagues put in for a joke while you took a comfort break.
3. The off by one error in a loop that was definitely your fault.
4. The lack of initialisation that the compiler warned you about, but of course, we all know better than to trust diagnostic advice from a compiler.
5. The silly mistake in the implementation that was caused by a cosmic ray hitting a brain cell while typing a line of code.

Typically, a smoke test will expose most of these kinds of errors (and this is why it's still worth writing smoke tests in the early stages of implementation, assuming you haven't written any kind of test with expectations).

So at this point we have an implementation for our component under test that seems to work - it would certainly pass a smoke test coded in the style shown right at the beginning, and would probably survive an 'executable-documentation' unit test too.

We then expect that our parameteried unit test will be repeatedly called with each new test case, and will repeatedly succeed, until at some point - **KERBOOM**: test failure!

So the failing test case contains a magic combination of levels for its test variables that cause the component to fail. Is it likely that **all** of the test variables contribute to the failure? All 20 of them?

It's safe to assume - at least as a starting point - that maybe only one, two, three or four test variables contribute the magic combination of levels that cause the failure. All of the other test variables could have their levels set to whatever choices we'd like and we'd still get a failure.

So we could thin out the number of test cases considerably - say we had a failure requiring a specific combination of levels from four test variables - we could find the failing test case within 5^4 = 625 repeats at most - much better!

The problem is that we don't know which of the 20 test variables are the ones that can act in concert to expose the bug.

However, imagine that we could generate a sequence of 'incomplete' test cases using only the first four test variables - that would give us 625 test cases with holes in them where we haven't yet decided what levels to set the remaining test variables to.

Perhaps we could then build another parallel sequence of incomplete test cases using the next four test variables - again that would give is 625 test cases with different holes in them; these holes would include the test variables that we filled in in the previous sequence (and vice-versa).

So merging the two sequences of test cases together would provide a new sequence of test cases that would include all combinations of test levels from the first four test variables and all combinations of test levels from the next four test variables, with fewer holes. Because we merge corresponding test cases from the two sequences, the resulting sequence length is still 625!

Carrying on with this procedure would yield a sequence of 625 test cases that would cover combinations of test levels from successive groups of four test variables - so for the much lower effort of examining up to 625 test cases, we have some chance of finding the same bug that we would have had to trawl through 95367431640625 test cases!

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

The encoded format will support the ability to progressively reconstruct the original string as the sequence is received; for each character occurring at least once in the original string, the reconstruction will fill in all of the occurrances of that character in the decoded string in each progressive step.

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

We'll write the test for this up-front as a parameterised unit test, and design the API at the same time. Then we'll implement it and see how it fares against the unit test, and see how NTestCaseBuilder can help us in the process.

Actually, *we'll* do the first part together on this document, and *I'll* go off and code the second part as a series of commits in this repository; these will go in the directory 'development\solution\SageSerpent.NTestCaseBuilder.WorkedExample' and will be tagged; with the benefit of hindsight, you can then watch me as I made mistakes and went through the debugging process.

The objective is for you to see some source code that uses NTestCaseBuilder to generate test cases, as well as the use of signatures to create one-off tests for debugging and a special test variable level factory for subsequent higher-level testing.


Our parameterised unit test simply takes a string as its parameter - for each string, it encodes it into the encoded format, then progressively decodes the format, checking the partially decoded result against the original string. We know how many steps the progressive decoding will take, because we can count the number of occurrances of each character in the original string in a histogram.

Our API just needs to create an encoded representation from a string, and then allow progressive decoding. How about this:-

    public class EncodedFormatStage1
    {
        ///<summary>
        /// Constructs an encoding of a string.
        ///</summary>
        ///<param name="stringToBeEncoded">Non-null string to encode: may be an empty string.</param>
        public EncodedFormat(String stringToBeEncoded)
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

Our parameterised unit test looks like this:-

	[Test]
	public void TestEncodingAndDecodingRoundtripStage1()
	{
		ParameterisedUnitTestForEncodingAndDecodingRoundtrip(String.Empty);
	}

	public void ParameterisedUnitTestForEncodingAndDecodingRoundtrip(String testCase)
	{
		IDictionary<Char, Int32> histogramFromTestCase = BuildHistogramOfCharacterFrequencies(testCase);

		var encodedFormat = new EncodedFormatStage1(testCase);

		var builderForPartiallyDecodedString = new StringBuilder();

		Int32 expectedSizeOfHistogramFromPartiallyDecodedString = 0;

		EncodedFormat.ProgressiveDecoder decoder = encodedFormat.CreateNewDecoder();

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

	private static IDictionary<Char, Int32> BuildHistogramOfCharacterFrequencies(IEnumerable<Char> stringDistribution)
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

(Actually, not quite - can you spot the untested possibility? It is one whose testing could reasonably be neglected by making some obvious implementation decisions - I'll leave it for you to think about what's missing and how you'd either write a test for it, or just design it out in the implementation. Hint: look at 'CreateNewDecoder()' and think about how it could be misused.)

The method 'ParameterisedUnitTestForEncodingAndDecodingRoundtrip()' is the actual parameterised unit test; the method 'TestEncodingAndDecodingRoundtripStage1' is a simple NUnit test that serves as a driver for it. To start with, we only have one test case - the empty string.

The driver seems a bit anaemic - just one test case of an empty string. Let's do better and put NTestCaseBuilder to work...

First things first - let's introduce NTestCaseBuilder to the test driver. Like this:-

	[Test]
	public void TestEncodingAndDecodingRoundtripStage2()
	{
		var factory = SingletonTestCaseEnumerableFactory.Create(String.Empty);
		const Int32 strength = 3;

		factory.ExecuteParameterisedUnitTestForAllTypedTestCases(strength, ParameterisedUnitTestForEncodingAndDecodingRoundtrip);
	}

Our parameterised unit test remains unchanged, but the driver now creates a factory, and then uses that factory to execute the parameterised unit test as a delegate passed to the method 'ExecuteParameterisedUnitTestForAllTypedTestCases()'.

If we run this test, the stubbed implementation of 'EncodedFormat' promptly throws an exception - if you run under the debugger or use NUnit's logging to capture the output, you will see that NTestCaseBuilder has intercepted the exception and wrapped it inside a 'TestCaseReproductionException'.

This exception refers internally to the original exception - in this case, a 'NotImplementedException' that came from the stubbed implementation. It also adds a *reproduction string*, so that you can reproduce the test failure without having to re-run through all of the preceeding test cases that did succeed.

In this case however, we only have one test case to start with - this is the empty string that was used to construct the singleton factory. So we'll forget about the error message for now and press on.



OK, that was nice, but all that I really did was to replicate our original anaemeic driver test. Let's add some sophistication in by generating more than one test case - which means that we need to be able to vary the test case; which in turn means we need test variables.

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

		var numberOfTestCases = factory.ExecuteParameterisedUnitTestForAllTypedTestCases(strength, ParameterisedUnitTestForEncodingAndDecodingRoundtrip);

		Console.Out.WriteLine("The parameterised unit test passed for all {0} test cases.", numberOfTestCases);
	}

	public TypedTestCaseEnumerableFactory<String> BuildFactoryRecursively(Int32 maximumStringLength)
	{
		if (0 == maximumStringLength)
		{
			return _singletonFactoryForEmptyString;
		}

		var simplerFactoryForShorterStrings = BuildFactoryRecursively(maximumStringLength - 1);

		var factoryForNonEmptyStrings = SynthesizedTestCaseEnumerableFactory.Create(
			_factoryForSingleCharacters, simplerFactoryForShorterStrings, (leftmostCharacterToPrepend, shorterString) => leftmostCharacterToPrepend + shorterString);

		return InterleavedTestCaseEnumerableFactory.Create(new[] { _singletonFactoryForEmptyString, factoryForNonEmptyStrings });
	}

Notice how I'm now using the return value of the call to 'ExecuteParameterisedUnitTestForAllTypedTestCases' - it tells me how many test cases I got through *if the parameterised unit test succeeded for each test case made by NTestCaseBuilder*.

I'll do this so I can make good on that wager I made beforehand when I've finished. In the meantime, of course, if any test case provokes a failure of the parameterised unit test, then the call will throw an exception and obviously I won't get any return value at all.

It's really just there as a 'feel-good-factor' when you've got your system under test debugged to the point where all the test cases go through with a pass. The 'green-bar moment', if you know what I mean.




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

9. You need to use the assemblies built by the project 'SageSerpent.NTestCaseBuilder'. *SageSerpent.NTestCaseBuilder.dll* is the one that your project will directly reference; it has an accompanying XML file for the API documentation.

A Thought-Provoking Article you should read
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

Yes:

1. Publish this via NuGet for immediate consumption of binaries in Visual Studio.	*** DONE ***

2. Add the capability to recursively build up a tree of factories, so that the final generated test cases can be arbitrarily 'long'. The encoding example above is a case in point: it has been arbitrarily limited to just ten characters per string test case, but it should be able to produce longer and longer strings in a lazily-evaluated fashion until the test decides that it has run long enough.

4. In a similar vein, consider a progressive approach where the strength is increased and again, the sequence is produced via lazy-evaluation; the test can keep going until a time limit is reached.

3. Allow local caps on the strengh for subtrees within the tree of factories. This is because we may know that some test variables will have largely independent behaviour, so we can trade off a lower strength of combination for just these variables against having a higher overall strength of combination.

4. Integrate with Pex - smooth the path for importing Pex-generated test cases as test-levels for higher-level tests, also for integrating with Pex's notion of a parameterised test.

5. Add support for automated permuting of 'operation'-style test cases. Also take into account a variable number of operations.	*** PARTIALLY DONE ***

6. Carry on with the Scala port of this code at *sageserpent-open/fsharp-to-scala-port-case-study*. **Maybe...**

7. Return an enumerable that computes test cases asynchronously to give better throughput when the parameterised unit test is itself computationally demanding.

8. Extend merged partial test vectors into full test vectors either some or all of the time to give even more early-access full test vectors.