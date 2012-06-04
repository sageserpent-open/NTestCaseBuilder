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
Can we cycle back by calling 'DoThis()' *after* 'DoThat()'? Thereby causing a lock-in at the bar, one presumes?

Now, one can (and hopefully does) write additional unit tests in this style to explore the various edge-cases and flesh out the parts of the behaviour that *must* be specified to client code. (Remember, it may be the case that client code doesn't care about these edge cases, although in this situation it is better to design the API so as to make these irrelevant cases impossible to exercise).

However, these tests are really about documenting behaviour - while they *do* act as a basic safety net against obvious breakage of the component as the implementation is refactored, extended or otherwise modified, they aren't good enough to consider signing off against.


There is also another problem in that this style of unit test doesn't scale well as we move away from the lowest level of components that form the roots of an application's software component dependencies, proceeding up the dependency chains to test components that are composed of, or otherwise rely on, simpler components.

It becomes progressively more difficult to set up the component under test because of the increasing number of dependency components that have to be correctly set up for the component under test to run. In effect, a unit test for a higher-level component is kind of integration test for that component's dependencies.

The traditional solution to this to mock the dependency components, which ensures that the higher-level component really is being unit-tested. This approach is well and good, and can also point the way to effective design of the dependencies between an application's components.

What is doesn't do however, is to provide confidence that the higher-level component is being genuinely tested - most tests using mocks tend to exaggerate the problem above, where only a specific corner of the component's behaviour is tested. Because most mocks rely on canned recordings of method calls (for behavioural mocks) or have deliberately trivial state (for state-based mocks), any variation in behaviour due to the component's dependencies is swept under the carpet.

So mocks are good for being able to quickly write 'executable documentation' unit tests for higher-level components, and they are good for highlighting a path to expressing dependencies in an application's design. They are however found wanting when it comes to using tests to help prove correctness.


This leads to the next stage, which is to write parameterised unit tests...

A parameterised unit test is written as a function with a single parameter passed to it: the parameter contains all the information needed to completely set up both the component under test and the sequence of actions to be performed on it. This also includes a presecription for setting up any component dependencies required by the component under test.

The idea is to repeatedly run the function with variations of the parameter in order to flush out breakages of the unit test that only occur for some state of the component under test, or the dependency components that it is built from / interacts with in the test, or for some specific variation of the sequence of operations performed by the test.

So for the example above, our parameter object would describe:-

1. Whether or not the 'Foo' is created with a 'Bar'.
2. If the latter, whether the 'Bar' is initially open or not. If open, whether it is at last orders or not.
3. The sequence of operations applied to the 'Foo' - for each stage in the sequence, whether to call either 'DoThis', or 'DoThat' and then carry on to a following operation, or alternatively to simply finish the test.

Omitting some of the detail, the example above becomes:

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

How is the 'Foo' instance built up? We have a choice here - we could use the parameterless constructor for 'Foo', or perhaps build up or own 'Bar' instance and supply it to an alternative constructor for 'Foo' (not shown above). So to cover both situations, we need to express an *interleaving* where a 'Foo' can be built in more than one way.

Looking at the sequence of 'Operation' instances, the sequence itself can be built up by taking a fixed number of operations and synthesizing a sequence from them. For now, let's fix this at 4 operations; we will see later how to loosen this rather arbitrary choice to get sequences of varying length.

This leaves the operations themselves - for each of the 4 slots in the sequence, there is a choice of whether the operation is a 'DoThis' or a 'DoThat' enumeration constant. In effect, each slot is a *test variable* that can be set to one of two distinct *levels*.

How do we make our alternate 'Foo' instances? One way is to call the parameterless constructor for 'Foo'. In this case, there is nothing to vary - we just get a 'Foo' without being able to play with its configuration: we can think of this as being a data *singleton*, because any overall test case that uses one of these 'Foo' instances should get the same value of 'Foo' each time (we assume that 'Foo' is well-behaved, so successive calls to the parameterless constructor yield what are effectively copies of the same object, sharing the same subsequent behaviour).

Note that unlike the usual usage of the work 'singleton', we do not insist that the singleton object actually be the same object reference each time - merely we that can't get any initial variation when we ask for one.

The other way of making a 'Foo' is to supply a 'Bar' - if we have a constructor (not shown above) for 'Bar' that takes a parameter describing whether the 'Bar' is closed, doing normal business or is taking last orders, then we could represent this as a synthesis of a 'Foo' from a 'Bar' which in turn is synthesized from a test variable with three levels.

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

1. It produces a sequence of varying test cases, for direct consumption in a parameterised unit test.

2. It can participate in the composite design pattern, becoming part of a larger tree of factories whose root produces more complex test cases.


Moving on, let's look at the test variables that contribute to the making of a 'TestCase' above - we have 4 test variables * 2 levels for the operation slots, plus 1 test variable * 3 levels for the parameterised 'Bar' constructor. Note that the singleton doesn't count - as it always yields a non-varying data value, there is essentially no test variable for it.

That means there are potentially 4 * 2 * 1 * 3 = 24 different test cases to produce.

That wasn't really a complex example - one can realistically imagine, say 20 test variables with 5 levels - meaning 5^20 = 95367431640625 test cases would result.

That's rather a lot of test cases - do we really need all of them?


Let's think about how likely a bug is going to be manifest when we run our parameterised test repeatedly over all 95367431640625 test cases. Let's say that we've already fixed the low-hanging bugs that occur straightaway for just about any choice of test case.

When I say 'low-hanging bugs', I mean things like:-

1. Forgetting the implement the stubbed code you wrote when you were coding 'test-first' in a language with static typing.
2. The division by zero one of your colleagues put in for a joke while you took a comfort break.
3. The off by one error in a loop that was definitely your fault.
4. The lack of initialisation that the compiler warned you about, but of course, we all know better than to trust diagnostic advice from a compiler.
5. The silly mistake in the implementation that was caused by a cosmic ray hitting a brain cell while typing a line of code.

Typically, a smoke test will expose most of these kinds of errors (and this is why it's still worth writing smoke tests in the early stages of implementation, assuming you haven't written any kind of test with expectations).

So at this point we have an implementation for our component under test that seems to work - it would certainly pass a smoke test coded in the style shown right at the beginning, and would probably survive an 'executable-documentation' unit test too.

We then expect that our parameteried unit test will be repeatedly called with each new test case, and will repeatedly succeed, until at some point - **KERBOOM**: Test failure!

So the failing test case contains a magic combination of levels for its test variables that cause the component to fail. Is it likely that **all** of the test variable contribute to the failure? All 20 of them?

It's safe to assume - at least as a starting point - that maybe only one, two, three or four test variables contribute the magic combination of levels that cause the failure. All of the other test variables could have their levels set to whatever choices we'd like and we'd still get a failure.

So we could thin out the number of test cases considerably - say we had a failure requiring a specific combination of levels from four test variables - we could find the failing test case within 5^4 = 625 repeats at most - much better!

The problem is that we don't know which of the 20 test variables are the ones that can act in concert to expose the bug.

However, imagine that we could generate a sequence of 'incomplete' test case using only the first four test variables - that would give us 625 test cases with holes in them where we haven't yet decided what levels to set the remaining test variables to.

Perhaps we could then build another parallel sequence of incomplete test cases using the next four test variables - again that would give is 625 test cases with different holes in them, including the test variables that we filled in in the previous sequence.

So merging the two sequences of test cases together would provide a new sequence of test cases that would include all combinations of test levels from the first four test variables and all combinations of test levels from the next four test variables. Because we merge corresponding test cases from the two sequences, the resulting sequence length is still 625!

Carrying on with this procedure would yield a sequence of 625 test cases that would cover combinations of test levels from successive groups of four test variables - so for the much lower effort of examining up to 625 test cases, we have some chance of finding the same bug that we would have had to trawl through 95367431640625 test cases!

Great - but there is still a problem. I forgot to mention that possibility that it might be combinations of levels from test variables from *across groups of four* that cause the bug. In other words, maybe it is test variable #2 with level #5, test variable #9 with level #3, test variable #18 with level #3 and test variable #20 with level #1 that exposes the bug.

The procedure is only systematic about combinations of levels from test variables in the same group, so it won't necessarily generate the magic test case; but it is a tantalising idea: could we somehow produce a much smaller sequence of test cases than the full 95367431640625, and still guarantee that *for any choice of one, two, three or four test variables*, the sequence would *guarantee that every combination of levels for those variables* would be completely covered.

Think about that last statement - the guarantee isn't merely that there is *some* choice of test variables whose level combinations are covered - it is saying that *any* choice of test variables you ask for has its level combinations covered; from within the same sequence of test cases.

This is the guarantee provided by any of the factories mentioned above - a factory can if given a *strength* produce a sequence of test case that provides that guarantee for any number of test variables up to and including the given strength.

As the strength is increased, the factory has to work harder to meet this guarantee - so if we set the strength all the way up to 20 in this example, we are back to generating all 95367431640625 test cases. In practice, strengths of up to 4 are probably good enough.

Note that when we use factories are part of the composite design pattern, there is no obvious relationship between the sequences of simpler test cases produced by factories in subtrees and the sequence of complex test cases produced by the overall root factory. This is because for a given strength, there are an increasing number of ways in which combinations of test variable levels can be overlaid into the same test case.

The exception is when we ask for the full strength including all test variables and we only use synthesizing factories: in this case we can think of the sequence of the test cases made by the root factory as being a cross-product of the levels taking from all the test variables at the leaves of the tree of factories.

However, we can say that as the number of test cases in the sequence goes up the closer to the root factory we go, it gets increasingly 'thinner' taken relative to the full cross-product of levels taken from all test variables. The amount of thinning as we go up the tree is more dramatic for lower strengths.


So we've met the players - to recap, we have:-

1. A test case: structured data, composed out of simpler pieces.

2. A test variable: the simplest piece of data that can show variation between one test case and another.

3. Levels: these are the values that a test variable is allowed to take.

4. A singleton: the simplest piece of data that can never vary from one test case to another.

5. Factories: these produce sequences of test cases that guarantee coverage of combinations of test variable levels according to a given strength. These can be combined according to the composite design pattern to make a tree structure.

6. A synthesizing factory: puts together the kinds of test cases produced by its child factories to make more complex test cases. It should be an interior node in any tree of factories. 

7. An interleaving factory: interleaves the kinds of test cases produced by its child factories to give alternatives in its sequence. It should be an interior node in any tree of factories.

8. A test variable level factory: introduces a test variable and its levels into a factory tree. It is always a leaf node in any tree of factories.

9. A singleton factory: introduces a singleton data value into a factory tree. Does not affect the strength guarantee because it has no test variable - which is why it is preferable to making a faux test variable factory with a single level. It is always a leaf node in any tree of factories.

10. A strength - the strength of guarantee a factory makes about the coverage of combinations of levels from different test variables. The higher the strength, the more test variables a combination can refer to in the guarantee - and the longer the sequence of test cases will be to meet that guarantee.

NOTE: testing with a strength of 2 is commonly known as 'pairwise testing'.

Walk me through an example!
---------------------------

Uh-huh.

How do I install this thing?
----------------------------

Uh-huh.

A Thought-Provoking Article you should read
-------------------------------------------

[http://www.testingeducation.org/wtst5/PairwisePNSQC2004.pdf](http://www.testingeducation.org/wtst5/PairwisePNSQC2004.pdf).

There are several points that the article makes, but ones which can be addressed here are:-

1. If there are certain test cases that are much more probable than others, it is possible to make this explicit by setting up the tree of factories with an interleaving factory. One child of the interleave is the conventional synthesis of a test case from a multitude of test variables and their levels; the other can be a test variable level factory that supplies special cases that are much more probable in practice and would be missed by low-strength testing. These special cases can themselves by synthesized from a special-case test variables if desired. What would be nice would be the ability to specify a very high strength of combination to apply to the special case part of the interleave, but revert back to say, a strength of 2 on the ordinary part of the interleave to get pairwise testing for the other test variables - this is on the current todo list below.

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

1. Publish this via NuGet for immediate consumption of binaries in Visual Studio.

2. Add the capability to recursively build up a tree of factories, so that the final generated test cases can be arbitrarily 'long'. The encoding example above is a case in point: it has been arbitrarily limited to just ten characters per string test case, but it should be able to produce longer and longer strings in a lazily-evaluated fashion until the test decides that it has run long enough.

4. In a similar vein, consider a progressive approach where the strength is increased and again, the sequence is produced via lazy-evaluation; the test can keep going until a time limit is reached.

3. Allow local caps on the strengh for subtrees within the tree of factories. This is because we may know that some test variables will have largely independent behaviour, so we can trade off a lower strength of combination for just these variables against having a higher overall strength of combination.

4. Integrate with Pex - smooth the path for importing Pex-generated test cases as test-levels for higher-level tests, also for integrating with Pex's notion of a parameterised test.

5. Add support for automated permuting of 'operation'-style test cases. Also take into account a variable number of operations.

6. Carry on with the Scala port of this code at *sageserpent-open/fsharp-to-scala-port-case-study*. **Maybe...**