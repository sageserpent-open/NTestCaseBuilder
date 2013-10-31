using System;
using NTestCaseBuilder;
using NUnit.Framework;

namespace NTestCaseBuilder.Examples
{
    [TestFixture]
    internal class TestCalculator
    {
        private static readonly ITypedFactory<Char> BinaryOperatorFactory =
            TestVariable.Create(new[] {'+', '-', '*', '/'});

        private static readonly ITypedFactory<Tuple<Boolean, String>> ConstantFactory =
            Synthesis.Create(TestVariable.Create(new[] {"0", "1", "2"}), constant => Tuple.Create(false, constant));

        private static Tuple<Boolean, String> BinaryExpressionFrom(Tuple<Boolean, String> lhs,
                                                                   Tuple<Boolean, String> rhs,
                                                                   Char binaryOperator)
        {
            switch (binaryOperator)
            {
                case '*':
                case '/':
                    {
                        var lhsWithCorrectPrecendence =
                            lhs.Item1 ? String.Format("({0})", lhs.Item2) : lhs.Item2;
                        var rhsWithCorrectPrecendence =
                            rhs.Item1 ? String.Format("({0})", rhs.Item2) : rhs.Item2;

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
                    Deferral.Create(() => BuildExpressionFactoryRecursively(directlyToTheRightOfABinaryOperator)),
                    Deferral.Create(() => BuildExpressionFactoryRecursively(true)),
                    BinaryOperatorFactory,
                    BinaryExpressionFrom
                    );

            var nonNegatedExpressionFactory =
                Interleaving.Create(new[] {ConstantFactory, binaryOperatorExpressionFactory});

            return
                Interleaving.Create(new[]
                                        {
                                            nonNegatedExpressionFactory,
                                            Synthesis.Create(
                                                Deferral.Create(() => BuildExpressionFactoryRecursively(true)),
                                                expression =>
                                                Tuple.Create(
                                                    expression.Item1,
                                                    String.Format(
                                                        directlyToTheRightOfABinaryOperator ? "(-{0})" : "-{0}",
                                                        expression.Item2))),
                                            Synthesis.Create(
                                                Deferral.Create(() => BuildExpressionFactoryRecursively(false)),
                                                expression =>
                                                Tuple.Create(false,
                                                             String.Format("({0})", expression.Item2)))
                                        });
        }

        [Test]
        public void FireUpCalculators()
        {
            const Int32 maximumDepth = 3;

            var expressionFactory = BuildExpressionFactoryRecursively(false).WithDeferralBudgetOf(maximumDepth);

            const Int32 strength = 2;

            var numberOfTestCasesExercised =
                expressionFactory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                                                                              (testCase =>
                                                                               Console.Out.WriteLine(testCase.Item2)));
            Console.Out.WriteLine("Exercised {0} test cases.", numberOfTestCasesExercised);
        }
    }
}