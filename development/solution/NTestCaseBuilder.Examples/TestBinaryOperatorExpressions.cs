using System;
using NUnit.Framework;

namespace NTestCaseBuilder.Examples
{
    [TestFixture]
    internal class TestBinaryOperatorExpressions
    {
        private static readonly ITypedFactory<Char> BinaryOperatorFactory =
            TestVariable.Create(new[] {'+', '-', '*', '/'});

        private static readonly ITypedFactory<String> ConstantFactory = TestVariable.Create(new[] {"0", "1", "2"});

        private static ITypedFactory<String> BuildExpressionFactoryRecursively()
        {
            var subexpressionFactory =
                Interleaving.Create(new[]
                                        {
                                            ConstantFactory,
                                            Synthesis.Create(
                                                Deferral.Create<String>(BuildExpressionFactoryRecursively),
                                                expression => String.Format("({0})", expression))
                                        });

            var binaryOperatorExpressionFactory =
                Synthesis.Create(subexpressionFactory,
                                 BinaryOperatorFactory,
                                 subexpressionFactory,
                                 (lhsOperand, binaryOperator, rhsOperand) =>
                                 String.Format("{0} {1} {2}", lhsOperand, binaryOperator,
                                               rhsOperand));

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
                                                                              (testCase =>
                                                                               Console.Out.WriteLine(testCase)));
            Console.Out.WriteLine("Exercised {0} test cases.", numberOfTestCasesExercised);
        }
    }
}