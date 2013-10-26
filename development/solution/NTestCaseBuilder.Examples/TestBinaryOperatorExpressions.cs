using System;
using NTestCaseBuilder;
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
            var binaryOperatorExpressionFactory =
                Synthesis.Create(Deferral.Create<String>(BuildExpressionFactoryRecursively),
                                 BinaryOperatorFactory,
                                 Deferral.Create<String>(BuildExpressionFactoryRecursively),
                                 (lhsOperand, binaryOperator, rhsOperand) =>
                                 String.Format("({0}) {1} ({2})", lhsOperand, binaryOperator,
                                               rhsOperand));

            return Interleaving.Create(new[] {ConstantFactory, binaryOperatorExpressionFactory});
        }

        [Test]
        public void FireUpBinaryOperatorExpressions()
        {
            const Int32 maximumDepth = 3;

            var expressionFactory = BuildExpressionFactoryRecursively().WithDeferralBudgetOf(maximumDepth);

            const Int32 strength = 3;

            expressionFactory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                                                                          (testCase =>
                                                                           Console.Out.WriteLine(testCase)));
        }
    }
}