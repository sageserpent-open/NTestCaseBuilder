using System;
using NTestCaseBuilder;
using NUnit.Framework;

namespace NTestCaseBuilder.Examples
{
    [TestFixture]
    internal class TestBinaryOperatorExpressions
    {
        private static readonly ITypedFactory<Char> OperatorFactory = TestVariable.Create(new[] {'+', '-', '*', '/'});

        private static readonly ITypedFactory<String> ConstantFactory = TestVariable.Create(new[] {"0", "1", "2"});

        private static ITypedFactory<String> BuildFactoryRecursively()
        {
            var binaryOperatorFactory = Synthesis.Create(Deferral.Create<String>(BuildFactoryRecursively),
                                                         OperatorFactory,
                                                         Deferral.Create<String>(BuildFactoryRecursively),
                                                         (lhsOperand, binaryOperator, rhsOperand) =>
                                                         String.Format("({0}) {1} ({2})", lhsOperand, binaryOperator,
                                                                       rhsOperand));

            return Interleaving.Create(new[] {ConstantFactory, binaryOperatorFactory});
        }

        [Test]
        public void FireUpBinaryOperatorExpressions()
        {
            const Int32 maximumDepth = 6;

            var expressionFactory = BuildFactoryRecursively().WithDeferralBudgetOf(maximumDepth);

            const Int32 strength = 3;

            expressionFactory.ExecuteParameterisedUnitTestForAllTestCases(strength,
                                                                          (testCase =>
                                                                           Console.Out.WriteLine(testCase)));
        }
    }
}
