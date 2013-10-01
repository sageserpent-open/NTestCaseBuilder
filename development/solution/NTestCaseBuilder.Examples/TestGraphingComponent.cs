using System.Threading;
using NUnit.Framework;

namespace NTestCaseBuilder.Examples
{
    [TestFixture]
    public class TestGraphingComponent
    {
        [Test]
        [RequiresSTAAttribute]  // This test needs manual interaction in order to dismiss the dialog boxes
                                // - so it shouldn't be run as part of an automated test suite.
        [Ignore]
        public void ShowSomeGraphs()
        {
            var windowToPopUp = new GraphDisplayWindow();

            windowToPopUp.ShowDialog();
        }
    }
}