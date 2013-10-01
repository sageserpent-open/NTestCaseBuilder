using System.Linq;
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
            var graph = new Graph();

            var vertices = (from id in Enumerable.Range(0, 4) select new Vertex {Id = id}).ToArray();

            foreach (var vertex in vertices)
            {
                graph.AddVertex(vertex);
            }

            graph.AddEdge(new Edge(vertices[0], vertices[0]));
            graph.AddEdge(new Edge(vertices[1], vertices[3]));

            var windowToPopUp = new GraphDisplayWindow { DataContext = graph };

            windowToPopUp.ShowDialog();
        }
    }
}