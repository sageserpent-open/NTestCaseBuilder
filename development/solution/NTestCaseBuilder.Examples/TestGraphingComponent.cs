using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Microsoft.FSharp.Collections;
using NUnit.Framework;

namespace NTestCaseBuilder.Examples
{
    [TestFixture]
    public class TestGraphingComponent
    {
        private class TestCase
        {
            public Int32 NumberOfVertices { get; set; }

            public IEnumerable<Tuple<Int32, Int32>> Connections { get; set; }

            public Graph MakeGraph()
            {
                var graph = new Graph();

                var vertices = (from id in Enumerable.Range(0, NumberOfVertices) select new Vertex {Id = id}).ToArray();

                foreach (var vertex in vertices)
                {
                    graph.AddVertex(vertex);
                }

                foreach (var connection in Connections)
                {
                    graph.AddEdge(new Edge(vertices[connection.Item1], vertices[connection.Item2]));
                }

                return graph;
            }
        }

        private static IEnumerable<Tuple<Int32, Int32>> EnumerateConnections(IEnumerable<Boolean> connectionSwitches, Int32 numberOfVertices)
        {
            if (numberOfVertices * numberOfVertices != connectionSwitches.Count())
            {
                throw new SageSerpent.Infrastructure.LogicErrorException(
                    "Number of vertices does not correspond to the maximum number of connections possible.");
            }

            var connectionIterator = connectionSwitches.GetEnumerator();

            var counter = 0;

            while(connectionIterator.MoveNext())
            {
                var sourceIndex = counter / numberOfVertices;
                var targetIndex = counter % numberOfVertices;

                if (connectionIterator.Current)
                    yield return Tuple.Create(sourceIndex, targetIndex);

                ++counter;
            }
        }

        private static TypedTestCaseEnumerableFactory<IEnumerable<Tuple<Int32, Int32>>> BuildConnectionsFactory(
            Int32 numberOfVertices)
        {
            var maximumNumberOfConnections = numberOfVertices * numberOfVertices;

            var connectionSwitchFactory = TestVariableLevelEnumerableFactory.Create(new[] {false, true});

            return SynthesizedTestCaseEnumerableFactory.Create(Enumerable.Repeat(connectionSwitchFactory, maximumNumberOfConnections), (SequenceCondensation<Boolean, IEnumerable<Tuple<Int32, Int32>>>)(connectionSwitches => EnumerateConnections(connectionSwitches, numberOfVertices)));
        }

        private static TypedTestCaseEnumerableFactory<TestCase> BuildTestCaseFactory(Int32 maximumNumberOfVertices)
        {
            var numberOfVertices = maximumNumberOfVertices;

            var connectionsFactory = BuildConnectionsFactory(numberOfVertices);

            var testCaseFactory = SynthesizedTestCaseEnumerableFactory.Create(connectionsFactory,
                                                                              connections =>
                                                                              new TestCase()
                                                                                  {
                                                                                      NumberOfVertices = numberOfVertices,
                                                                                      Connections = connections
                                                                                  });

            return 0 == maximumNumberOfVertices
                       ? testCaseFactory
                       : InterleavedTestCaseEnumerableFactory.Create(new[]
                                                                         {
                                                                             testCaseFactory,
                                                                             BuildTestCaseFactory(maximumNumberOfVertices -
                                                                                                  1)
                                                                         });
        }

        [Test]
        [RequiresSTAAttribute] // This test needs manual interaction in order to dismiss the dialog boxes
        // - so it shouldn't be run as part of an automated test suite.
        [Ignore]
        public void ShowSomeGraphs()
        {
            var factory = BuildTestCaseFactory(5);

            const int maximumStrengthRequired = 2;

            factory.ExecuteParameterisedUnitTestForAllTypedTestCases(maximumStrengthRequired, testCase =>
                                                                                                  {
                                                                                                      var windowToPopUp
                                                                                                          =
                                                                                                          new GraphDisplayWindow
                                                                                                              {
                                                                                                                  DataContext
                                                                                                                      =
                                                                                                                      testCase
                                                                                                                      .
                                                                                                                      MakeGraph
                                                                                                                      ()
                                                                                                              };
                                                                                                      windowToPopUp.
                                                                                                          ShowDialog();
                                                                                                  });
        }
    }
}