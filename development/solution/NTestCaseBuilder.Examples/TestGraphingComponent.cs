using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using QuickGraph;
using QuickGraph.Algorithms;
using SageSerpent.Infrastructure;

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


        private static IEnumerable<Tuple<Int32, Int32>> EnumerateConnections(IEnumerable<Boolean> connectionSwitches,
            Int32 numberOfVertices)
        {
            if (NumberOfPotentialUniqueConnectionsWithoutSelfLoops(numberOfVertices) != connectionSwitches.Count())
            {
                throw new SageSerpent.Infrastructure.LogicErrorException(
                    "Number of vertices does not correspond to the maximum number of connections possible.");
            }

            var connectionIterator = connectionSwitches.GetEnumerator();

            var counter = 0;

            while (connectionIterator.MoveNext())
            {
                // Have to avoid creating potential connections that are self-loops - see comment for 'NumberOfPotentialUniqueConnectionsWithoutSelfLoops'.

                int sourceVertexId;
                int targetVertexId;
                GetSourceAndTargetVertices(numberOfVertices, counter, out sourceVertexId, out targetVertexId);

                if (connectionIterator.Current)
                    yield return Tuple.Create(sourceVertexId, targetVertexId);

                ++counter;
            }
        }

        private static void GetSourceAndTargetVertices(int numberOfVertices, int masterIndex, out int sourceVertexId,
            out int targetVertexId)
        {
            sourceVertexId = masterIndex / (numberOfVertices - 1);
            targetVertexId = masterIndex % (numberOfVertices - 1);

            if (targetVertexId >= sourceVertexId)
            {
                targetVertexId = 1 + targetVertexId;
            }
        }

        private static int NumberOfPotentialUniqueConnectionsWithoutSelfLoops(Int32 numberOfVertices)
            // Ban self-loops; we are trying to create DAGs in the first place.
        {
            return numberOfVertices*(numberOfVertices - 1);
        }

        private static ITypedFactory<IEnumerable<Tuple<Int32, Int32>>> BuildConnectionsFactory(Int32 numberOfVertices)
        {
            var numberOfPotentialUniqueConnectionsWithoutSelfLoops =
                NumberOfPotentialUniqueConnectionsWithoutSelfLoops(numberOfVertices);

            var connectionSwitchFactory = TestVariable.Create(new[] {false, true});

            return
                Synthesis.Create(
                    Enumerable.Repeat(connectionSwitchFactory, numberOfPotentialUniqueConnectionsWithoutSelfLoops),
                    connectionSwitches => EnumerateConnections(connectionSwitches, numberOfVertices));
        }

        private static Boolean ConnectionsImplyADag(
            IEnumerable<KeyValuePair<int, Tuple<int, object>>> testVariableIndexToLevelNumberAndValueMap,
            Int32 numberOfVertices)
        {
            return
                Enumerable.Range(0, numberOfVertices)
                    .Any(
                    frozenRootingIndexToCloseOver =>
                    testVariableIndexToLevelNumberAndValueMap.All(testVariableIndexToLevelNumberAndValuePair =>
                                                                      {
                                Int32 sourceVertexId;
                                Int32 targetVertexId;
                                GetSourceAndTargetVertices(numberOfVertices,
                                    testVariableIndexToLevelNumberAndValuePair.Key, out sourceVertexId,
                                    out targetVertexId);
                                return !(Boolean) testVariableIndexToLevelNumberAndValuePair.Value.Item2 ||
                                       (numberOfVertices + sourceVertexId - frozenRootingIndexToCloseOver) %
                                                                              numberOfVertices <
                                       (numberOfVertices + targetVertexId - frozenRootingIndexToCloseOver) %
                                                                              numberOfVertices;
                                                                      }));
        }

        private static ITypedFactory<TestCase> BuildTestCaseFactory(Int32 numberOfVertices)
        {
            var connectionsFactory =
                BuildConnectionsFactory(numberOfVertices)
                    .WithFilter(dictionary => ConnectionsImplyADag(dictionary, numberOfVertices));

            var testCaseFactory = Synthesis.Create(connectionsFactory,
                connections => new TestCase() {NumberOfVertices = numberOfVertices, Connections = connections});

            return
                Interleaving.Create(new[]
                {testCaseFactory, Deferral.Create(() => BuildTestCaseFactory(1 + numberOfVertices))});
        }

        [Test]
        [RequiresSTAAttribute] // This test needs manual interaction in order to dismiss the dialog boxes
        // - so it shouldn't be run as part of an automated test suite.
        [Ignore]
        public void ShowSomeGraphs()
        {
            var factory = BuildTestCaseFactory(1).WithDeferralBudgetOf(12);

            const int maximumStrengthRequired = 2;

            factory.ExecuteParameterisedUnitTestForAllTestCases(maximumStrengthRequired, testCase =>
            {
                var graph = testCase.MakeGraph();
                if (!graph.IsDirected)
                {
                    throw new LogicErrorException(
                        "One of the aims of this example is to show the guaranteed generation of a DAG via a filter: it has failed.");
                }

                Console.WriteLine("**********");

                foreach (var vertex in graph.TopologicalSort())
                {
                    var outEdges = graph.OutEdges(vertex).ToList();
                    Console.WriteLine(outEdges.Any()
                        ? String.Format("Source vertex: {0}, leading to targets: {1}", vertex.Id,
                            outEdges.Select(edge => edge.GetOtherVertex(vertex).Id.ToString())
                                .Aggregate((lhs, rhs) => String.Format("{0}, {1}", lhs, rhs)))
                        : String.Format("Isolated vertex: {0}", vertex.Id));
                }

                var windowToPopUp = new GraphDisplayWindow {DataContext = graph};
                windowToPopUp.ShowDialog();
            });
        }
    }
}