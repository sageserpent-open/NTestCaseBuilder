using System;
using System.Collections.Generic;
using System.Linq;
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
                int targetIndex;
                int sourceIndex;
                GetSourceAndTargetIndices(numberOfVertices, counter, out targetIndex, out sourceIndex);

                if (connectionIterator.Current)
                    yield return Tuple.Create(sourceIndex, targetIndex);

                ++counter;
            }
        }

        private static void GetSourceAndTargetIndices(Int32 numberOfVertices, Int32 masterIndex, out Int32 targetIndex,
                                                      out Int32 sourceIndex)
        {
            sourceIndex = masterIndex / (numberOfVertices - 1);
            targetIndex = masterIndex % (numberOfVertices - 1);

            if (targetIndex == sourceIndex)
            {
                targetIndex = (targetIndex + 1) % numberOfVertices;
            }
        }

        private static int NumberOfPotentialUniqueConnectionsWithoutSelfLoops(Int32 numberOfVertices)
            // Ban self-loops, as Graph# can't render them right now.
        {
            return numberOfVertices * (numberOfVertices - 1);
        }

        private static TypedFactory<IEnumerable<Tuple<Int32, Int32>>> BuildConnectionsFactory(Int32 numberOfVertices)
        {
            var numberOfPotentialUniqueConnectionsWithoutSelfLoops =
                NumberOfPotentialUniqueConnectionsWithoutSelfLoops(numberOfVertices);

            var connectionSwitchFactory = TestVariable.Create(new[] {false, true});

            return
                Synthesis.Create(
                    Enumerable.Repeat(connectionSwitchFactory, numberOfPotentialUniqueConnectionsWithoutSelfLoops),
                    (SequenceCondensation<Boolean, IEnumerable<Tuple<Int32, Int32>>>)
                    (connectionSwitches => EnumerateConnections(connectionSwitches, numberOfVertices)));
        }

        private static Boolean FilterOutNonDagCases(
            IEnumerable<KeyValuePair<int, Tuple<int, object>>> testVariableIndexToLevelNumberAndValueMap,
            Int32 numberOfVertices)
        {
            return
                Enumerable.Range(0, numberOfVertices).Any(
                    frozenRootingIndexToCloseOver =>
                    testVariableIndexToLevelNumberAndValueMap.All(testVariableIndexToLevelNumberAndValuePair =>
                                                                      {
                                                                          Int32 sourceIndex;
                                                                          Int32 targetIndex;
                                                                          GetSourceAndTargetIndices(numberOfVertices,
                                                                                                    testVariableIndexToLevelNumberAndValuePair
                                                                                                        .Key,
                                                                                                    out sourceIndex,
                                                                                                    out targetIndex);
                                                                          return
                                                                              !(Boolean)
                                                                               testVariableIndexToLevelNumberAndValuePair
                                                                                   .Value.Item2 ||
                                                                              (sourceIndex -
                                                                               frozenRootingIndexToCloseOver) %
                                                                              numberOfVertices <
                                                                              (targetIndex -
                                                                               frozenRootingIndexToCloseOver) %
                                                                              numberOfVertices;
                                                                      }));
        }

        private static TypedFactory<TestCase> BuildTestCaseFactory(Int32 maximumNumberOfVertices)
        {
            var numberOfVertices = maximumNumberOfVertices;

            var connectionsFactory = BuildConnectionsFactory(numberOfVertices);

            var testCaseFactory = Synthesis.Create(connectionsFactory,
                                                   connections =>
                                                   new TestCase()
                                                       {NumberOfVertices = numberOfVertices, Connections = connections});

            var testCaseFactoryWithFilter =
                testCaseFactory.WithFilterTyped(dictionary => FilterOutNonDagCases(dictionary, numberOfVertices));

            return 0 == maximumNumberOfVertices
                       ? testCaseFactory
                       : Interleaving.Create(new[]
                                                 {
                                                     testCaseFactoryWithFilter,
                                                     BuildTestCaseFactory(maximumNumberOfVertices - 1)
                                                 });
        }

        [Test]
        [RequiresSTAAttribute] // This test needs manual interaction in order to dismiss the dialog boxes
        // - so it shouldn't be run as part of an automated test suite.
        [Ignore]
        public void ShowSomeGraphs()
        {
            var factory = BuildTestCaseFactory(12);

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