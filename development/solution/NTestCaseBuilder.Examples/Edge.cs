using System;
using System.Diagnostics;

namespace NTestCaseBuilder.Examples
{
    [DebuggerDisplay("Source: {Source.Id}, Destination: {Target.Id}")]
    public class Edge: QuickGraph.Edge<Vertex>
    {
        public Edge(Vertex source, Vertex target): base(source, target)
        {

        }

        public Tuple<Int32, Int32> Connection
        {
            get { return Tuple.Create(Source.Id, Target.Id); }
        }
    }
}