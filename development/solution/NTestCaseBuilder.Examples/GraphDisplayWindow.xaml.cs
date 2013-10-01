using System.Linq;
using GraphSharp.Controls;

namespace NTestCaseBuilder.Examples
{
    internal class GraphLayout : GraphLayout<Vertex, Edge, Graph>
    {
    }

    internal class Model
    {
        public Model()
        {
            var graph = new Graph();

            var vertices = (from id in Enumerable.Range(0, 4) select new Vertex {Id = id}).ToArray();

            foreach (var vertex in vertices)
            {
                graph.AddVertex(vertex);
            }

            graph.AddEdge(new Edge(vertices[0], vertices[0]));
            graph.AddEdge(new Edge(vertices[1], vertices[3]));

            Graph = graph;
        }

        public Graph Graph { get; set; }

        public string LayoutAlgorithmType
        {
            get { return "ISOM"; }
        }
    }

    /// <summary>
    ///   Interaction logic for GraphDisplayWindow.xaml
    /// </summary>
    public partial class GraphDisplayWindow
    {
        public GraphDisplayWindow()
        {
            DataContext = new Model();

            InitializeComponent();
        }
    }
}