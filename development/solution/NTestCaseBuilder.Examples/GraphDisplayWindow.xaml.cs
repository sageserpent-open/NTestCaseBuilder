using System.Linq;
using GraphSharp.Controls;

namespace NTestCaseBuilder.Examples
{
    internal class GraphLayout : GraphLayout<Vertex, Edge, Graph>
    {
    }

    /// <summary>
    ///   Interaction logic for GraphDisplayWindow.xaml
    /// </summary>
    public partial class GraphDisplayWindow
    {
        public GraphDisplayWindow()
        {
            DataContext = new Graph();

            InitializeComponent();
        }
    }
}