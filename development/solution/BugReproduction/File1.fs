#light

namespace Foo

    open System
    
    type Node =
        TestVariableNode of seq<Object>
      | InterleavingNode of seq<Node>
      | SynthesizingNode of seq<Node> * Delegate


