#light

namespace Foo

    open System.Collections
    

    type ITestCaseEnumeratorFactory =
        inherit INodeWrapper
        
        abstract member CreateEnumerator: uint32 -> IEnumerator;
        
        abstract member MaximumStrength: System.UInt32;
