#light

namespace Foo

    open System

    type TestVariableLevelEnumeratorFactory (levels: seq<Object>) =
        inherit TestCaseEnumeratorFactoryCommonImplementation ()
        let node =
           Foo.TestVariableNode levels





module Throwaway =

    let foo =
        TestVariableLevelEnumeratorFactory Seq.empty