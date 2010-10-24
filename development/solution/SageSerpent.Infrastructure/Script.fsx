#r "Z:/SageSerpent/workInProgressForSageSerpentOnly/development/solution/SageSerpent.Infrastructure/bin/Debug/SageSerpent.Infrastructure.dll"
#r "FSharp.Compiler.CodeDom.dll"

open SageSerpent.Infrastructure

// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

let a = Map.empty

let b = Map.of_list [(1, "Hi"); (3, "Rabbit!"); (2, "there")]

let o = 2:>obj

let p = BargainBasement.CrossProduct [[1;2;3];[67; -23];[0]]

let q = BargainBasement.MergeSortedListsAllowingDuplicates [1;2;3;4] [2;2;4;78]

let v1 = BargainBasement.CountDuplicatesInSortedList [1;2;3]

let v2 = BargainBasement.CountDuplicatesInSortedList [1;2;2;3]

let v3 = BargainBasement.CountDuplicatesInSortedList [2;2;2;3]

let v4 = BargainBasement.CountDuplicatesInSortedList ([]: int list)

let v5 = BargainBasement.CountDuplicatesInSortedList [1;2;2;2]

let v6 = BargainBasement.CountDuplicatesInSortedList [2;2;2;2]

let v7 = BargainBasement.CountDuplicatesInSortedList [1]

let v8 = BargainBasement.CountDuplicatesInSortedList [1;2]

let q1 = BargainBasement.BreakOff 2u [3;4;7;8];;

let q2 = BargainBasement.ChopUpList [3;4;7;8] [1u;2u];;

let q3 = BargainBasement.BreakOff 4u [3;4;7;8];;

let q4 = BargainBasement.ChopUpList [3;4;7;8] [];;

let r = RandomBehaviour 2;;

r.ChooseAnyNumberFromOneTo 10u;;


let r2 = BargainBasement.PartitionItemsIntoSubgroupsOfRandomNonZeroLength [1u .. 10u] 3u r;;

let result = BargainBasement.ChooseCombinationsOfItems [2] 0u;;


let simpleQuote = <@ 2 @>


let create () =
    let notSoSimpleQuote = <@ [] @>
    notSoSimpleQuote
    
type MyVeryOwnJustin = int * int *string
    
open System.CodeDom

let typeRepresentation = CodeTypeReference (typeof<MyVeryOwnJustin>)

let codeNamespace = CodeNamespace "_quiggly_"

let codeCompileUnit = CodeCompileUnit ()

codeCompileUnit.Namespaces.Add codeNamespace

let codeTypeDeclaration = CodeTypeDeclaration "Vabbing"

codeNamespace.Types.Add codeTypeDeclaration

let codeMemberMethod = CodeMemberMethod ()

codeMemberMethod.Name <- "Foo"

codeMemberMethod.ReturnType <- CodeTypeReference (typeof<Unit>)

codeMemberMethod.Parameters.Add (CodeParameterDeclarationExpression (typeRepresentation, "argument"))

let codeMethodReturnStatement = CodeMethodReturnStatement ()

codeMethodReturnStatement.Expression <- CodeObjectCreateExpression (codeMemberMethod.ReturnType, [||])

codeMemberMethod.Statements.Add codeMethodReturnStatement

codeTypeDeclaration.Members.Add codeMemberMethod

open Microsoft.FSharp.Compiler.CodeDom


let compileItAll () =
    use codeProvider =
        new Microsoft.CSharp.CSharpCodeProvider ()
    codeProvider.GenerateCodeFromCompileUnit (codeCompileUnit, System.Console.Out, Compiler.CodeGeneratorOptions ())
    let compilerParameters = Compiler.CompilerParameters (GenerateInMemory = true)
    compilerParameters.GenerateInMemory <- true
    let compilationResults = codeProvider.CompileAssemblyFromDom (compilerParameters, [| codeCompileUnit |])
    printf "%A\n" compilationResults.Errors
    let generatedAssembly = compilationResults.CompiledAssembly
    
    let generatedClass = generatedAssembly.GetType "Vabbing"
    
    let instanceOfGeneratedClass = System.Activator.CreateInstance generatedClass
    
    printf "%A\n" instanceOfGeneratedClass
 
 
let someFunction x =
    1u :: [ x ]
    
let theType = someFunction.GetType ()

printf "%A\n" (theType.GetMembers ())






