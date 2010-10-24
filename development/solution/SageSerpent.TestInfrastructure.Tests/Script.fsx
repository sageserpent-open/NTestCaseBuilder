#r "Z:\\Documents\\SageSerpent\\bzrRepository\\workingBranch\\development\\solution\\SageSerpent.TestInfrastructure.Tests\\..\\ThirdParty\\PowerCollections\\Binaries\\PowerCollections.dll"


open Wintellect.PowerCollections


let bag = Bag<_> [1; 2; 3; 4; 3];;

printf "bag: %A\n" bag

let bagTwo = Bag<_> [3; 3; 1; 2; 4];;

bag.IsEqualTo bagTwo

