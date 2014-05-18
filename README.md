NTestCaseBuilder, for .NET Testing
==================================

This is a .NET library that generates sets of test cases, for use by parameterised tests.

Think of it as being a DSL for making 'object mothers', whose generated data can be used as test cases (or for any other purpose, for that matter).

Each test case is built up progressively from smaller pieces of data that are combined together.

The smallest pieces are referred to as 'test variables'. Each test variable is constrained to take values from its own set of allowed 'levels'.

Changing each test variable independently to a new level produces a new test case.

If a test requires a certain 'strength' of coverage of combinations of levels from different test variables, the library produces a stream of test cases that honour this guarantee while avoiding getting swamped with a combinatoric explosion of test cases.

If a parameterised test fails with an exception for a specific test case, the library creates a signature that allows it to reproduce the failing test case immediately in a separate test; this is to aid debugging.

The sources are written in F#, but the API can be used just as comfortably from C# as from F#.

Samples and Documentation
-------------------------

Take a look at the [NTestCaseBuilder wiki](https://github.com/sageserpent-open/NTestCaseBuilder/wiki).


License
-------

The MIT License (MIT)

Copyright (c) 2014 Gerard Murphy, SageSerpent Ltd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


