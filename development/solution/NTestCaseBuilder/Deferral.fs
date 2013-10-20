namespace NTestCaseBuilder
    type Deferral =
        static member Create (deferredFactory: unit -> IFactory) =
            deferredFactory ()  // TODO: sort out completely bogus implementation!

