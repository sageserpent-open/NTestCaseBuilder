#light

module SageSerpent.Infrastructure.BargainBasement

let rec CrossProduct sequences =
            match sequences with
                [] -> [[]]
              | head::tail -> let crossProductOfTail = CrossProduct tail
                              [for itemInHead in head do
                                for itemInCrossProductOfTail in crossProductOfTail do
                                    yield itemInHead::itemInCrossProductOfTail] 