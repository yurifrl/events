module ReporterSpec
  ( spec
  ) where

import Test.Hspec

import Reports (Event(..), Aggregate(..), aggregate, report)

spec :: Spec
spec = do
  describe "Report spec" $ do
    let event = Event
                  "2011-12-03T10:15:30Z"
                  4285
                  "SLICE_IT"
                  "1bb53ed1-787b-4543-9def-ea18eef7902e"
    
    describe "report" $ do
      it "can filter by Number of purchases per hour, broken down by amount bracket  ..." $ do
         print $ report (aggregate [event])
         -- report (aggregate [event]) `shouldMatchList` [Agg "2011-12-03:10|10-50" 1]
      
      --it "can filter by Number of purchases per hour, broken down by amount bracket and payment method" $ do
        -- report (aggregate [event]) `shouldMatchList` [Agg "2011-12-03:10|10-50|SLICE_IT" 1]
        
      
    describe "Aggregate" $ do
      it "can aggregate single event" $ do
        aggregate [event] `shouldMatchList` [ Agg "2011-12-03:10|10-50" 1
                                            , Agg "2011-12-03:10|10-50|SLICE_IT" 1
                                            , Agg "10-50|SLICE_IT" 1
                                            , Agg "2011-12-03|1bb53ed1-787b-4543-9def-ea18eef7902e" 1
                                            , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|SLICE_IT" 1
                                            ]
      it "should generate the correct number of aggregates" $ do
        let aggs = aggregate [event, event]
        length aggs `shouldBe` 5
      
      it "can aggregate multiple events" $ do
        aggregate [event, event] `shouldMatchList`
            [ Agg "2011-12-03:10|10-50" 2
            , Agg "2011-12-03:10|10-50|SLICE_IT" 2
            , Agg "10-50|SLICE_IT" 2
            , Agg "2011-12-03|1bb53ed1-787b-4543-9def-ea18eef7902e" 2
            , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|SLICE_IT" 2
            ]
           
      it "can aggregate multiple events" $ do
        let events = [ Event "2011-12-03T10:15:30Z" 4285 "SLICE_IT" "1bb53ed1-787b-4543-9def-ea18eef7902e"
                     , Event "2011-12-03T12:15:30Z" 1142 "PAY_NOW" "1bb53ed1-787b-4543-9def-ea18eef7902e"
                     , Event "2011-12-03T14:15:30Z" 185 "PAY_NOW" "1bb53ed1-787b-4543-9def-ea18eef7902e"
                     , Event "2011-12-04T10:15:30Z" 82850 "PAY_LATER" "1bb53ed1-787b-4543-9def-ea18eef7902e"
                     ]
        aggregate events `shouldMatchList`
          [ Agg "10-50|PAY_NOW" 1
          , Agg "10-50|SLICE_IT" 1
          , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|PAY_LATER" 1
          , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|PAY_NOW" 2
          , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|SLICE_IT" 1
          , Agg "2011-12-03:10|10-50" 1
          , Agg "2011-12-03:10|10-50|SLICE_IT" 1
          , Agg "2011-12-03:12|10-50" 1
          , Agg "2011-12-03:12|10-50|PAY_NOW" 1
          , Agg "2011-12-03:14|<10" 1
          , Agg "2011-12-03:14|<10|PAY_NOW" 1
          , Agg "2011-12-03|1bb53ed1-787b-4543-9def-ea18eef7902e" 3
          , Agg "2011-12-04:10|>500" 1
          , Agg "2011-12-04:10|>500|PAY_LATER" 1
          , Agg "2011-12-04|1bb53ed1-787b-4543-9def-ea18eef7902e" 1
          , Agg "<10|PAY_NOW" 1
          , Agg ">500|PAY_LATER" 1
          ]

            
