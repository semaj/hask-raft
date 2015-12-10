import Test.Hspec
import Message
import Data.Maybe
import Server
import Data.Time.Clock
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
  t <- getCurrentTime
  t2 <- getCurrentTime
  hspec $ do
    let server = initServer "S" ["a", "b", "c", "d"] t 10000000
        leader = server { sState = Leader, votedFor = "S", currentTerm = 1 }
        follower = server { sid = "a", sState = Follower, votedFor = "S", currentTerm = 1 } -- no others or maps...
        put = Command CPUT 1 "x" "1234" "k" "v"
        get = Command CGET 1 "x" "1234" "k" ""
    describe "leaderExecute" $ do
      it "can accept multiple commands" $ do
        let withCommands = leader { slog = [put,
                                            put { cmid = "2345", creator = "y", ckey = "k", cvalue = "v1" },
                                            get { cmid = "3456", creator = "z" }] }
            executed = leaderExecute withCommands
            mess1 = Message "S" "x" "S" OK "1234" (Just "k") (Just "v") Nothing
            mess2 = Message "S" "y" "S" OK "2345" (Just "k") (Just "v1") Nothing
            mess3 = Message "S" "z" "S" OK "3456" (Just "k") (Just "v1") Nothing
        (HM.toList $ store executed) `shouldBe` [("k", "v1")]
        (sendMe executed) `shouldBe` [mess1, mess2, mess3]

    describe "respondLeader" $ do
      it "adjusts" $ do
        let withNextIndices = leader { nextIndices = HM.fromList [("a", 4)],
                                       matchIndices = HM.fromList [("a", 2)] }
            aer1 = AER 1 (-1) False
            aer2 = AER 1 6 True
            mess1 = Message "a" "S" "S" RAFT "1234" Nothing Nothing (Just aer1)
            mess2 = Message "a" "S" "S" RAFT "2345" Nothing Nothing (Just aer2)
            responded1 = respondLeader withNextIndices mess1 aer1
            responded2 = respondLeader withNextIndices mess2 aer2
        (HM.toList $ nextIndices responded1) `shouldBe` [("a", 3)]
        (HM.toList $ nextIndices responded2) `shouldBe` [("a", 7)]
        (HM.toList $ matchIndices responded2) `shouldBe` [("a", 6)]

    describe "send AE, respond, handle response" $ do
      it "AEs" $ do
        let withCommands = leader { slog = [put,
                                            put { cmid = "2345", creator = "y", ckey = "k", cvalue = "v1" },
                                            get { cmid = "3456", creator = "z" }] }
            withSent = leaderSendAEs "1" t withCommands
            toA = head $ sendMe withSent
            respondedFollower = respondFollower follower toA (fromJust $ rmess toA)
            toAresponse = head $ sendMe respondedFollower
            toAresponseAER = fromJust $ rmess $ toAresponse
        (success $ toAresponseAER) `shouldBe` True
        (slog respondedFollower) `shouldBe` (slog withCommands)
        (commitIndex respondedFollower) `shouldBe` (-1)
        (currentTerm respondedFollower) `shouldBe` 1
        toAresponse `shouldBe` (Message "a" "S" "S" RAFT "1a" Nothing Nothing $ Just $ AER 1 2 True)
        -- TEST THE MAP TOO HERE DUDE
        let respondedLeader = respondLeader withSent toAresponse toAresponseAER
            newNext = nextIndices respondedLeader
            newMatch = matchIndices respondedLeader
            newLastMess = lastMess respondedLeader
        (HM.lookup "a" newNext) `shouldBe` Just 3
        (HM.lookup "a" newMatch) `shouldBe` Just 2
        (HM.lookup "a" newLastMess) `shouldBe` (Just $ SentMessage Nothing t)
        let moreCommands = respondedLeader { commitIndex = 4, sendMe = [],
                                             slog = (slog respondedLeader) ++
                                                    [put { cmid = "42", creator = "x", ckey = "k1", cvalue = "v2" },
                                                     put { cmid = "43", creator = "y", ckey = "k2", cvalue = "v2" },
                                                     put { cmid = "44", creator = "z", ckey = "k2", cvalue = "v3" }] }
            moreSent = leaderSendAEs "2" t2 moreCommands
            toA2 = head $ sendMe moreSent
            rf2 = respondFollower respondedFollower { sendMe = [] } toA2 (fromJust $ rmess toA2)
            ar2 = head $ sendMe rf2
            araer2 = fromJust $ rmess $ ar2
        (success $ araer2) `shouldBe` True
        (commitIndex rf2) `shouldBe` 4
        (currentTerm rf2) `shouldBe` 1
        (slog rf2) `shouldBe` (slog moreCommands)
        ar2 `shouldBe` (Message "a" "S" "S" RAFT "2a" Nothing Nothing $ Just $ AER 1 5 True)




