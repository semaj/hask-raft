import Test.Hspec
import Message
import Server
import Data.Time.Clock
import qualified Data.HashMap.Lazy as HM

main :: IO ()
main = do
  t <- getCurrentTime
  hspec $ do
    let server = initServer "S" ["a", "b", "c", "d"] t 100
        leader = server { sState = Leader, votedFor = "S", currentTerm = 1 }
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
            aer2 = AER 1 5 True
            mess1 = Message "a" "S" "S" RAFT "1234" Nothing Nothing (Just aer1)
            mess2 = Message "a" "S" "S" RAFT "2345" Nothing Nothing (Just aer2)
            responded1 = respondLeader withNextIndices mess1 aer1
            responded2 = respondLeader withNextIndices mess2 aer2
        (HM.toList $ nextIndices responded1) `shouldBe` [("a", 3)]
        (HM.toList $ nextIndices responded2) `shouldBe` [("a", 5)]
        (HM.toList $ matchIndices responded2) `shouldBe` [("a", 5)]


