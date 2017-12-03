import Aoc 
  (d1puzzle1
  ,d1puzzle2
  ,d2puzzle1
  ,d2puzzle2
  )
import Test.Hspec

main :: IO ()
main = hspec $ 
  describe "Day 1" $ do
    it "returns sum of all digits match" $ do 
      d1puzzle1 "1111" `shouldBe` (4 :: Int)
    
    it "returns 0 when no digits match" $ do
      d1puzzle1 "1234" `shouldBe` (0 :: Int)

    it "returns last digit  when only first and last match" $ do 
      d1puzzle1 "91212129" `shouldBe` (9 :: Int)

    it "returns correct sum when some digits match" $ do
      d1puzzle1 "1122" `shouldBe` (3 :: Int)

    it "returns correct answer to 1.1" $ do 
      d1puzzle1 day1q1 `shouldBe` (1203 :: Int)

    it "returns sum when all digits match" $ do
      d1puzzle2 "1212" `shouldBe` (6 :: Int)

    it "returns 0 when no digits match" $ do
      d1puzzle2 "1221" `shouldBe` (0 :: Int)

    it "returns correct answer when 2 digits match" $ do
      d1puzzle2 "123425" `shouldBe` (4 :: Int)

    it "returns correct answer when some digits match" $ do
      d1puzzle2 "123123" `shouldBe` (12 :: Int)

    it "returns correct answer when most digits do not match" $ do
      d1puzzle2 "12131415" `shouldBe` (4 :: Int)

    it "returns correct answer to 1.2" $ do
      d1puzzle2 day1q2 `shouldBe` (1146 :: Int)

    it "returns the correct checksum" $ do
      d2puzzle1 [[5,1,9,5],[7,5,3],[2,4,6,8]] `shouldBe` (18 :: Int)

    it "returns correct answer to 2.1" $ do
      d2puzzle1 day2q1 `shouldBe` (51833 :: Int)

    it "returns the correct checksum" $ do
      d2puzzle2 [[5,9,2,8],[9,4,7,3],[3,8,6,5]] `shouldBe` (9 :: Int)

    it "returns correct answer to 2.2" $ do
      d2puzzle2 day2q2 `shouldBe` (288 :: Int)


day1q1 :: String
day1q1 =
  "31813174349235972159811869755166343882958376474278437681632495222499211488649543755655138842553867246131245462881756862736922925752647341673342756514856663979496747158241792857625471323535183222497949751644488277317173496124473893452425118133645984488759128897146498831373795721661696492622276282881218371273973538163779782435211491196616375135472517935481964439956844536136823757764494967297251545389464472794474447941564778733926532741752757865243946976266426548341889873514383464142659425122786667399143335772174973128383869893325977319651839516694295534146668728822393452626321892357192574444856264721585365164945647254645264693957898373214897848424966266582991272496771159583715456714645585576641458358326521858518319315233857473695712238323787254556597566461188452279853766184333696344395818615215846348586541164194624371353556812548945447432787795489443312941687221314432694115847863129826532628228386894683392352799514942665396273726821936346663485499159141368443782475714679953213388375939519711591262489869326145476958378464652451441434846382474578535468433514121336844727988128998543975147649823215332929623574231738442281161294838499441799996857746549441142859199799125595761724782225452394593514388571187279266291364278184761833324476838939898258225748562345853633364314923186685534864178665214135631494876474186833392929124337161222959459117554238429216916532175247326391321525832362274683763488347654497889261543959591212539851835354335598844669618391876623638137926893582131945361264841733341247646125278489995838369127582438419889922365596554237153412394494932582424222479798382932335239274297663365164912953364777876187522324991837775492621675953397843833247525599771974555545348388871578347332456586949283657613841414576976542343934911424716613479249893113961925713317644349946444271959375981158445151659431844142242547191181944395897963146947935463718145169266129118413523541222444997678726644615185324461293228124456118853885552279849917342474792984425629248492847827653133583215539325866881662159421987315186914769478947389188382383546881622246793781846254253759714573354544997853153798862436887889318646643359555663135476261863"

day1q2 :: String
day1q2 =
  "31813174349235972159811869755166343882958376474278437681632495222499211488649543755655138842553867246131245462881756862736922925752647341673342756514856663979496747158241792857625471323535183222497949751644488277317173496124473893452425118133645984488759128897146498831373795721661696492622276282881218371273973538163779782435211491196616375135472517935481964439956844536136823757764494967297251545389464472794474447941564778733926532741752757865243946976266426548341889873514383464142659425122786667399143335772174973128383869893325977319651839516694295534146668728822393452626321892357192574444856264721585365164945647254645264693957898373214897848424966266582991272496771159583715456714645585576641458358326521858518319315233857473695712238323787254556597566461188452279853766184333696344395818615215846348586541164194624371353556812548945447432787795489443312941687221314432694115847863129826532628228386894683392352799514942665396273726821936346663485499159141368443782475714679953213388375939519711591262489869326145476958378464652451441434846382474578535468433514121336844727988128998543975147649823215332929623574231738442281161294838499441799996857746549441142859199799125595761724782225452394593514388571187279266291364278184761833324476838939898258225748562345853633364314923186685534864178665214135631494876474186833392929124337161222959459117554238429216916532175247326391321525832362274683763488347654497889261543959591212539851835354335598844669618391876623638137926893582131945361264841733341247646125278489995838369127582438419889922365596554237153412394494932582424222479798382932335239274297663365164912953364777876187522324991837775492621675953397843833247525599771974555545348388871578347332456586949283657613841414576976542343934911424716613479249893113961925713317644349946444271959375981158445151659431844142242547191181944395897963146947935463718145169266129118413523541222444997678726644615185324461293228124456118853885552279849917342474792984425629248492847827653133583215539325866881662159421987315186914769478947389188382383546881622246793781846254253759714573354544997853153798862436887889318646643359555663135476261863"

day2q1 :: [[Int]]
day2q1 =
  [[4168,3925,858,2203,440,185,2886,160,1811,4272,4333,2180,174,157,361,1555]
  ,[150,111,188,130,98,673,408,632,771,585,191,92,622,158,537,142]
  ,[5785,5174,1304,3369,3891,131,141,5781,5543,4919,478,6585,116,520,673,112]
  ,[5900,173,5711,236,2920,177,3585,4735,2135,2122,5209,265,5889,233,4639,5572]
  ,[861,511,907,138,981,168,889,986,980,471,107,130,596,744,251,123]
  ,[2196,188,1245,145,1669,2444,656,234,1852,610,503,2180,551,2241,643,175]
  ,[2051,1518,1744,233,2155,139,658,159,1178,821,167,546,126,974,136,1946]
  ,[161,1438,3317,4996,4336,2170,130,4987,3323,178,174,4830,3737,4611,2655,2743]
  ,[3990,190,192,1630,1623,203,1139,2207,3994,1693,1468,1829,164,4391,3867,3036]
  ,[116,1668,1778,69,99,761,201,2013,837,1225,419,120,1920,1950,121,1831]
  ,[107,1006,92,807,1880,1420,36,1819,1039,1987,114,2028,1771,25,85,430]
  ,[5295,1204,242,479,273,2868,3453,6095,5324,6047,5143,293,3288,3037,184,987]
  ,[295,1988,197,2120,199,1856,181,232,564,1914,1691,210,1527,1731,1575,31]
  ,[191,53,714,745,89,899,854,679,45,81,726,801,72,338,95,417]
  ,[219,3933,6626,2137,3222,1637,5312,238,5895,222,154,6649,169,6438,3435,4183]
  ,[37,1069,166,1037,172,258,1071,90,497,1219,145,1206,143,153,1067,510]]

day2q2 :: [[Int]]
day2q2 =
  [[4168,3925,858,2203,440,185,2886,160,1811,4272,4333,2180,174,157,361,1555]
  ,[150,111,188,130,98,673,408,632,771,585,191,92,622,158,537,142]
  ,[5785,5174,1304,3369,3891,131,141,5781,5543,4919,478,6585,116,520,673,112]
  ,[5900,173,5711,236,2920,177,3585,4735,2135,2122,5209,265,5889,233,4639,5572]
  ,[861,511,907,138,981,168,889,986,980,471,107,130,596,744,251,123]
  ,[2196,188,1245,145,1669,2444,656,234,1852,610,503,2180,551,2241,643,175]
  ,[2051,1518,1744,233,2155,139,658,159,1178,821,167,546,126,974,136,1946]
  ,[161,1438,3317,4996,4336,2170,130,4987,3323,178,174,4830,3737,4611,2655,2743]
  ,[3990,190,192,1630,1623,203,1139,2207,3994,1693,1468,1829,164,4391,3867,3036]
  ,[116,1668,1778,69,99,761,201,2013,837,1225,419,120,1920,1950,121,1831]
  ,[107,1006,92,807,1880,1420,36,1819,1039,1987,114,2028,1771,25,85,430]
  ,[5295,1204,242,479,273,2868,3453,6095,5324,6047,5143,293,3288,3037,184,987]
  ,[295,1988,197,2120,199,1856,181,232,564,1914,1691,210,1527,1731,1575,31]
  ,[191,53,714,745,89,899,854,679,45,81,726,801,72,338,95,417]
  ,[219,3933,6626,2137,3222,1637,5312,238,5895,222,154,6649,169,6438,3435,4183]
  ,[37,1069,166,1037,172,258,1071,90,497,1219,145,1206,143,153,1067,510]]
