module Types where

import Linear.V2 (V2(..), _x, _y)

-- Used in MyCharacter.hs
data Game = Game
  { _myCharacter  :: Character
  , _dir          :: Direction
  , _jump         :: Bool
  , _tokens       :: [GridCoord]
  , _exits        :: [GridCoord]
  , _dead         :: Bool
  , _done         :: Bool
  } deriving (Show)

type GridCoord = V2 Int

type PreciseCoord = V2 Float

data Character = Character
  { _loc      :: PreciseCoord
  , _hv       :: Int
  , _vv       :: Int
  } deriving (Show)

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = Neutral
  | DownDir
  | RightDir
  | LeftDir
  deriving (Eq, Show)

height :: Int
height = 50

width :: Int
width = 50

speedTable :: [Float]
-- speedTable = [-1, -0.5, -0.25, -0.25, 0, 0.25, 0.25, 0.5, 1]
-- speedTable = [-1, -0.9596001665972512, -0.9200333194502291, -0.8812994585589339, -0.8433985839233654, -0.8063306955435237, -0.7700957934194087, -0.7346938775510206, -0.7001249479383591, -0.6663890045814245, -0.6334860474802166, -0.6014160766347356, -0.5701790920449813, -0.5397750937109539, -0.5102040816326532, -0.4814660558100792, -0.45356101624323203, -0.4264889629321117, -0.4002498958767181, -0.3748438150770513, -0.35027072053311126, -0.326530612244898, -0.30362349021241153, -0.2815493544356519, -0.2603082049146189, -0.2399000416493128, -0.22032486463973347, -0.20158267388588091, -0.18367346938775514, -0.16659725114535612, -0.1503540191586839, -0.13494377342773847, -0.1203665139525198, -0.10662224073302792, -0.09371095376926282, -0.0816326530612245, -0.07038733860891297, -0.0599750104123282, -0.05039566847147023, -0.04164931278633903, -0.03373594335693462, -0.02665556018325698, -0.020408163265306124, -0.01499375260308205, -0.010412328196584757, -0.006663890045814245, -0.0037484381507705126, -0.0016659725114535613, -0.0004164931278633903, -0.0, 0.0, 0.0004164931278633903, 0.0016659725114535613, 0.0037484381507705126, 0.006663890045814245, 0.010412328196584757, 0.01499375260308205, 0.020408163265306124, 0.02665556018325698, 0.03373594335693462, 0.04164931278633903, 0.05039566847147023, 0.0599750104123282, 0.07038733860891297, 0.0816326530612245, 0.09371095376926282, 0.10662224073302792, 0.1203665139525198, 0.13494377342773847, 0.1503540191586839, 0.16659725114535612, 0.18367346938775514, 0.20158267388588091, 0.22032486463973347, 0.2399000416493128, 0.2603082049146189, 0.2815493544356519, 0.30362349021241153, 0.326530612244898, 0.35027072053311126, 0.3748438150770513, 0.4002498958767181, 0.4264889629321117, 0.45356101624323203, 0.4814660558100792, 0.5102040816326532, 0.5397750937109539, 0.5701790920449813, 0.6014160766347356, 0.6334860474802166, 0.6663890045814245, 0.7001249479383591, 0.7346938775510206, 0.7700957934194087, 0.8063306955435237, 0.8433985839233654, 0.8812994585589339, 0.9200333194502291, 0.9596001665972512, 1]
-- speedTable = [-0.9974998270115687, -0.9572010001810303, -0.9177330769964827, -0.8790960574579256, -0.841289941565359, -0.8043147293187825, -0.7681704207181965, -0.7328570157636014, -0.6983745144549965, -0.6647229167923823, -0.6319022227757584, -0.599912432405125, -0.568753545680482, -0.5384255626018296, -0.5089284831691676, -0.4802623073824961, -0.4524270352418152, -0.4254226667471246, -0.3992492018984245, -0.373906640695715, -0.3493949831389959, -0.3257142292282673, -0.3028643789635292, -0.28084543234478143, -0.2596573893720243, -0.23930025004525757, -0.2197740143644814, -0.2010786823296956, -0.18321425394090035, -0.16618072919809557, -0.14997810810128126, -0.1346063906504574, -0.12006557684562402, -0.10635566668678115, -0.09347666017392875, -0.08142855730706683, -0.07021135808619536, -0.05982506251131439, -0.0502696705824239, -0.04154518229952389, -0.03365159766261435, -0.026588916671695287, -0.020357139326766707, -0.014956265627828598, -0.010386295574880973, -0.006647229167923822, -0.0037390664069571495, -0.0016618072919809554, -0.00041545182299523886, -0.0, 0.0, 0.00041545182299523886, 0.0016618072919809554, 0.0037390664069571495, 0.006647229167923822, 0.010386295574880973, 0.014956265627828598, 0.020357139326766707, 0.026588916671695287, 0.03365159766261435, 0.04154518229952389, 0.0502696705824239, 0.05982506251131439, 0.07021135808619536, 0.08142855730706683, 0.09347666017392875, 0.10635566668678115, 0.12006557684562402, 0.1346063906504574, 0.14997810810128126, 0.16618072919809557, 0.18321425394090035, 0.2010786823296956, 0.2197740143644814, 0.23930025004525757, 0.2596573893720243, 0.28084543234478143, 0.3028643789635292, 0.3257142292282673, 0.3493949831389959, 0.373906640695715, 0.3992492018984245, 0.4254226667471246, 0.4524270352418152, 0.4802623073824961, 0.5089284831691676, 0.5384255626018296, 0.568753545680482, 0.599912432405125, 0.6319022227757584, 0.6647229167923823, 0.6983745144549965, 0.7328570157636014, 0.7681704207181965, 0.8043147293187825, 0.841289941565359, 0.8790960574579256, 0.9177330769964827, 0.9572010001810303, 0.9974998270115687]
speedTable = [-0.4999999292893244, -0.4798000154446494, -0.4600165946689369, -0.44064966696218677, -0.4216992323243989, -0.4031652907555735, -0.3850478422557105, -0.3673468868248097, -0.3500624244628714, -0.3331944551698955, -0.3167429789458818, -0.30070799579083063, -0.28508950570474184, -0.2698875086876153, -0.2551020047394512, -0.24073299386024946, -0.22678047605001012, -0.2132444513087331, -0.20012491963641846, -0.18742188103306623, -0.1751353354986763, -0.16326528303324878, -0.15181172363678364, -0.14077465730928085, -0.13015408405074042, -0.11995000386116235, -0.11016241674054669, -0.10079132268889338, -0.09183672170620243, -0.08329861379247387, -0.07517699894770766, -0.06747187717190382, -0.060183248465062365, -0.053311112827183274, -0.04685547025826656, -0.040816320758312194, -0.03519366432732021, -0.029987500965290587, -0.025197830672223344, -0.020824653448118468, -0.016867969292975956, -0.013327778206795819, -0.010204080189578048, -0.007496875241322647, -0.005206163362029617, -0.0033319445516989546, -0.0018742188103306617, -0.0008329861379247387, -0.00020824653448118466, -0.0, 0.0, 0.00020824653448118466, 0.0008329861379247387, 0.0018742188103306617, 0.0033319445516989546, 0.005206163362029617, 0.007496875241322647, 0.010204080189578048, 0.013327778206795819, 0.016867969292975956, 0.020824653448118468, 0.025197830672223344, 0.029987500965290587, 0.03519366432732021, 0.040816320758312194, 0.04685547025826656, 0.053311112827183274, 0.060183248465062365, 0.06747187717190382, 0.07517699894770766, 0.08329861379247387, 0.09183672170620243, 0.10079132268889338, 0.11016241674054669, 0.11995000386116235, 0.13015408405074042, 0.14077465730928085, 0.15181172363678364, 0.16326528303324878, 0.1751353354986763, 0.18742188103306623, 0.20012491963641846, 0.2132444513087331, 0.22678047605001012, 0.24073299386024946, 0.2551020047394512, 0.2698875086876153, 0.28508950570474184, 0.30070799579083063, 0.3167429789458818, 0.3331944551698955, 0.3500624244628714, 0.3673468868248097, 0.3850478422557105, 0.4031652907555735, 0.4216992323243989, 0.44064966696218677, 0.4600165946689369, 0.4798000154446494, 0.4999999292893244]

maxSpeed :: Int
maxSpeed = length speedTable

-- Used in UI.hs
data Tick = Tick -- Can be used to mark time

type Name = ()

data Cell = MyCharacter | Token | Exit | Empty