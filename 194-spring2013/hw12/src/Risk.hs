{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where
import Control.Monad.Random
import Control.Monad
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
    deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
    random           = first DV . randomR (1,6)
    randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
     deriving (Show)

-- exercise 2 ----------------------------------
-- use of 'head' safe, maxFightingUnits ensures at least 1 attacker and defender
-- use of '!! 1' safe, elements tested by 'case of = 2' prior to use
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
    let (maxAtt, maxDef) = maxFightingUnits bf
    z <- zipRoll (getBattleRolls maxAtt) (getBattleRolls maxDef)
    let battle1 = fightBattle (head z) bf
    case length z of
        1 -> return battle1
        2 -> return (fightBattle (z !! 1) battle1)
        _ -> error ("battle error " ++ show z)

maxFightingUnits :: Battlefield -> (Army, Army)
maxFightingUnits (Battlefield att def)
    | att < 2   = error "attackers less than 2"
    | def < 1   = error "defenders less than 1"
    | otherwise = (min 3 (att - 1), min 2 def)

getBattleRolls :: Int -> Rand StdGen [DieValue]
getBattleRolls n = replicateM n die

zipRoll :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen [(DieValue, DieValue)]
zipRoll asm dsm = do
    as <- asm
    ds <- dsm
    return (zip (sortBy (flip compare) as) (sortBy (flip compare) ds))

fightBattle :: (DieValue, DieValue) -> Battlefield -> Battlefield
fightBattle (a,d) (Battlefield att def)
    | a > d     = Battlefield att (def - 1)
    | otherwise = Battlefield (att - 1) def

-- exercise 3 ----------------------------------
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield 1 _) = return bf
invade bf@(Battlefield _ 0) = return bf
invade bf                   = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    battles <- replicateM 1000 (invade bf)
    let attackerWins = length $ filter (\x -> defenders x == 0) battles
    return (fromIntegral attackerWins / 1000)

-- exercise 4 ----------------------------------
-- from http://web.mit.edu/sp.268/www/risk.pdf
-- attackers win 2 units 37.2% of the time (vs 33.6% for defenders)
-- if true then attacker likely to win battles
-- (lose 1 each 29.2% does not determine overall outcome)

-- sample 1,000,000 sets of three attacker die rolls vs two defender die rolls
-- to show P(attacker winning 2 units) == 37.2%
successWinPercentage :: Rand StdGen Double
successWinPercentage = do
    wins <- go 1000000 (return 0)
    return (fromIntegral wins / 1000000) where
        go :: Int -> Rand StdGen Int -> Rand StdGen Int
        go 0 acc = acc
        go n acc = do
            ((a1,d1):(a2,d2):_) <- zipRoll (getBattleRolls 3) (getBattleRolls 2)
            let acc' = if a1 > d1 && a2 > d2 then liftM (+ 1) acc else acc
            go (n - 1) acc'
