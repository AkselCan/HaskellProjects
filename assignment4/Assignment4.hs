{-# language CPP #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

module Assignment4 where


import Prelude hiding (Monoid, mempty, foldMap, Foldable, (<>))




import Data.Set      (Set)
import qualified Data.Set as Set
import Data.List (sort, group, foldl', group, sort, groupBy, sortOn, sortBy, tails, nub, delete)
import Data.Ord
import Data.Function
import Data.Maybe



-- | Containers

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

-- * Exercise 1

instance Functor Rose where
    fmap f (MkRose a rs) = MkRose (f a) (map (fmap f) rs)

class Monoid a where
    mempty ::           a
    (<>)   :: a -> a -> a

instance Monoid [a] where
    mempty = []
    (<>)   = (++)

newtype Sum     a = Sum     { unSum     :: a } deriving (Eq, Show)
newtype Product a = Product { unProduct :: a } deriving (Eq, Show)

instance Num a => Monoid (Sum a) where
    mempty           = Sum 0
    Sum n1 <> Sum n2 = Sum (n1 + n2)

-- * Exercise 2

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x <> Product y  =  Product (x * y)

class Functor f => Foldable f where
    fold    :: Monoid m =>             f m -> m
    foldMap :: Monoid m => (a -> m) -> f a -> m
    -- * Exercise 4
    foldMap f = fold . fmap f

instance Foldable [] where
    fold = foldr (<>) mempty

-- * Exercise 3

instance Foldable Rose where
    fold (MkRose a rs) = a <> foldr (\rose acc -> fold rose <> acc) mempty rs

-- * Exercise 5
newtype Add a = Add { getAdd :: a }
newtype Multiply a = Multiply { getMultiply :: a }

instance Num a => Monoid (Add a) where
  mempty = Add 0
  Add x <> Add y = Add (x + y)

instance Num a => Monoid (Multiply a) where
  mempty = Multiply 1
  Multiply x <> Multiply y = Multiply (x * y)


fsum, fproduct :: (Foldable f, Num a) => f a -> a
fsum = getAdd . foldMap Add
fproduct = getMultiply . foldMap Multiply

-- | Poker

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
    deriving (Bounded, Enum, Eq, Ord)

-- * Exercise 6

instance Show Rank where
    show r = case r of
        R2   -> "2"
        R3   -> "3"
        R4   -> "4"
        R5   -> "5"
        R6   -> "6"
        R7   -> "7"
        R8   -> "8"
        R9   -> "9"
        R10  -> "10"
        J    -> "J"
        Q    -> "Q"
        K    -> "K"
        A    -> "A"

data Suit = S | H | D | C
    deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

-- * Exercise 7

instance Show Card where
    show (Card { rank = r, suit = s }) = show r ++ show s

type Deck = [Card]

-- * Exercise 8

fullDeck, piquetDeck :: Deck
fullDeck = [Card rank suit | rank <- [R2 .. A], suit <- [S, H, D, C]]
piquetDeck = [Card rank suit | rank <- [R7 .. A], suit <- [S, H, D, C]]


newtype Hand = Hand { unHand :: [Card] } deriving (Eq, Show)

data HandCategory
    = HighCard      [Rank]
    | OnePair       Rank [Rank]
    | TwoPair       Rank Rank Rank
    | ThreeOfAKind  Rank Rank Rank
    | Straight      Rank
    | Flush         [Rank]
    | FullHouse     Rank Rank
    | FourOfAKind   Rank Rank
    | StraightFlush Rank
    deriving (Eq, Ord, Show)

-- * Exercise 9

sameSuits :: Hand -> Bool
sameSuits (Hand cards) = all (== head suits) (tail suits)
  where suits = map suit cards

-- * Exercise 10

isStraight :: [Rank] -> Maybe Rank
isStraight ranks
  | isStraightSeq sortedRanks = Just $ last sortedRanks
  | isAceLowStraight sortedRanks = Just R5
  | otherwise = Nothing
  where
    sortedRanks = sort ranks
    
    isStraightSeq :: [Rank] -> Bool
    isStraightSeq (x:y:zs) = succ x == y && isStraightSeq (y:zs)
    isStraightSeq [_] = True
    isStraightSeq [] = False

    isAceLowStraight :: [Rank] -> Bool
    isAceLowStraight ranks = sort (R2:tail ranks) == [R2, R3, R4, R5, A]
       
-- * Exercise 11

ranks :: Hand -> [Rank]
ranks (Hand cards) = sortBy (flip compare) $ map rank cards

-- * Exercise 12

order :: Hand -> [(Int, Rank)]
order (Hand cards) = 
  let ranks = map rank cards
      groupedRanks = group $ sortOn Down ranks
      rankCounts = map (\g -> (length g, head g)) groupedRanks
  in sortOn (Down . fst) rankCounts

-- * Exercise 13

handCategory :: Hand -> HandCategory
handCategory hand@(Hand cards)
  | isStraight && isFlush                 = StraightFlush highestRank
  | isFourOfAKind                        = FourOfAKind fourKindRank kickerRank
  | isFullHouse                          = FullHouse threeKindRank pairRank
  | isFlush                              = Flush sortedRanks
  | isStraight                           = Straight highestRank
  | isThreeOfAKind                       = ThreeOfAKind threeKindRank kicker1 kicker2
  | isTwoPairs                           = TwoPair highPairRank lowPairRank kickerRank
  | isOnePair                            = OnePair pairRank (take 3 kickers)
  | otherwise                            = HighCard sortedRanks
  where
    sortedRanks = sort $ map rank cards
    rankGroups = group sortedRanks
    rankCounts = sortBy (comparing $ Down . length) $ map (\g -> (length g, head g)) rankGroups
    
    highestRank = head sortedRanks
    isFlush = all (== head suits) (tail suits)
      where suits = map suit cards

    isStraight = sortedRanks == straightRanks || sortedRanks == [A, R5, R4, R3, R2]
      where straightRanks = [highestRank, pred highestRank..last sortedRanks]
    
    [isFourOfAKind, isFullHouse, isThreeOfAKind, isTwoPairs, isOnePair] = map ((>1) . fst) (rankCounts ++ repeat (0, R2))
    
    (fourKindRank, _) = head rankCounts
    (threeKindRank, _) = head rankCounts
    (_, pairRank) = rankCounts !! 1
    (highPairRank, _) = head rankCounts
    (_, lowPairRank) = rankCounts !! 1
    kickerRank = head $ map snd $ dropWhile ((==4) . fst) rankCounts
    kicker1 = head $ map snd $ dropWhile ((==3) . fst) rankCounts
    kicker2 = head $ map snd $ drop 2 $ dropWhile ((==3) . fst) rankCounts
    kickers = map snd $ filter ((==1) . fst) rankCounts

    
-- * Exercise 14

instance Ord Hand where
    compare h1 h2 =
        let cat1 = handCategory h1
            cat2 = handCategory h2
        in compare cat1 cat2

-- * Exercise 15

combs :: Int -> [a] -> [[a]]
combs 0 _  = [[]]
combs _ [] = []
combs n (x:xs) = map (x:) (combs (n-1) xs) ++ combs n xs

-- * Exercise 16

allHands :: Deck -> [Hand]
allHands deck = map Hand $ combs 5 deck

-- * Exercise 17

distinctHands :: Deck -> Set Hand
distinctHands = foldl' (flip Set.insert) Set.empty . allHands

-- * Question 1

{- ANSWER -}
-- * Question 2

{- ANSWER -}
