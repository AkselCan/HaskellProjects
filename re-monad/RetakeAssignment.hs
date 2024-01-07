{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module RetakeAssignment where

import Control.Applicative (Applicative(..))
import Control.Monad       (ap, liftM, replicateM)
import Data.Foldable       (Foldable(..))
import Data.Monoid         (Monoid(..), Sum(..), (<>))
import Data.Ratio          ((%))
import System.Random       (Random(random, randomR), getStdRandom)
-- import Control.Category (Category(id))
-- import Control.Monad.RWS (Monoid(mappend))

-- | A Game of Chance

-- * The Gambling Monad

data Coin = H | T
    deriving (Bounded, Eq, Enum, Ord, Show)

data Dice = D1 | D2 | D3 | D4 | D5 | D6
    deriving (Bounded, Eq, Enum, Ord, Show)
    
data Outcome = Win | Lose
    deriving (Eq, Ord, Show)

class Monad m => MonadGamble m where
    toss :: m Coin
    roll :: m Dice

-- Exercise 1

game :: MonadGamble m => m Outcome
game = do
    coinResults <- replicateM 6 toss
    let headsCount = length $ filter (== H) coinResults
    diceResult <- roll
    let diceValue = case diceResult of
                        D1 -> 1
                        D2 -> 2
                        D3 -> 3
                        D4 -> 4
                        D5 -> 5
                        D6 -> 6
    return $ if diceValue >= headsCount then Win else Lose

-- * Simulation

-- Exercise 2

instance Random Coin where
    randomR (l, h) g = 
        let (value, newGen) = randomR (fromEnum l, fromEnum h) g
        in (toEnum value, newGen)
    random g = randomR (minBound, maxBound) g

instance Random Dice where
    randomR (l, h) g = 
        let (value, newGen) = randomR (fromEnum l, fromEnum h) g
        in (toEnum value, newGen)
    random g = randomR (minBound, maxBound) g

-- Exercise 3

instance MonadGamble IO where
    toss = getStdRandom random
    roll = getStdRandom random


-- Exercise 4

simulate :: IO Outcome -> Integer -> IO Rational
simulate game n = do
    outcomes <- replicateM (fromIntegral n) game
    let wins = length $ filter (== Win) outcomes
    return $ (fromIntegral wins) % n


-- * Decision trees

data DecisionTree a
    = Result a
    | Decision [DecisionTree a]
    deriving (Eq, Show)

instance Functor DecisionTree where     -- All monads are functors. You don't
    fmap = liftM                        -- have to make use of this fact.
 
instance Applicative DecisionTree where -- All monads are "applicative functors"
    pure  = return                      -- (not treated in this course). You
    (<*>) = ap                          -- don't have to make use of this fact.

-- Exercise 5

instance Monad DecisionTree where

    -- return :: a -> DecisionTree a
    return x = Result x

    -- (>>=) :: DecisionTree a -> (a -> DecisionTree b) -> DecisionTree b
    (Result x) >>= f = f x
    (Decision xs) >>= f = Decision (map (>>= f) xs)

-- Exercise 6

instance MonadGamble DecisionTree where
    toss = Decision [Result H, Result T]
    roll = Decision [Result D1, Result D2, Result D3, Result D4, Result D5, Result D6]

-- Exercise 7

probabilityOfWinning :: DecisionTree Outcome -> Rational
probabilityOfWinning (Result Win)  = 1
probabilityOfWinning (Result Lose) = 0
probabilityOfWinning (Decision xs) = sum (map probabilityOfWinning xs) / fromIntegral (length xs)

-- | Instrumented State Monad

-- Exercise 8

class Monad m => MonadState m s | m -> s where

    get :: m s
    get = modify Prelude.id

    put :: s -> m ()
    put s = modify (const s) >> return ()

    modify :: (s -> s) -> m s
    modify f = do
        s <- get
        put (f s)
        return (f s)

-- * Instrumentation

data Counts = Counts {
    binds   :: Int,
    returns :: Int,
    gets    :: Int,
    puts    :: Int
} deriving (Eq, Show)

-- Exercise 9

instance Semigroup Counts where        
    Counts b1 r1 g1 p1 <> Counts b2 r2 g2 p2 = 
        Counts (b1 + b2) (r1 + r2) (g1 + g2) (p1 + p2)                 

instance Monoid Counts where
    mempty  = Counts 0 0 0 0
    
oneBind, oneReturn, oneGet, onePut :: Counts
oneBind   = Counts 1 0 0 0 
oneReturn = Counts 0 1 0 0
oneGet    = Counts 0 0 1 0
onePut    = Counts 0 0 0 1

newtype State' s a = State' { runState' :: (s, Counts) -> (a, s, Counts) }

-- Exercise 10

instance Functor (State' s) where       -- All monads are functors. You don't
    fmap = liftM                        -- have to make use of this fact.
 
instance Applicative (State' s) where   -- All monads are "applicative functors"
    pure  = return                      -- (not treated in this course). You
    (<*>) = ap                          -- don't have to make use of this fact.

instance Monad (State' s) where

    -- return :: a -> State' s a
    return x = State' $ \(s, c) -> (x, s, c `mappend` oneReturn)

    -- (>>=) :: State' s a -> (a -> State' s b) -> State' s b
    State' f >>= k = State' $ \(s, c) ->
        let (a, s', c') = f (s, c `mappend` oneBind)
            State' h = k a
        in h (s', c')

instance MonadState (State' s) s where

    -- get :: State' s s
    get = State' $ \(s, c) -> (s, s, c `mappend` oneGet)

    -- put :: s -> State' s ()
    put s = State' $ \(_, c) -> ((), s, c `mappend` onePut)

    -- modify :: (s -> s) -> State' s s
    modify f = get >>= \s -> put (f s) >> return (f s)

-- * Tree Labeling

data Tree a = Branch (Tree a) a (Tree a) | Leaf
    deriving (Eq, Ord, Show)

-- Exercise 11

label :: MonadState m Int => Tree a -> m (Tree (Int, a))
label Leaf = return Leaf
label (Branch left x right) = do
    leftLabeled <- label left
    n <- get
    modify (+1)
    rightLabeled <- label right
    return $ Branch leftLabeled (n, x) rightLabeled

-- Exercise 12

run :: State' s a -> s -> (a, Counts)
run (State' f) initialState = 
    let (result, _, counts) = f (initialState, mempty)
    in (result, counts)