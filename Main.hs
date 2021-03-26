{-# LANGUAGE ScopedTypeVariables #-} -- this isnt currently used, but may be used later for documentation
{-# LANGUAGE TypeSynonymInstances #-} -- these two homies help in showing Games
{-# LANGUAGE FlexibleInstances #-}    --
{-# LANGUAGE DeriveAnyClass #-}
module Lib
    ( Card
    ) where

import Data.Array ( Ix, (!), (//), array, Array )
import Data.Maybe ( fromJust, isJust )
import qualified Data.Sequence as S
import qualified Data.Text.IO as T


-- this is gonna be a very fun card game
-- eventually ill use like brick or something for the ui
-- but not right now, have to actually make the game part first

-- here lies the card declaration, its kinda scooty do dont fuck with it
data Card = Card { name :: String   -- card name, replace with Text soon
                 , cost :: Cost     -- how much a card costs to cast
                 , tyln :: TypeLine -- defines card behavior
                 , attk :: Maybe Int      -- damage done to other playables
                 , defn :: Maybe Int      -- damage it can take
                 , damg :: Maybe Int      -- how much damage it does to the player
                 , arrw :: Maybe [Pair Int]    -- where it can attack
                 , ownr :: Player         -- whose control is it under
                 , efct :: S.Seq EffectFull     }  -- effects are what a card does upon interaction
                 deriving Show      -- this will require an entire parser to implement

testCard :: Card
testCard = Card "test" [Colorless] Creature (Just 1) (Just 1) (Just 1) (Just allArrows) First S.empty

allArrows :: [Pair Int]
allArrows = tupleToPair <$> [(i,j) | i <- [-1..1], j <- [-1..1]]

-- HERE LIES GAME-SPECIFIC TYPE DECS --

-- type synonyms

type Hand        = S.Seq Card
type Deck        = [Card]
type Cost        = [Resource]
type Pool        = [Resource]
type Fives       = S.Seq (Resource, IsUsed)
type Turn        = Int
type Priority    = Player
type Stack       = [GameAction]
type Table       = Array (Pair Int) (Maybe Card)
type EffectStack = S.Seq EffectFull 


--  i'm throwing all the fx type bullshit into its own section so i dont die

data EffectFull       = EF {eftrgt :: Targetting (Pair Int)
                           ,efexpr :: EffectExpr
                           ,eftime :: EffectTime
                           ,efkind :: EffectKind} deriving Show
data EffectTime       = Continuous | OneShot                         deriving (Eq,Show)
data Targetting a     = NeedsTarget | DoesntTarget | Target a    deriving (Eq,Show)
data EffectKind       = OnAttack | OnPlay | OnLeave                  deriving (Eq,Show)
type EffectExpr       = Either UntargettedG TargettedG
type UntargettedG     = (Game -> Game)
type TargettedG       = ((Int,Int) -> Game -> Game)
instance Show UntargettedG where
    show _ = "untargetted dummy"
instance Show TargettedG where
    show _ = "targetted dummy"

{- 
writing this here so i dont forget how my own fx system works
full a g runs the game action and then the fx stack, which consists of an effect stack carried by g
some effects need targets, so before priority is passed to the other player, a Select action is passed through action0 
this repeats until no member of the fx stack has a Targetting value of NeedsTarget
above can be easily implemented with a recursive function YOU'RE GOING TO HAVE TO WRITE YOURSELF, SETH
funny thing i thought about
oneshot and continuous effects dont actually need to be separate
a +1/+1/+1 to target creature permanently isnt actually continuous, its a oneshot with no undo
this structure also makes it so +1/+1/+1 to target creature from another is actually 2 fx
one OnPlay effect to put the buff on, one OnLeave to take the effect off
a continuous effect doesnt leave the stack on being ran, so it actually triggers each turn
nice for sulfuric vortex type effects, bad for pumping creatures
better idea is to just do an OnTurn value of EffectKind and remove EffectTime entirely
-}
-- new datatypes
data Resource    = White | Black | Red | Colorless deriving (Eq, Show)
data TypeLine    = Creature | Sorcery deriving (Eq, Show)
data IsUsed      = Used | Unused deriving (Eq, Show)
data Player      = First | Second deriving (Eq, Show)
data GameAction      = Draw                                 |
                   Play Int (Pair Int)                  | 
                   Attack (Pair Int) (Pair Int)         | -- the maybe check for this second argument has been inlined into action'
                   Flip (Pair Int)                      | 
                   Change Int Resource                  |
                   Use Int                              |
                   Pass                                 |
                   Select (Pair Int) Int                |
                   Give
data PlayerInfo  = PlayerInfo { hand :: Hand
                              , deck :: Deck
                              , five :: Fives
                              , pool :: Pool
                              , plyr :: Player } deriving Show

-- END TYPES --

-- hey here's how actions are gonna work
-- draw takes no parameters, and will append one card from the top of the deck to the active player's hand
-- play takes an int (card in player's hand, 0-indexed) and a table space to place the card
-- attack takes a card on the table to attack with and maybe another card on the table, Nothing signifies attacking a player directly
-- use puts a resource into a pool, this does not increment turn so it is kept separate in action and not action'
-- flip rotates the card's arrows. i dont want to implement "on flip" fx in this so this action may be entirely useless	
-- pass takes no parameters and turns all resources to Unused

-- this Pair type needs some space for its instances

infix 1 :#
data Pair a      = a :# a deriving (Eq, Ord, Show, Ix)

tupleToPair :: (a, a) -> Pair a
tupleToPair (x,y) = x :# y

instance Functor Pair where
  fmap f (x :# y) =  f x :# f y

-- these sorta feel like cheating but theyre really useful. why hasnt anyone done this before?
-- they probably have and these dont satisfy functor laws or whatever
fakebimap :: (t -> a) -> (t -> a) -> Pair t -> Pair a
fakebimap f g (a :# b) = f a :# g b

infix 3 *~* -- look at this fuckin insane pipe bro
(f :# g) *~* (a :# b) = f a :# g b
-- end cheating 
fstP :: Pair a -> a
fstP (a :# _) = a
sndP :: Pair a -> a
sndP (_ :# a) = a

firstP, secondP :: (a -> a) -> (Pair a -> Pair a)
firstP  f ~(a :# b) = f a :# b
secondP g ~(a :# b) = a :# g b

-- the game will consist of 5 different parts
-- hand, table, player, deck, state

-- hand --
testHand :: Hand 
testHand = S.replicate 6 testCard



-- table --
initTable :: Array (Pair Int) (Maybe Card)
initTable =  array (head pos, last pos) [(a,b) | a <- pos, b <- replicate (length pos) Nothing]
    where pos = [x :# y | x <- [1..4], y <- [1..4]]

-- player  --
testResourceState :: S.Seq (Resource, IsUsed)
testResourceState = S.replicate 5 (Colorless, Unused)

useResource :: (Resource, IsUsed) -> (Resource, IsUsed)
useResource (x,Unused) = (x,Used)

switchPlayer :: Player -> Player
switchPlayer First  = Second
switchPlayer Second = First

grabPlayer First d@(x :# y) = fstP d
grabPlayer Second d@(x :# y) = sndP d

initFirst :: PlayerInfo
initFirst  = PlayerInfo testHand testDeck testResourceState [] First
initSecond :: PlayerInfo
initSecond = PlayerInfo testHand testDeck testResourceState [] Second

-- deck --
testDeck :: [Card]
testDeck = replicate 20 testCard
multiDraw :: Int -> PlayerInfo -> PlayerInfo
multiDraw n p = iterate draw p !! n

-- state --
data Game = Win Player | 
            Tie | 
            Continue Turn Priority Table (Pair PlayerInfo) EffectStack
            deriving (Show)

initStack :: EffectStack
initStack = S.empty

initGame :: Game
initGame = Continue 1 First initTable (initFirst :# initSecond) initStack

-- state updates --

push :: EffectKind -> Card -> S.Seq EffectFull -> S.Seq EffectFull
push ek c es = es S.>< S.filter (\x -> efkind x == ek) (efct c) --ek is effectkind, es is effectstack, c is card

battle :: Maybe Card -> Maybe Card -> [Bool]
battle x y  = [ defn cd <= attk ca, defn ca <= attk cd ] -- first value is for if CD leaves, second is for if CA leaves 
                where ca = fromJust x
                      cd = fromJust y

cardSelected :: Ix a => Array (Pair a) e -> a -> a -> e
cardSelected table x y = table ! (x :# y)


battleMagic :: Ix a => Pair a -> Pair a -> Array (Pair a) (Maybe Card) -> b -> S.Seq EffectFull -> (Array (Pair a) (Maybe Card), b, S.Seq EffectFull)
battleMagic (xa:#ya) (xb:#yb) c d e = case battle (cardSelected c xa ya) (cardSelected c xb yb) of -- card attacking, card defending, table, players, stack
                                      [True,_]    -> (removeCards c [xb:#yb],d, goodbyecd e)
                                      [_,True]    -> (removeCards c [xa:#ya],d, goodbyeca e)
                                      [True,True] -> (removeCards c [xb:#yb,xa:#ya],d, (goodbyeca . goodbyecd) e)
                                      where ca = fromJust (cardSelected c xa ya)
                                            cd = fromJust (cardSelected c xb yb)
                                            goodbyecd = push OnLeave cd . push OnAttack ca
                                            goodbyeca = push OnLeave ca . push OnAttack cd

removeCards :: Ix i => Array i (Maybe a) -> [i] -> Array i (Maybe a)
removeCards table [cards] = table // [(i,Nothing) | i <- [cards]]

localContextP :: Player -> (a -> a) -> Pair a -> Pair a
localContextP p f x    = case p of First -> firstP f x
                                   Second -> secondP f x

action0 :: GameAction -> Game -> Game
action0 act g@(Continue a b c d e) = case act of
     Play i (x :# y)      -> if alreadyExists c (x:#y)
                             then Continue a b c d e
                             else Continue a b (c // [( x:#y , cardFromHand i b d )])
                                               (localContextP b (rmov i) d) 
                                               (push OnPlay (fromJust $ cardFromHand i b d) e)
     Use x                -> Continue a b c (localContextP b undefined d) e
     Select (x :# y) indx -> Continue a b c d (S.adjust' (\a -> a {eftrgt = Target (x:#y)}) indx e)
     _                    -> action1 act g
     where alreadyExists table (x:#y) = isJust $ table ! (x:#y)
           cardFromHand select b d = S.lookup select (hand $ grabPlayer b d)
                            

action1 :: GameAction -> Game -> Game
action1 act g@(Continue a b c d e) = incrementTurn a b ^~^ case act of 
      Draw              -> (c, playerContextP draw d,e)   
      Attack (xa :# ya) (xb :# yb) -> case (cardSelected c xa ya,cardSelected c xb yb) of 
                                (Just ca,Nothing) -> if ownr ca == b 
                                                     then (c, localContextP otherP (multiDraw (fromJust $ damg ca)) 
                                                           d, 
                                                           push OnAttack ca e)
                                                     else (c,d,e)
                                (Nothing,Just cd) -> (c, d, e) -- nice job wasting your turn, idiot
                                (Just ca,Just cd) -> if battleChecks ca (xa:#ya) (xb:#yb) -- put battlechecks here, NOT above
                                                     then battleMagic (xa:#ya) (xb:#yb) c d e -- nice job wasting your turn, idiot 
                                                     else (c,d,e)
      Flip (x :# y)     -> let cs = cardSelected c x y in 
                           ( c // [(x:#y, flippedCard cs (flipArrows cs) )], d,e) -- don't touch this
      Change slot color -> (c, playerContextP (chng slot color) d,e)
      Pass              -> (c, playerContextP pass d,e)
      Give              -> (c,d,e)
      where flipArrows card       = ((\(x :# y) -> negate x :# negate y) <$>) <$> arrw (fromJust card) 
            flippedCard card help = (fromJust card)  {arrw = help} <$ card 
            playerContextP f x     = case b of First -> firstP f x
                                               Second -> secondP f x
            otherP                = switchPlayer b
            battleChecks ca (xa:#ya) (xb:#yb) = (&&) (ownr ca == b) (any (\x -> (xb:#yb) == x) (((+) <$> (xa:#ya) *~*) <$> fromJust (arrw ca)))

-- here lie game helpers --
pass :: PlayerInfo -> PlayerInfo
pass x@(PlayerInfo h d f p l)     = PlayerInfo h d ((Unused <$) <$> f) p l
draw :: PlayerInfo -> PlayerInfo
draw x@(PlayerInfo h d f p l)     = PlayerInfo (head d S.<| h) (tail d) f p l
chng :: Int -> Resource -> PlayerInfo -> PlayerInfo
chng i c x@(PlayerInfo h d f p l) = PlayerInfo h d (S.adjust' (changeColor c) i f) p l
                                    where changeColor c (x, y) = (c, y)
rmov :: Int -> PlayerInfo -> PlayerInfo
rmov i x@(PlayerInfo h d f p l ) = PlayerInfo (S.deleteAt i h) d f p l
use5 :: Int -> PlayerInfo -> PlayerInfo
use5 i x@(PlayerInfo h d f p l ) = PlayerInfo h d (S.adjust' useResource i f) p l   
incrementTurn :: Num a => a -> Player -> (a, Player)
incrementTurn a b = (a+1,switchPlayer b)
(^~^) :: (Turn, Priority) -> (Table, Pair PlayerInfo, EffectStack) -> Game
(a,b) ^~^ (c,d,e) = Continue a b c d e
-- end helpers --

-- down here is the FULL GAME ACTION (tm)

staticCostList :: [(GameAction,Cost)]
staticCostList = [(Give,[])]

-- full a g = case costCheck a g of
--            False -> g
--            True -> winCheck . runFX $ action a g
-- god this is gonna be a bitch to write i just know it
