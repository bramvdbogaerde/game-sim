module Game where

import Data.Vector hiding (map, zip, (++))
import Data.List ((\\))

-- | A card is a combination of the number 
-- on the card and the number of heads on the card 
data Card = Card { num :: Int, heads :: Int }

data Player = Player {
   -- | The set of cards currently in the player's hand
   hand   :: [Card],
   -- | The set of cards taken by the player
   taken  :: [Card]
}

-- | Give the given card to the given player
giveCard :: Card -> Player -> Player
giveCard card pl = pl { hand = card : (hand pl) }

-- | Let the player ake the give card 
takeCard :: Card -> Player -> Player
takeCard card pl = pl { taken = card : (taken pl) }


-- | The state of the game consists of a number 
-- of players and five stacks of cards
data GameState = GameState {
   players :: [Player],
   stacks :: Vector [Card]
}

-- | A mapping from the number on a card to its value
values :: [(Int, Int)] 
values = specials ++ remaining
   where specials    = [(55, 7), (11, 5), (22, 5), (33, 5), (44, 5), (66, 5),
                       (77, 5), (88, 5), (99, 5), (10, 3), (20, 3), (20, 4),
                       (30, 4), (40, 4), (50, 4), (60, 4), (70, 4), (80, 4),
                       (90, 4),(100, 4), (5,  3), (15, 3), (25, 3), (35, 3),
                       (45, 3), (65, 3), (75, 3), (85, 3), (95, 3)]
         remaining  = (zip ([0 .. 105] \\ (map fst specials)) (repeat 1))

type Seed = Int

-- | Create an initial game state
initialState :: Int    -- ^ the number of players in the game
             -> Seed   -- ^ seed to shuffle the game deck before handing out cards
             -> GameState
initialState = undefined
