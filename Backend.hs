{-# LANGUAGE TemplateHaskell #-}
module Backend where
import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as V


data OpCode
  = DIE -- ^ processor self destructs
  | DATA -- ^ no-op or data
  | COPY -- ^ Overwrite B with the value of A
  | TRANSFER -- ^ add A to B and set B to 0. You can transfer money.
  | ADD -- ^ add A to B
  | SUBTRACT -- ^ subtract A from B
  | JUMP -- ^ unconditional jump to A
  | MAKEMONEY -- ^ set A to 0 and mark it as money cell
  | MAKEPROC -- ^ marke A as a processor cell

  -- conditional operations
  
  | ISEQUAL -- ^ skip ahead if A = B
  | ISLESS -- ^ skip ahead if A < B
  | ISZERO -- ^ skip ahead if A is zero
  | ISMONEY -- ^ skip ahead if A is money
  | ISPROC -- ^ skip ahead if A is a processor
  deriving (Read,Show,Enum)
opSize :: OpCode -> Number
opSize = (+1) . abs . fromEnum
type Number = Int
data Cell = Cell
  {_n           :: Number
  ,_isProcessor :: Bool
  ,_isMoney     :: Bool}
  deriving Show

data Universe = Universe
  {_universe_size :: Number
  ,_memory        :: Vector Cell
  -- Stored so we don't have to iterate over all of space every iteration:
  -- (Doesn't chaneg universe behavior)
  ,_processors    :: Set Number}
makeLenses ''Cell
makeLenses ''Universe
pattern MaxStep :: Number
pattern MaxStep = 100

transition :: M ()
transition = mapM_ execute =<< use processors

type M = State Universe

(%) = mod
infixl 7 %

execute :: Number -> M ()
execute loc = do
  u <- get
  let [instruction,adist,bdist] = [0..2] >>= \i ->
        u^..memory.ix((loc + i)%u^.universe_size).n

  -- Cannot read, write or jump to cells outside of range:
  if abs adist > MaxStep || abs bdist > MaxStep then deleteProcessor loc
    else do
      --let [a,b] = [adist,bdist] >>= \j -> u..memory.ix((loc+j)%u^.universe_size)
      let
        a,b :: Traversal' Universe Cell
        _a,_b :: Cell
        a = memory.ix((loc+adist)%u^.universe_size); _a = u^?!a
        b = memory.ix((loc+bdist)%u^.universe_size); _b = u^?!b
      case toEnum $ instruction - 1 of
        DATA                                  -> move loc 3
        COPY     | not $ _isMoney _b          -> b .= _a
        TRANSFER | _isMoney _a == _isMoney _b -> do b.n += _n _a; a.n .= 0
        ADD      | not $ _isMoney _b          -> do b.n += _n _a; move loc 3
        SUBTRACT | not $ _isMoney _b          -> do b.n -= _n _a; move loc 3
        JUMP                                  -> move loc adist
        MAKEMONEY                             -> do a.n .= 0; a.isMoney .= True
        MAKEPROC                              -> a.isProcessor .= True
        ISEQUAL                               -> moveIf loc $ _a^.n == _n _b
        ISLESS                                -> moveIf loc $ _a^.n < _n _b
        ISZERO                                -> moveIf loc $ _a^.n == 0
        ISMONEY                               -> moveIf loc $ _a^.isMoney
        ISPROC                                -> moveIf loc $ _a^.isProcessor
        _                                     -> deleteProcessor loc

addProcessor :: Number -> M ()
addProcessor i = do processors . at i ?= ()
                    memory . ix i . isProcessor .= True
deleteProcessor :: Number -> M ()
deleteProcessor i = do processors . at i .= Nothing
                       memory . ix i . isProcessor .= False

move :: Number -> Number -> M ()
move loc dist = do
  u <- get
  deleteProcessor loc
  when (abs dist <= MaxStep)
    . addProcessor $ (loc + dist) % u^.universe_size

moveIf :: Number -> Bool -> M ()
moveIf i p = move i if p then 6 else 3
