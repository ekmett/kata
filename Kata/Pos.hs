module Kata.Pos where

import Control.Category
import Prelude hiding ((.), id)
import Data.Monoid
import Data.Monoid.Reducer
import Data.Record.Label

newtype GccFlags = GccFlags { getGccFlags :: {-# UNPACK #-} !Word8 }

bitLabel :: Bits f => Int -> w :-> Bool
bitLabel n = label (\w -> testBit w n) (\i w -> if i then setBit w n else clearBit w n)

gccFlags :: GccFlags :-> Word8
gccFlags = label GccFlags (const . GccFlags)

present = gccFlags . bitLabel 0
startOfFile = gccFlags . bitLabel 1
returningToFile = gccFlags . bitLabel 2
systemHeader = gccFlags . bitLabel 3
externC = gccFlags . bitLabel 4

mkGccFlags :: [Int] -> GccFlags
mkGccFlags [] = GccFlags 0
mkGccFlags xs = GccFlags $ foldr (\a r -> if a >= 1 && a <= 4 then setBit r a else r) 1 xs

instance Monoid GccFlags where
    mempty = GccFlags 0 
    x `mappend` y | not (get present y) = x
                  | otherwise = y

-- GHC's file manipulation has already collapsed \r\n and \n\r into \n

-- Pos "Foo" 1 3 Nothing <> Pos "Bar" 1 2 (Just (GccFlags _ True _ _)

data Loc a = Loc a !Int !Int !(GccFlags) 

data Pos a = CppFile a {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !GccFlags !(Seq Loc) !(Seq Loc)
           | CppLine {-# UNPACK #-} !Int {-# UNPACK #-} !Int   {- #line linenum -}
           | Lines {-# UNPACK #-} !Int {-# UNPACK #-} !Int    
           | Columns {-# UNPACK #-} !Int                          
           | Tab {-# UNPACK #-} !Int {-# UNPACK #-} !Int


{-
In file included from pointmain.c:2:
In file included from point.h:23:
vector:12:34: error: no member named 'horisontal' 'in struct Point'; did you mean 'horizontal'?
    printf "%d, %d\n" p1.vertical p1.horisontal
                                     ^~~~~~~~~~
                                     horizontal
In file included from pointmain.c:2:
./point.h:5:19: note: 'horizontal' declared here
    vertical, horizontal : double
              ^
-}

-- | Compute the location of the next standard 8-column aligned tab
nextTab :: Int -> Int
nextTab !x = x + (8 - (x-1) `mod` 8)

instance Functor Pos where
    fmap f (Pos a x y z) = Pos (f a) x y z
    fmap f (Lines x y) = Lines x y
    fmap f (Columns x) = Columns x
    fmap g (Tab x y) = Tab x y

instance Pointed Pos where
    point a = Pos a 1 1 (Just (GccFlags True False False False))

-- only addresses perfect pairings

bitonic :: Seq a -> Seq b -> Seq a -> Seq b -> (Seq a -> Seq b -> r) -> r
bitonic ca oa cb ob k = k (ca `mappend` drop (length oa) cb) 
                          (drop (length cb) oa `mappend` ob) 

-- accumulate partial information
instance Monoid (Pos a) where
    mempty = Columns 0

    Pos _ _ _ g out inn `mappend` Pos f l c Nothing out' inn' = bitonic out inn out' inn' (Pos f l c g)
    Pos f l _ g `mappend` Lines m d         = Pos f (l + m) d g
    Pos f l c g `mappend` Columns d         = Pos f l (c + d) g
    Pos f l c g `mappend` Tab x y           = Pos f l (nextTab (c + x) + y) g

    Lines l _ `mappend` Lines m d = Lines (l + m) d 
    Lines l c `mappend` Columns d = Lines l (c + d) 
    Lines l c `mappend` Tab x y   = Lines l (nextTab (c + x) + y) 

    Columns c `mappend` Columns d  = Columns (c + d)
    Columns c `mappend` Tab x y    = Tab (c + x) y

    Tab _ _   `mappend` Lines m d = Lines m d 
    Tab x y   `mappend` Columns d  = Tab x (y + d)
    Tab x y   `mappend` Tab x' y'  = Tab x (nextTab (y + x') + y')
    _         `mappend` pos        = pos

instance Reducer Word8 (Pos a) where
    unit 0x0a                      = Lines 1 1 -- newline
    unit 0x0d                      = Columns 0 -- carriage return (TODO: mode switch more gracefully for macs)
    unit 0x09                      = Tab 0 0   -- tab
    unit c | c >= 0x80 && c < 0xc0 = Columns 0 -- UTF8 tail byte
    unit _                         = Columns 1 -- UTF8 head byte

instance Reducer Char (Pos a) where
    unit '\n' = Lines 1 1
    unit '\r' = Columns 0
    unit '\t' = Tab 0 0 
    unit _    = Columns 1

{-
type Args = ()

data StartMacro = Macro Pos Args

data MacroPos = NoMacro Pos
              | (Seq (Pos,EndMacro)) (Seq (StartMacro, Pos))
-}
