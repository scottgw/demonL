module ParserPosition where

import Control.Monad

import Text.Parsec
import Text.Parsec.ByteString

data Pos a = Pos SourcePos a deriving (Eq)

instance Show a => Show (Pos a) where
    show = show . contents -- show (position p) ++ "> " ++ show (contents p)

instance Functor Pos where
    fmap f (Pos s a) = Pos s (f a)

showUnpos :: Show a => Pos a -> String
showUnpos = show . contents

inheritPos :: (Pos a -> b) -> Pos a -> Pos b
inheritPos f a = attachPos (position a) (f a)

attachPos :: SourcePos -> a -> Pos a
attachPos = Pos

attachPosM :: Monad m => m SourcePos -> m a -> m (Pos a)
attachPosM = liftM2 attachPos

attachPosHere :: a -> Parser (Pos a)
attachPosHere a = flip attachPos a `fmap` getPosition

attachPosBefore :: Parser a -> Parser (Pos a)
attachPosBefore = attachPosM getPosition

position :: Pos a -> SourcePos
position (Pos p _) = p

contents :: Pos a -> a
contents (Pos _ a) = a
