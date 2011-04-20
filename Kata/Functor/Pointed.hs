module Kata.Functor.Pointed where

class Functor f => Pointed f where
    point :: a -> f a
