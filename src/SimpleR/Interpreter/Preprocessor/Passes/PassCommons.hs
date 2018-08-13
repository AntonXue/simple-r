{-# LANGUAGE FlexibleInstances #-}

module SimpleR.Interpreter.Preprocessor.Passes.PassCommons where

import SimpleR.Language

data PassResult a =
    PassOkay a
  | PassFail [String]

instance Functor PassResult where
  fmap f (PassOkay a) = PassOkay (f a)
  fmap f (PassFail msgs) = PassFail msgs

instance Applicative PassResult where
  pure a = PassOkay a
  
  (PassOkay f) <*> (PassOkay a) = PassOkay (f a)
  (PassFail msgs1) <*> (PassFail msgs2) = PassFail (msgs1 ++ msgs2)
  (PassFail msgs) <*> _ = PassFail msgs
  _ <*> (PassFail msgs) = PassFail msgs

instance Monad PassResult where
  return a = PassOkay a

  (PassOkay a) >>= f = f a
  (PassFail msgs) >>= _ = PassFail msgs


