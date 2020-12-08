{-# LANGUAGE FlexibleContexts,OverloadedStrings #-}

module Solutions.Day08 where

import qualified Data.Set as S

import Parse

day08a :: NonEmpty String -> Either String Int
day08a strings = do
  prog <- maybeToRight "bad parse" $ traverse (evalParser parseInstr) $ toList strings
  pure $ evalState (interpret prog) (0, 0, S.empty)

day08b :: NonEmpty String -> Either String Int
day08b strings = do
  prog <- maybeToRight "bad parse" $ traverse (evalParser parseInstr) $ toList strings
  pure $ undefined

data Instr = Acc Int | Jmp Int | Nop Int deriving Show

type Program = [Instr]
type PC = Int
type Accumulator = Int
type ProgramState = (PC, Accumulator, Set PC)

interpret :: Program -> State ProgramState Accumulator
interpret prog = get >>= \(pc, acc, visited) ->
  if pc `S.member` visited
     then pure acc
     else do
       execInstr prog (pure ())
       interpret prog

execInstr :: MonadState (PC, Accumulator, Set PC) m => Program -> m () -> m ()
execInstr prog e = get >>= \(pc, acc, visited) -> do
  let visited' = S.insert pc visited
  case prog !!? pc of
    Just (Acc i) -> put (succ pc, acc + i, visited')
    Just (Jmp i) -> put (pc + i,  acc,     visited')
    Just (Nop _) -> put (succ pc, acc,     visited')
    Nothing      -> e

-- ==========

parseInstr :: Parser String Instr
parseInstr =
      (Acc <$> (string "acc " *> signedNumber))
  <|> (Jmp <$> (string "jmp " *> signedNumber)
  <|> (Nop <$> (string "nop " *> signedNumber)))

type Except e = ExceptT e Identity
throw :: e -> Except e a
throw = ExceptT . Identity . Left
