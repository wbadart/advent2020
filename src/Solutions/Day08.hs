{-# LANGUAGE FlexibleContexts,OverloadedStrings,ViewPatterns #-}

module Solutions.Day08 where

import Relude.Unsafe ( (!!) )
import Parse

day08a :: NonEmpty String -> Either String Int
day08a strings = do
  prog <- maybeToRight "bad parse" $ traverse (evalParser parseInstr) $ toList strings
  pure $ getAcc (execInterp prog)

day08b :: NonEmpty String -> Either String Int
day08b strings = do
  prog <- maybeToRight "bad parse" $ traverse (evalParser parseInstr) $ toList strings
  maybeToRight "no solution" $ viaNonEmpty head do
    let finalState = execInterp prog
    pc <- getTrace finalState
    let instr = prog !! pc
    guard $ not (isAcc instr)
    let prog' = splice pc (switch instr) prog
    let finalState' = execInterp prog'
    guard $ getPC finalState' == length prog'
    pure $ getAcc finalState'

interpret :: Program -> State ProgramState ()
interpret prog = get >>= \(pc, _acc, visited) ->
  if pc `elem` visited || pc == length prog
     then pass
     else do
       execInstr prog
       interpret prog

execInstr :: MonadState ProgramState m => Program -> m ()
execInstr prog = get >>= \(pc, acc, visited) -> do
  let visited' = pc : visited
  case prog !!? pc of
    Just (Acc i) -> put (succ pc, acc + i, visited')
    Just (Jmp i) -> put (pc + i,  acc,     visited')
    Just (Nop _) -> put (succ pc, acc,     visited')
    Nothing      -> error "out of program bounds"

execInterp :: Program -> ProgramState
execInterp = (`execState` initialState) . interpret
  where initialState = (0, 0, [])


-- ==========


parseInstr :: Parser String Instr
parseInstr =
      (Acc <$> (string "acc " *> signedNumber))
  <|> (Jmp <$> (string "jmp " *> signedNumber)
  <|> (Nop <$> (string "nop " *> signedNumber)))

data Instr = Acc Int | Jmp Int | Nop Int deriving Show

isAcc :: Instr -> Bool
isAcc = \case Acc _ -> True; _ -> False

type Program = [Instr]
type PC = Int
type Accumulator = Int
type ProgramState = (PC, Accumulator, [PC])
getPC = \case (p, _, _) -> p
getAcc = \case (_, a, _) -> a
getTrace = \case (_, _, t) -> t

switch :: Instr -> Instr
switch = \case Jmp i -> Nop i; Nop i -> Jmp i; x -> x

splice :: Int -> a -> [a] -> [a]
splice i x (splitAt i -> (lhs, _:rhs)) = lhs <> (x:rhs)
