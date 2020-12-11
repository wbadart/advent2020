{-# LANGUAGE FlexibleContexts,OverloadedStrings #-}

module Solutions.Day08
  ( day08a
  , day08b
  ) where

data Instr = Acc Int | Jmp Int | Nop Int deriving Show
type ProgramState = (PC, Accumulator, [PC])

day08a :: NonEmpty String -> Either String Int
day08a strings = do
  prog <- maybeToRight "bad parse" . traverse (parse parseInstr) $ toList strings
  return (execInterp prog ^. _2)

day08b :: NonEmpty String -> Either String Int
day08b strings = do
  prog <- maybeToRight "bad parse" . traverse (parse parseInstr) $ toList strings
  maybeToRight "no solution" $ viaNonEmpty head do
    pc <- execInterp prog ^. _3
    guard (not . isAcc $ prog ^. idx pc)
    let (pc', acc', _) = prog & idx pc %~ switch & execInterp
    guard (pc' == length prog)
    pure acc'

interpret :: ProgramState -> Program -> ProgramState
interpret s@(pc, acc, visited) prog
  | pc `elem` visited || pc == length prog = s
  | otherwise =
      let visited' = pc : visited
          (pc', acc') =
            case prog ^. idx pc of
              Acc i -> (succ pc, acc + i)
              Jmp i -> (pc + i,  acc)
              Nop _ -> (succ pc, acc)
       in interpret (pc', acc', visited') prog

execInterp :: Program -> ProgramState
execInterp = interpret (0, 0, [])

-- ==========

parseInstr :: Parser Instr
parseInstr =
      (Acc <$> (string "acc " *> signedNumber))
  <|> (Jmp <$> (string "jmp " *> signedNumber))
  <|> (Nop <$> (string "nop " *> signedNumber))

isAcc :: Instr -> Bool
isAcc = \case Acc _ -> True; _ -> False

type Program = [Instr]
type PC = Int
type Accumulator = Int

switch :: Instr -> Instr
switch = \case Jmp i -> Nop i; Nop i -> Jmp i; x -> x
