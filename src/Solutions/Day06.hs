module Solutions.Day06 where

day06a :: NonEmpty String -> Either String Int
day06a = Right . sum . map countQuestions . breakAll "" . toList

countQuestions :: [String] -> Int
countQuestions = length . unstableNub . mconcat

day06b :: NonEmpty String -> Either String Int
day06b = undefined
