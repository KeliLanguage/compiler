module Util where

import Data.List
import Ast
import Data.Maybe
import Data.Set hiding (map, (\\))

data MatchResult 
    = GotDuplicates
    | ZeroIntersection
    | GotExcessive [StringToken] -- means there are extraneous elements in source
    | Missing [String]           -- means some elements in target is not matched
    | PerfectMatch               -- means all elements in source matches all elements in target
    deriving (Show)

-- NOTE: 
--  source also means actual elements
--  target also means expected elements
match :: [StringToken] -> [StringToken] -> MatchResult
match source target =
    let source' = map snd source in
    let target' = map snd target in
    let intersection = intersect source' target' in
    let intersectionLength = length intersection in
    if length (nub source') /= length source' then
        GotDuplicates
    else if intersectionLength == 0 then
        ZeroIntersection
    else if intersectionLength == length target then (
        if length source > length target then
            let excessiveCases = 
                    catMaybes (map 
                        (\x -> find (\y -> snd y == x) source) 
                        (toList (difference (fromList source') (fromList target')))) in
            GotExcessive excessiveCases
        else
            PerfectMatch
    )
    else if intersectionLength < length target then
        Missing (target' \\ source')
    else
        error "impossible"
