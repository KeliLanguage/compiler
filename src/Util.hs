module Util where

import Prelude hiding (id)
import Data.List
import Control.Monad    
import qualified Ast.Raw as Raw
import Data.Maybe
import Data.Set hiding (map, (\\), foldl')

data OneOf3 a b c = First a | Second b | Third c deriving (Show)

data MatchResult 
    = GotDuplicates [Raw.StringToken]
    | ZeroIntersection
    | GotExcessive [Raw.StringToken] -- means there are extraneous elements in source
    | Missing [String]           -- means some elements in target is not matched
    | PerfectMatch               -- means all elements in source matches all elements in target
    deriving (Show, Eq)

-- NOTE: 
--  source also means actual elements
--  target also means expected elements
match :: [Raw.StringToken] -> [Raw.StringToken] -> MatchResult
match source target =
    let source' = sort (map snd source) in
    let target' = sort (map snd target) in
    let intersection' = intersect source' target' in
    let intersectionLength = length intersection' in
    case findDuplicates source of
        Just duplicates ->
            GotDuplicates duplicates
        Nothing ->
            if intersectionLength == 0 then
                ZeroIntersection
            else if intersectionLength == length target then (
                if length source > length target then
                    let excessiveCases = 
                            catMaybes (map 
                                (\x -> find (\y -> snd y == x) source) 
                                (toList (difference (fromList source') (fromList target')))) in
                    GotExcessive excessiveCases
                else
                    PerfectMatch)
            else if intersectionLength < length target then
                Missing (target' \\ source')
            else
                error "impossible"
            
findDuplicates :: [Raw.StringToken] -> Maybe [Raw.StringToken]
findDuplicates tokens = 
    let result = 
            foldl' 
            (\acc t@(pos, id) -> 
                if acc == [] then
                    case find (\(pos', id') -> pos /= pos' && id == id') tokens of
                        Just t' ->
                            [t, t']
                        Nothing ->
                            []
                else 
                    acc)
            []
            tokens in
    if length result > 0 then
        Just result
    else 
        Nothing


