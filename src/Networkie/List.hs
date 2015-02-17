module Networkie.List(
  unfoldRest
  ) where

import Prelude()
import ClassyPrelude

unfoldRest :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldRest fct ini = go fct ini mempty
  where                                               
    go f s acc =                                      
      case f s of                                     
       Nothing -> (acc, s)                           
       Just (a, b) -> go f b (acc <> [a])            
