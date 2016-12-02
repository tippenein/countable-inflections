-- |
-- Module      :  Text.Countable
-- Copyright   :  © 2016 Brady Ouren
-- License     :  MIT
--
-- Maintainer  :  Brady Ouren <brady@andand.co>
-- Stability   :  experimental
-- Portability :  portable
--
-- pluralization and singularization transformations

{-# LANGUAGE OverloadedStrings #-}

module Text.Countable
  ( pluralize
  , pluralizeWith
  , singularize
  , singularizeWith
  , makeMatchMapping
  , makeIrregularMapping
  , makeUncountableMapping
  )
where

import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Countable.Data
import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils (substitute')
import System.IO.Unsafe

type RegexPattern = T.Text
type RegexReplace = T.Text
type Singular = T.Text
type Plural = T.Text

data Inflection
  = Simple (Singular, Plural)
  | Match (Maybe Regex, RegexReplace)

-- | pluralize a word given a default mapping
pluralize :: T.Text -> T.Text
pluralize = pluralizeWith mapping
  where
    mapping = defaultIrregulars ++ defaultUncountables ++ defaultPlurals

-- | singularize a word given a default mapping
singularize :: T.Text -> T.Text
singularize = singularizeWith mapping
  where
    mapping = defaultIrregulars ++ defaultUncountables ++ defaultSingulars

-- | pluralize a word given a custom mapping.
-- Build the [Inflection] with a combination of
-- `makeUncountableMapping` `makeIrregularMapping` `makeMatchMapping`
pluralizeWith :: [Inflection] -> T.Text -> T.Text
pluralizeWith = lookupWith pluralLookup

-- | singularize a word given a custom mapping.
-- Build the [Inflection] with a combination of
-- `makeUncountableMapping` `makeIrregularMapping` `makeMatchMapping`
singularizeWith :: [Inflection] -> T.Text -> T.Text
singularizeWith =  lookupWith singularLookup

lookupWith :: (Text -> Inflection -> Maybe Text) -> [Inflection] -> Text -> Text
lookupWith f mapping target = fromMaybe target $ headMaybe matches
  where
    matches = catMaybes $ fmap (f target) (Prelude.reverse mapping)

-- | Makes a simple list of mappings from singular to plural, e.g [("person", "people")]
-- the output of [Inflection] should be consumed by `singularizeWith` or `pluralizeWith`
makeMatchMapping :: [(RegexPattern, RegexReplace)] -> [Inflection]
makeMatchMapping = fmap (\(pat, rep) -> Match (regexPattern pat, rep))

-- | Makes a simple list of mappings from singular to plural, e.g [("person", "people")]
-- the output of [Inflection] should be consumed by `singularizeWith` or `pluralizeWith`
makeIrregularMapping :: [(Singular, Plural)] -> [Inflection]
makeIrregularMapping = fmap Simple

-- | Makes a simple list of uncountables which don't have
-- singular plural versions, e.g ["fish", "money"]
-- the output of [Inflection] should be consumed by `singularizeWith` or `pluralizeWith`
makeUncountableMapping :: [T.Text] -> [Inflection]
makeUncountableMapping = fmap (\a -> Simple (a,a))


defaultPlurals :: [Inflection]
defaultPlurals = makeMatchMapping defaultPlurals'

defaultSingulars :: [Inflection]
defaultSingulars = makeMatchMapping defaultSingulars'

defaultIrregulars :: [Inflection]
defaultIrregulars = makeIrregularMapping defaultIrregulars'

defaultUncountables :: [Inflection]
defaultUncountables = makeUncountableMapping defaultUncountables'

pluralLookup :: T.Text -> Inflection -> Maybe T.Text
pluralLookup t (Match (r1,r2)) = runSub (r1,r2) t
pluralLookup t (Simple (a,b)) = if t == a then Just b else Nothing

singularLookup :: T.Text -> Inflection -> Maybe T.Text
singularLookup t (Match (r1,r2)) = runSub (r1,r2) t
singularLookup t (Simple (a,b)) = if t == b then Just a else Nothing

runSub :: (Maybe Regex, RegexReplace) -> T.Text -> Maybe T.Text
runSub (Nothing, _) _ = Nothing
runSub (Just reg, rep) t = matchWithReplace (reg, rep) t

matchWithReplace :: (Regex, RegexReplace) -> T.Text -> Maybe T.Text
matchWithReplace (reg, rep) t =
  if regexMatch t reg
  then toMaybe $ substitute' reg (encodeUtf8 t) (encodeUtf8 rep)
  else Nothing
  where
    toMaybe = either (const Nothing) (Just . decodeUtf8)

regexMatch :: T.Text -> Regex -> Bool
regexMatch t r = case match of
                   Left _ -> False
                   Right m -> isJust m
  where match = unsafePerformIO $ execute r (encodeUtf8 t)

regexPattern :: T.Text -> Maybe Regex
regexPattern pat = toMaybe reg
  where toMaybe = either (const Nothing) Just
        reg = unsafePerformIO $ compile compCaseless execBlank (encodeUtf8 pat)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x
