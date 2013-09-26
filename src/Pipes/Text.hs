module Pipes.Text
  ( decodeUtf8
  , encodeUtf8
  , pack
  , unpack
  , head
  , null
  , length
  , toCaseFold
  , toLower
  , toUpper
  , foldl
  , concatMap
  , any
  , all
  , maximum
  , minimum
  , scanl
  , replicate
  , take
  , takeWhile
  , drop
  , dropWhile
  , stripStart
  , nextChar
  , P.replicateM
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Pipes
import Prelude hiding
  ( all
  , any
  , concat
  , concatMap
  , drop
  , dropWhile
  , foldl
  , head
  , length
  , replicate
  , scanl
  , take
  , takeWhile
  , maximum
  , minimum
  , null
  )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Pipes.Prelude as P

-- | Transform a Pipe of 'ByteString's expected to be UTF-8 encoded
-- into a Pipe of Text
decodeUtf8 :: Monad m => Pipe ByteString Text m r
decodeUtf8 = go TE.streamDecodeUtf8
  where go enc = do
            chunk <- await
            let TE.Some text _ enc' = enc chunk
            yield text
            go enc'
{-# INLINEABLE decodeUtf8 #-}

-- | Transform a Pipe of 'Text' into a Pipe of 'ByteString's using UTF-8
-- encoding
encodeUtf8 :: Monad m => Pipe Text ByteString m r
encodeUtf8 = P.map TE.encodeUtf8
{-# INLINEABLE encodeUtf8 #-}

pack :: Monad m => Pipe String Text m r
pack = P.map T.pack
{-# INLINEABLE pack #-}

unpack :: Monad m => Pipe Text String m r
unpack = P.map T.unpack
{-# INLINEABLE unpack #-}

nextChar :: Monad m => Producer Text m r -> m (Either r (Char, Producer Text m r))
nextChar = go
  where
    go p = do
        x <- next p
        case x of
            Left   r         -> return (Left r)
            Right (text, p') -> case (T.uncons text) of
                Nothing         -> go p'
                Just (c, text') -> return (Right (c, yield text' >> p'))
{-# INLINABLE nextChar #-}

head :: Monad m => Producer Text m () -> m (Maybe Char)
head = go
  where go p = do
            x <- nextChar p
            case x of
                Left _      -> return Nothing
                Right (c,_) -> return $ Just c
{-# INLINEABLE head #-}

null :: Monad m => Producer Text m () -> m Bool
null = P.all T.null
{-# INLINEABLE null #-}

length :: (Monad m, Num n) => Producer Text m () -> m n
length = P.fold (\n t -> n + (fromIntegral $ T.length t)) 0 id
{-# INLINEABLE length #-}


----------------
-- case conversion
----------------

toCaseFold :: Monad m => Pipe Text Text m ()
toCaseFold = P.map T.toCaseFold
{-# INLINEABLE toCaseFold #-}

toLower :: Monad m => Pipe Text Text m ()
toLower = P.map T.toLower
{-# INLINEABLE toLower #-}

toUpper :: Monad m => Pipe Text Text m ()
toUpper = P.map T.toUpper
{-# INLINEABLE toUpper #-}

----------------
-- folds
----------------

foldl
    :: Monad m
    => (a -> Char -> a) -> a -> (a -> r) -> Producer Text m () -> m r
foldl step begin done = P.fold (T.foldl' step) begin done
{-# INLINEABLE foldl #-}

concatMap :: Monad m => (Char -> Text) -> Pipe Text Text m ()
concatMap = P.map . T.concatMap
{-# INLINEABLE concatMap #-}

any :: Monad m => (Char -> Bool) -> Producer Text m () -> m Bool
any = P.any . T.any
{-# INLINEABLE any #-}

all :: Monad m => (Char -> Bool) -> Producer Text m () -> m Bool
all = P.all . T.all
{-# INLINEABLE all #-}

maximum :: Monad m => Producer Text m () -> m (Maybe Char)
maximum = P.fold (minMaxStep max T.maximum) Nothing id
{-# INLINEABLE maximum #-}

minMaxStep :: (Char -> Char -> Char) -> (Text -> Char) ->
              Maybe Char -> Text -> Maybe Char
minMaxStep f tf old t = if T.null t
                          then old
                          else Just $ maybe new (f new) old
  where new = tf t
{-# INLINEABLE minMaxStep #-}

minimum :: Monad m => Producer Text m () -> m (Maybe Char)
minimum = P.fold (minMaxStep min T.minimum) Nothing id
{-# INLINEABLE minimum #-}

-----------------
-- scans
-----------------

scanl :: Monad m => (Char -> Char -> Char) -> Char -> Pipe Text Text m r
scanl step = go
  where go char = do
            text <- await
            let (char', text') =
                  T.mapAccumL (\a b -> (step a b, step a b)) char text
            yield text'
            go char'
{-# INLINEABLE scanl #-}


------------
-- generating
------------

replicate :: Monad m => Int -> Text -> Producer Text m ()
replicate n text = P.replicateM n (return text)
{-# INLINEABLE replicate #-}


------------
-- substrings
------------

take :: Monad m => Int -> Pipe Text Text m ()
take = go
  where go n
          | n <= 0    = return ()
          | otherwise = do
              text <- await
              let text' = T.take n text
                  taken = fromIntegral $ T.length text'
              yield text'
              if taken < n
                then go (n - taken)
                else return ()
{-# INLINEABLE take #-}

drop :: Monad m => Int -> Pipe Text Text m r
drop = go
  where go n
          | n <= 0    = cat
          | otherwise = do
              text <- await
              let len = T.length text
              if len >= n
                then do
                    yield $ T.drop n text
                    cat
                else do
                    yield text
                    go (len - n)
{-# INLINEABLE drop #-}


takeWhile :: Monad m => (Char -> Bool) -> Pipe Text Text m ()
takeWhile f = do
    text <- await
    let (prefix,remainder) = T.span f text
    yield prefix
    if T.null remainder
      then takeWhile f
      else return ()
{-# INLINEABLE takeWhile #-}

dropWhile :: Monad m => (Char -> Bool) -> Pipe Text Text m r
dropWhile f = do
    text <- await
    let remainder = T.dropWhile f text
    if T.null remainder
      then dropWhile f
      else do
          yield remainder
          cat
{-# INLINEABLE dropWhile #-}

stripStart :: Monad m => Pipe Text Text m r
stripStart = do
    chunk <- await
    let text = T.stripStart chunk
    if T.null text
      then stripStart
      else cat
{-# INLINEABLE stripStart #-}
