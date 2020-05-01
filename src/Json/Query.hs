module Json.Query
  ( Path(..)
  , query
  ) where

-- Revisit this later.
-- Do not release this library with this module still here.

data Path
  = Key {-# UNPACK #-} !ShortText !Path
    -- ^ JSON path element of a key into an object, \"object.key\".
  | Index {-# UNPACK #-} !Int !Path
    -- ^ JSON path element of an index into an array, \"array[index]\".
  | Nil

query :: Path -> Value -> Maybe Value
query = go where
  go Nil v = Just v
  go (Key k p) (Object mbrs) = foldr
    (\(Member key val) other -> if key == k
      then Just val
      else other
    ) Nothing mbrs >>= go p
  go (Index i p) (Array vs) = Chunks.index vs i >>= go p
  go _ _ = Nothing

-- Example Use:
-- data Foo = Foo String String String
-- object :: Parser Value (Chunks _)
-- myParser :: Parser Value Foo
-- myParser =
--   object
--   >->
--   Foo
--     <$> path "foo"
--     <*> (path "bar" >-> object >-> path "baz")
--     <*> path "gaz"
