module Language.Regular.Property where

import Prelude hiding (null)

class Property p where
	-- | @null p@ should hold exactly when no value @v@ matches @p@.
	null :: p -> Bool

	-- | If @b_i@ tells whether @p_i@ matches value @v@ for each @i@ in @1@ to
	-- @n@, then @combine [p_1, ..., p_n] bs@ should match @v@ exactly when @bs
	-- = [b_1, ..., b_n]@. The result of calling @combine@ with two arguments
	-- whose length do not match is undefined (in the C sense -- instances can
	-- do whatever they like, including throwing exceptions).
	--
	-- We use this type rather than @[(p, Bool)] -> p@ so that @combine@ may do
	-- some precomputation on its first argument if it wants to.
	combine :: [p] -> [Bool] -> p

equiv p1 p2 = null (f [True, False]) && null (f [False, True])
	where f = combine [p1, p2]
