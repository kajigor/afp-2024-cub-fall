import GHC.Base
{-# RULES "map/map" forall f g xs. map f (map g xs) = map (f . g) xs #-}

-- Name is necessary, it is used by ghc in output

-- Each variable mentioned in a rule must either be in scope (e.g. map), or bound by the forall (e.g. f, g, xs).
-- The variables bound by the forall are called pattern variables.


{-# RULES
"fold/build"  forall k z (g :: forall b. (a -> b -> b) -> b -> b).
              GHC.Base.foldr k z (build g) = g k z
  #-}

-- here g is polymorphic and MUST have a type a type signature. Otherwise it MAY have one

-- {-# RULES
-- "wrong1"   forall e1 e2.  case True of { True -> e1; False -> e2 } = e1
-- "wrong2"   forall f.      f True = True
--  #-}

  -- Incorrect rules:
{-# RULES
"double reverse" forall xs. reverse (reverse xs) = xs
  #-}

-- reverse (reverse [1..]) != [1..] -- because it won't terminate

-- {-# RULES
-- "loop" forall x y. f x y = f y x
--   #-} -- GHC won't check for termination and therefore go to the infinite loop

-- {-# RULES
-- "f/f" forall x. f (f x) = f x
-- "f/g" forall x. f (g x) = fg x
-- "fff" forall x. f (fg x) = fg x
--   #-}

-- f . f . g -> f . fg or fg?
-- GHC doesn't check for independece of order


-- Methods will be specialized by GHC before rewrite rules have a chance to be applied. 
-- Such rules won’t fire because the types of specialized functions won’t match the types specified in the rewrite rules.