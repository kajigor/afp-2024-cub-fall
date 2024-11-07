-- Does it make sense to inline at a particular call site?
map f xs = map (\x -> body) xs -- This is not any better than the original, so GHC does not inline it.

-- GHC only inlines functions that are applied to as many arguments as they have syntactically on the left-hand side (LHS) of function definition.
-- Otherwise the body would need to be wrapped with a lambda anyway.

comp1 f g = \x -> f (g x)
comp2 f g x = f (g x)

map (comp1 not not) xs > map (comp2 not not) xs 





-- How much code duplication inlining would cause? 
-- Code bloat is bad as it increases compilation time, size of program, and lowers cache hit rates.




-- How much work duplication would inlining cause? 

-- Assuming that foo is some computational-expensive functions
let x = foo 1000 in x + x -- Inlining is a bad idea here

let x = foo 1000
    f = \y -> x * y
in [(f 1), (f 2), (f 3), (f 4)] -- Computing x can be duplicated even though x appears once
-- If we inline x in its occurrence site it will be evaluated every time f is called. This is why inlining inside a lambda may be not a good idea.
