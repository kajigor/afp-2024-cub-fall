// Here, we changed the order of function `sum`. Still we want to prove the equality between `sum` and `sum_method`.
// Fill in the invariants, prove the lemma and use them inside the method.


// not the difference between previous and current implementations of `sum`
function sum(s: seq<int>) : int {
    if |s| == 0 then 0 else s[0] + sum(s[1..])
}

lemma sum_prop(s1: seq<int>, s2: seq<int>)
    ensures sum(s1 + s2) == sum(s1) + sum(s2)
{
    if |s1| == 0 {
        assert s1 + s2 == s2;
    } else {
        var s1head := s1[0];
        var s1tail := s1[1..];
        assert s1 + s2 == [s1head] + (s1tail + s2);
    }
}

method sum_method(numbers: seq<int>) returns (s : int)
    ensures s == sum(numbers[..|numbers|])
 {
    s := 0;
    for i := 0 to |numbers|
        // add invariant, stating that `s` is equal to the sum on prefix
        invariant s == sum(numbers[..i])
    {
        assert sum(numbers[..i + 1]) == sum(numbers[..i]) + numbers[i] by {
            // prove the induction step in `by {}` block
            // in an example of 3_0lemma_below_zero we invoke lemma in such block to prove assert
            // here, you should do approximately the same, but with some helpful assertions
            assert numbers[..i + 1] == numbers[..i] + [numbers[i]];
            sum_prop(numbers[..i], [numbers[i]]);
        }
        s := s + numbers[i];
    }

    return s;
}
