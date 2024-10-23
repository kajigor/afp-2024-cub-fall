// Here, we changed the order of function `sum`. Still we want to prove the equality between `sum` and `sum_method`.
// Fill in the invariants, prove the lemma and use them inside the method.


// not the difference between previous and current implementations of `sum`
function sum(s: seq<int>) : int {
    if |s| == 0 then 0 else s[0] + sum(s[1..])
}

lemma sum_prop(s: seq<int>)
    requires |s| > 0
    ensures sum(s) == sum(s[..|s| - 1]) + s[ |s| - 1 ]
{
    if (|s| > 1) {
        // add an assetion to prove lemma
        assert s[..|s| - 1][1..] == s[1..][..|s| - 2];
    }
}

method sum_method(numbers: seq<int>) returns (s : int)
    ensures s == sum(numbers[..|numbers|])
 {
    s := 0;
    for i := 0 to |numbers|
        // add invariant, stating that `s` is equal to the sum on prefix
        invariant 0 <= i <= |numbers|
        invariant s == sum(numbers[..i])
    {
        assert sum(numbers[..i + 1]) == sum(numbers[..i]) + numbers[i] by {
            // prove the induction step in `by {}` block
            // in an example of 3_0lemma_below_zero we invoke lemma in such block to prove assert
            // here, you should do approximately the same, but with some helpful assertions
            sum_prop(numbers[..i + 1]);
            assert numbers[..i + 1][..i] == numbers[..i];
        }
        s := s + numbers[i];
    }

    return s;
}
