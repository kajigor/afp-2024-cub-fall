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
    if (|s| > 1){
        calc == {
            sum(s);
            s[0] + sum(s[1..]);
            {assert  s[1..] == s[1..|s|];}
            assert s[..|s| - 1][1..] == s[1..][..|s| - 2];
            sum(s[..|s| - 1]) + s[ |s| - 1 ];
        }
    }
}

method sum_method(numbers: seq<int>) returns (s : int)
    ensures s == sum(numbers[..|numbers|])
 {
    s := 0;
    for i := 0 to |numbers|
        invariant 0 <= i <= |numbers| 
        invariant s == sum(numbers[.. i])   
    {
        // here, you can write auxiliary assertions about the sum and product
        assert s == sum(numbers[.. i]);
        sum_prop(numbers[.. i + 1]);
        s := s + numbers[i];
        sum_prop(numbers[.. i + 1]);
        assert numbers[.. (i + 1)][..i] == numbers[.. i];
        assert sum(numbers[.. i + 1]) == sum(numbers[.. i]) + numbers[i];
    
    }

    return s;
}
