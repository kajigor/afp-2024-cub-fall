// In this exercise, we will use lemmas, additional concept, helping to prove invariants.
// They remind the mathematical lemmas: they have some initial conditions and from them infer postconditions.


// We are verifying here the method below_zero, which checks if there is a prefix of the input sequence ops, 
// such that the sum of its elements is negative.

function psum(s: seq<int>): int
{
    if |s| == 0 then 0
    else psum(s[ .. |s| - 1]) + s[|s| - 1]
}

lemma docalc(x : int, y: int)
  ensures (x + y) * (x + y) == x * x + 2 * x * y + y * y
{
  calc {
    (x + y) * (x + y);
    // distributive law: (a + b) * c == a * c + b * c
    x * (x + y) + y * (x + y);
    // distributive law: a * (b + c) == a * b + a * c
    x * x + x * y + y * x + y * y;
    calc {
	    y * x;
	    x * y;
    }
    x * x + x * y + x * y + y * y;
    calc {
      x * y + x * y;
      // a = 1 * a
      1 * x * y + 1 * x * y;
      // Distributive law
      (1 + 1) * x * y;
      2 * x * y;
    }
    x * x + 2 * x * y + y * y;
  }
}

lemma psum_property(s: seq<int>, i: int)
    requires 0 <= i < |s|
    ensures psum(s[.. (i + 1)]) == psum(s[.. i]) + s[i]
{
    calc == {
        psum(s[.. (i + 1)]);
        psum(s[.. (i + 1)][.. (i + 1) - 1]) + s[.. (i + 1)][(i + 1) - 1];
        { assert s[.. (i + 1)][.. (i + 1) - 1] == s[.. i]; }
        { assert psum(s[.. (i + 1)][.. (i + 1) - 1]) == psum(s[..i]); }
        psum(s[.. i]) + s[i];
    }
}


method below_zero(ops: seq<int>) returns (res : bool)
    ensures !res ==> forall i : int :: 0 <= i <= |ops| ==> psum(ops[.. i]) >= 0
    ensures res ==> exists i : int :: 0 <= i <= |ops| && psum(ops[.. i]) < 0
{
    var balance : int := 0;
    var i : int := 0;
    while (i < |ops|)
        invariant 0 <= i <= |ops| // as always 
        invariant balance == psum(ops[.. i])
        invariant forall j : int :: 0 <= j <= i ==> psum(ops[.. j]) >= 0 
    {
        assert psum(ops[.. (i + 1)]) == psum(ops[.. i]) + ops[i] by {
            psum_property(ops, i);
        }
        balance := balance + ops[i];
        if (balance < 0) {
            assert balance == psum(ops[.. i + 1]); // added these asserts to understand why postconditions don't hold
            assert psum(ops[.. i + 1]) < 0; 
            return true;
        }
        i := i + 1;
    }

    return false;
}