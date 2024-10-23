
// Implement a method MaxSegSum which finds the maximum sum of a subsequent segment [l..r] of a given array.

function Sum(a: seq<int>, s: int, t: int): int
  requires 0 <= s <= t <= |a|
{
  if s == t then 0 else Sum(a, s, t-1) + a[t-1]
}

method MaxSegSum(a: seq<int>) returns (k: int, m: int)
  ensures 0 <= k <= m <= |a|
  ensures forall p,q :: 0 <= p <= q <= |a| ==> Sum(a, p, q) <= Sum(a, k, m)
{
  k, m := 0, 0;
  var s := 0;  
  var n := 0;
  var c, t := 0, 0;  
  while n < |a|
     // add invariants
     invariant 0 <= c <= n <= |a|
     invariant 0 <= k <= m <= |a|
     invariant t == Sum(a, c, n)
     invariant s == Sum(a, k, m)
     invariant forall i :: 0 <= i <= n  ==> Sum(a, i, n) <= t
     invariant forall p, q :: 0 <= p <= q <= n ==> Sum(a, p, q) <= s
  {
    t, n := t + a[n], n + 1;
    if t < 0 {
      c, t := n, 0;
    } else if s < t {
      k, m, s := c, n, t;
    }
  }
}