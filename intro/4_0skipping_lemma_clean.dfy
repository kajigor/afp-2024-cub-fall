// In previous examples of lemmas we used only equations and asserts.
// However, lemmas allow more broad functionality. 
// For example, we can iterate over the loop to prove postconditions of lemma (like in methods). 

// What makes this problem interesting is that the
// array we are searching in has two special properties: all elements are nonnegative, 
// and each successive element decreases by at most one from the
// previous element.

// a[i] >= 0
// a[i - 1] - 1 <= a[i] ~~  a[i] - d <= a[i + d]
// a[i] != 0, forall j in [i, i + a[i]) -> a[j] != 0 we can derive this property from the initial condition and use it for search
// by moving index this way `index := index + a[index];` we don't skip any 0's and speedup the search
// let's prove our search is correct


// when using only assertions we can prove `forall` postcondition only for limited range 
lemma SkippingLemma0(a: array<int>, j: int) 
  requires forall i :: 0 <= i < a.Length ==> 0 <= a[i]
  requires forall i :: 0 < i < a.Length ==> a[i - 1] - 1 <= a[i]
  requires 0 <= j < a.Length - 3
{
  assert a[j] - 1 <= a[j + 1];
  assert a[j + 1] - 1 <= a[j + 2];
  assert a[j + 2] - 1 <= a[j + 3];
  // therefore:
  assert a[j] - 3 <= a[j + 3];
}

// however, we can use `while` loops as in usual methods
// in lemmas we can also use `if`, `match` and other constructs 
lemma SkippingLemma(a: array<int>, j: int) //  {:axiom} 
  requires forall i :: 0 <= i < a.Length ==> 0 <= a[i]
  requires forall i :: 0 < i < a.Length ==> a[i - 1] - 1 <= a[i]
  requires 0 <= j < a.Length
  ensures forall i :: j <= i < j + a[j] && i < a.Length ==> a[i] != 0
{
  // here, iterating over the loop we can inductively prove the postcondition
  var i := j;
  if a[i] == 0 { return; }
  i := i + 1;
  while i < j + a[j] && i < a.Length
    // add here invariants to prove post-conditions
    invariant j <= i <= j + a[j]
    invariant i <= a.Length
    invariant j + a[j] <= i - 1 + a[i - 1]
    invariant forall k :: j <= k < i ==> a[k] != 0
  {
    i := i + 1;
  }
}


method FindZero(a: array<int>) returns (index: int)
  requires forall i :: 0 <= i < a.Length ==> 0 <= a[i]
  requires forall i :: 0 < i < a.Length ==> a[i - 1] - 1 <= a[i]
  requires exists i :: 0 <= i < a.Length && a[i] == 0
  ensures 0 <= index && index < a.Length && a[index] == 0
{
  index := 0;
  while index < a.Length
    // add invariants
    invariant forall i :: 0 <= i < index && i < a.Length ==> a[i] != 0 
  {
    if a[index] == 0 { return; }
    SkippingLemma(a, index);
    index := index + a[index];
  }
  index := -1;
}
