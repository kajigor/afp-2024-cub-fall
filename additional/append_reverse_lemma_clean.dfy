// Prove the property of the `reverse` function stated in the lemma.

function reverse(xs: seq<nat>): seq<nat>
{
  if xs == [] then [] else reverse(xs[1..]) + [xs[0]]
}

lemma ReverseAppendDistr(xs: seq<nat>, ys: seq<nat>)
  ensures reverse(xs + ys) == reverse(ys) + reverse(xs)
  decreases |xs|
{
  if { // pattern-matching on xs
    case xs == [] => {
      assert [] + ys == ys;
    }
    case xs != [] => {
      var x := xs[0];
      var xs' := xs[1..];
      assert xs + ys == [x] + (xs' + ys);
      calc {
        reverse(ys) + (reverse(xs') + reverse([x]));
        reverse(ys) + (reverse(xs') + [x]);
        reverse(ys) + reverse([x] + xs');
        reverse(xs + ys);
      }
    }
  }
}
