// Prove the property of the `reverse` function stated in the lemma.

function reverse(xs: seq<nat>): seq<nat>
{
    if xs == [] then [] else reverse(xs[1..]) + [xs[0]]
}

lemma ReverseAppendDistr(xs: seq<nat>, ys: seq<nat>)
    ensures reverse(xs + ys) == reverse(ys) + reverse(xs)
{
    if { // pattern-matching on xs
        case xs == [] => { 
            // fill in the proof here
            assert ys == [] + ys;
        }
        case xs != [] => {
            // fill in the proof here
            assert reverse((xs + ys)[1..]) == reverse(ys) + reverse(xs[1..]) by {
                assert (xs + ys)[1..] == xs[1..] + ys;
                ReverseAppendDistr(xs[1..], ys);
            }
        }
    }
}
