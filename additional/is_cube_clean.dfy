
// Implement method is_cube which check if a number is a cube of some other number.

method cube_root(N: nat) returns (r: nat)
  ensures cube(r) <= N < cube(r + 1)
  ensures r <= N
{
  r := 0;
  while cube(r + 1) <= N
    invariant forall k :: 0 <= k <= r ==> cube(k) <= N
    invariant 0 <= r <= N
    invariant cube(N) >= N
  {
    r := r + 1;
  }
}

method is_cube(n: nat) returns (r: bool)
  ensures r ==> exists r :: 0 <= r <= n && n == cube(r)
  ensures !r ==> forall r :: 0 <= r <= n ==> n != cube(r)
{
    var root := cube_root(n);
    if cube(root) == n {
        r := true;
    } else {
        assert forall i, j :: 0 <= i < j <= n ==> cube(i) < cube(j);
        assert cube(root) < n < cube(root + 1);
        r := false;
    }
}

function cube(n: int): int { n * n * n }
