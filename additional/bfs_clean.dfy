ghost predicate is_node(graph: seq<seq<int>>, n: int)
{
    0 <= n < |graph|
}

ghost predicate is_graph(graph: seq<seq<int>>)
{
    forall i :: is_node(graph, i) ==>
        forall k :: 0 <= k < |graph[i]| ==> is_node(graph, graph[i][k])
}

ghost predicate is_graph_path(graph: seq<seq<int>>, path: seq<int>)
{
    (forall i :: 0 <= i < |path| ==> is_node(graph, path[i])) &&
    (forall i :: 0 <= i < |path| - 1 ==> path[i+1] in graph[path[i]])
}

ghost predicate path_ends_are(path: seq<int>, start: int, end: int)
{
    |path| > 0 && path[0] == start && path[|path|-1] == end
}

ghost predicate path_crosses(path: seq<int>, visited: set<int>) 
{
    exists i :: 0 <= i < |path| - 1 && path[i] in visited && path[i+1] !in visited
}

// graph - adjececny list representation of a graph
// want to check if we can reach `end` vertex `start` vertex
// we are using standard bfs algorithm

method bfs(graph : seq<seq<int>>, start : int, end : int) returns (b : bool)
    requires is_node(graph, start)
    requires is_node(graph, end)
    requires is_graph(graph) // valid graph
    ensures b ==> exists p : seq<int> :: is_graph_path(graph, p) && path_ends_are(p, start, end) 
    ensures (exists p : seq<int> :: is_graph_path(graph, p) && path_ends_are(p, start, end)) ==> b 
    decreases * // this `decreases` statement states that this method can possibly do not terminate
    // you can optionally remove this `decreases`, uncomment `decreases |q| + unvisited` in while statements
    // (where unvisited denotes number of unvisited vertices) and prove termination
{
    assert forall n :: is_node(graph, n) ==> is_graph_path(graph, [n]) && path_ends_are([n], n, n);
    if start == end {
        b := true;
        return;
    }
    b := false;
    var q := [start]; // queue
    var visited : set<int> := {start}; // set of visited nodes

    assert forall n :: is_node(graph, n) && n !in visited ==> forall p :: is_graph_path(graph, p) && path_ends_are(p, start, n) ==> path_crosses(p, visited) by {
        assert start in visited;
        assert forall n :: is_node(graph, n) && n != start ==> n !in visited;
        assert forall n :: is_node(graph, n) && n !in visited ==> n != start;
     
        assert forall n :: is_node(graph, n) && n !in visited ==> forall p :: is_graph_path(graph, p) && path_ends_are(p, start, n) ==> exists i :: 0 < i < |p| && p[i - 1] in visited && p[i] !in visited by {
            assert forall n :: is_node(graph, n) && n !in visited ==> forall p :: is_graph_path(graph, p) && path_ends_are(p, start, n) ==> p[0] == start;
            assert forall n :: is_node(graph, n) && n !in visited ==> forall p :: is_graph_path(graph, p) && path_ends_are(p, start, n) ==> p[|p| - 1] != start; 
            assert forall n :: is_node(graph, n) && n !in visited ==> forall p :: is_graph_path(graph, p) && path_ends_are(p, start, n) ==> exists i, j :: 0 <= i < j < |p| && p[i] == start && p[j] != start;
            // It obviously leads to desired statement, because if there are two indices that have different values, there is a border. But SMT solver cannot understand it and I didn't came up with the solution
        }
    }
    while |q| > 0
        invariant start in visited
        invariant end !in visited
        invariant forall i :: 0 <= i < |q| ==> is_node(graph, q[i])
        invariant forall n :: is_node(graph, n) && n !in visited ==> forall p :: is_graph_path(graph, p) && path_ends_are(p, start, n) ==> path_crosses(p, visited)
        invariant forall p : seq<int> :: is_graph_path(graph, p) && path_ends_are(p, start, end) ==> path_crosses(p, visited)
        invariant forall j :: 0 <= j < |q| ==> (exists p :: is_graph_path(graph, p) && path_ends_are(p, start, q[j]))
        invariant forall n :: n in visited ==> exists p :: is_graph_path(graph, p) && path_ends_are(p, start, n) && (forall j :: 0 <= j < |p| ==> p[j] in visited)
        invariant forall n :: n in visited ==> exists p :: is_graph_path(graph, p) && path_ends_are(p, start, n) && !path_crosses(p, visited)
        invariant forall j :: 0 <= j < |q| ==> q[j] in visited
        decreases *
    {
        var node := q[0];
        q := q[1..];
        var neighbors := graph[node];
        var i := 0;
        while i < |neighbors|
            invariant start in visited
            invariant end !in visited
            invariant 0 <= i <= |neighbors|
            invariant forall j :: 0 <= j < |q| ==> is_node(graph, q[j])
            invariant forall j :: 0 <= j < |q| ==> q[j] in visited
            invariant node in visited
            invariant forall n :: n in visited ==> exists p :: is_graph_path(graph, p) && path_ends_are(p, start, n) && (forall j :: 0 <= j < |p| ==> p[j] in visited)
            invariant forall n :: n in visited ==> exists p :: is_graph_path(graph, p) && path_ends_are(p, start, n) && !path_crosses(p, visited)
            invariant forall p : seq<int> :: is_graph_path(graph, p) && path_ends_are(p, start, end) ==> path_crosses(p, visited)
            invariant exists p :: is_graph_path(graph, p) && path_ends_are(p, start, node) && (forall j :: 0 <= j < |p| ==> p[j] in visited)
            invariant exists p :: is_graph_path(graph, p) && path_ends_are(p, start, node) && !path_crosses(p, visited)
            invariant forall n :: is_node(graph, n) && n !in visited ==> forall p :: is_graph_path(graph, p) && path_ends_are(p, start, n) ==> path_crosses(p, visited)
            invariant forall j :: 0 <= j < |graph[node]| && graph[node][j] !in visited ==> 
                       exists p :: (forall u :: 0 <= u < |p| ==> p[u] in visited) &&
                                   is_graph_path(graph, p) && 
                                   path_ends_are(p, start, node) && 
                                   !path_crosses(p, visited) &&
                                   is_graph_path(graph, p + [graph[node][j]]) && 
                                   path_ends_are(p + [graph[node][j]], start, graph[node][j]) &&
                                   path_crosses(p + [graph[node][j]], visited)
            invariant forall j :: 0 <= j < |q| ==> (exists p :: is_graph_path(graph, p) && path_ends_are(p, start, q[j]) && (forall u :: 0 <= u < |p| ==> p[u] in visited))
            invariant forall j :: 0 <= j < |q| ==> (exists p :: is_graph_path(graph, p) && path_ends_are(p, start, q[j]) && !path_crosses(p, visited))
        {
            var neighbor := neighbors[i];
            if neighbor !in visited {
                visited := visited + {neighbor};
                if neighbor == end {
                    b := true;
                    return;
                }
                q := q + [neighbor];
            }
            i := i + 1;
        }
    }
    assert forall n : int :: is_node(graph, n) && n in visited ==> (forall i : int :: 0 <= i < |graph[n]| ==> graph[n][i] in visited);
    // this assertion states that there is no visited and unvisited vertices connected by an edge
    // it helps to prove second postcondition (think how?)
    // write an invariant that will help to prove it
}