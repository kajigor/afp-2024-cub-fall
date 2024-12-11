module MatrixModule {
  class Matrix {
	var rows: int
	var cols: int
	var data: array2<int>

	predicate Valid()
	  reads this
	{
	  rows > 0 && cols > 0 && data.Length0 == rows && data.Length1 == cols
	}

	constructor (r: int, c: int, values: array2<int>) 
	  requires r > 0 && c > 0 && values.Length0 == r && values.Length1 == c
	  ensures rows == r && cols == c && data == values
	{
	  rows := r;
	  cols := c;
	  data := values;
	}

	method Add(other: Matrix) returns (result: Matrix)
	  requires Valid() && other.Valid()
	  requires rows == other.rows && cols == other.cols
	  ensures result.rows == rows && result.cols == cols
	  ensures result.Valid()
	  ensures forall i, j :: 0 <= i < rows && 0 <= j < cols ==> result.data[i, j] == data[i, j] + other.data[i, j]
	{
	  var resultData: array2<int> := new int[rows, cols];
	  for i := 0 to rows
		  invariant 0 <= i <= rows
		  invariant forall ii, jj :: 0 <= ii < i && 0 <= jj < cols ==> resultData[ii, jj] == data[ii, jj] + other.data[ii, jj]
	  {
		  for j := 0 to cols
		    invariant 0 <= j <= cols
		    invariant forall ii, jj :: 0 <= ii < i && 0 <= jj < cols ==> resultData[ii, jj] == data[ii, jj] + other.data[ii, jj]
		    invariant forall jj :: 0 <= jj < j ==> resultData[i, jj] == data[i, jj] + other.data[i, jj]
		  {
		    resultData[i, j] := data[i, j] + other.data[i, j];
		  }
	  }
	  result := new Matrix(rows, cols, resultData);
	}

	method MultiplyByConst(k: int) returns (result: Matrix)
	  requires Valid()
	  ensures result.rows == rows && result.cols == cols
	  ensures result.Valid()
	  ensures forall i, j :: 0 <= i < rows && 0 <= j < cols ==> result.data[i, j] == k * data[i, j]
	{
	  var resultData: array2<int> := new int[rows, cols];
	  for i := 0 to rows
		  invariant 0 <= i <= rows
		  invariant forall ii, jj :: 0 <= ii < i && 0 <= jj < cols ==> resultData[ii, jj] == k * data[ii, jj]
	  {
		  for j := 0 to cols
		    invariant 0 <= j <= cols
		    invariant forall ii, jj :: 0 <= ii < i && 0 <= jj < cols ==> resultData[ii, jj] == k * data[ii, jj]
		    invariant forall jj :: 0 <= jj < j ==> resultData[i, jj] == k * data[i, jj]
	 	  {
		    resultData[i, j] := k * data[i, j];
		  }
	  }
	  result := new Matrix(rows, cols, resultData);
	}

	method Transpose() returns (result: Matrix)
	  requires Valid()
	  ensures result.rows == cols && result.cols == rows
	  ensures result.Valid()
	  ensures forall i, j :: 0 <= i < rows && 0 <= j < cols ==> data[i, j] == result.data[j, i]
	{
	  var resultData: array2<int> := new int[cols, rows];
	  for i := 0 to rows 
		  invariant 0 <= i <= rows
		  invariant forall ii, jj :: 0 <= ii < i && 0 <= jj < cols ==> resultData[jj, ii] == data[ii, jj]
	  {
		  for j := 0 to cols
		    invariant 0 <= j <= cols
		    invariant forall ii, jj :: 0 <= ii < i && 0 <= jj < cols ==> resultData[jj, ii] == data[ii, jj]
		    invariant forall jj :: 0 <= jj < j ==> resultData[jj, i] == data[i, jj]
		  {
		    resultData[j, i] := data[i, j];
		  }
	  }
	  result := new Matrix(cols, rows, resultData);
	}

	function scalarProduct(i: int, a: array2<int>, j: int, b: array2<int>, k: int, acc: int) : int 
	  reads a, b
	  requires k < 0 || (0 <= i < a.Length0 && 0 <= k < a.Length1 && 0 <= k < b.Length0 && 0 <= j < b.Length1)
	  ensures k < 0 ==> scalarProduct(i, a, j, b, k, acc) == acc
	  ensures k < 0 || (0 <= i < a.Length0 && 0 <= k < a.Length1 && 0 <= k < b.Length0 && 0 <= j < b.Length1)
	  ensures k >= 0 ==> scalarProduct(i, a, j, b, k, acc) == a[i, k] * b[k, j] + scalarProduct(i, a, j, b, k - 1, acc)
	  decreases k
	{
		if k < 0 then acc
		else a[i, k] * b[k, j] + scalarProduct(i, a, j, b, k - 1, acc)
	}
	
	method Multiply(other: Matrix) returns (result: Matrix)
	  requires Valid() && other.Valid()
	  requires cols == other.rows
	  ensures result.rows == rows && result.cols == other.cols
	  ensures result.Valid()
	  ensures forall i, j :: 0 <= i < rows && 0 <= j < other.cols ==> result.data[i, j] == scalarProduct(i, data, j, other.data, cols - 1, 0)
	{
	  var resultData: array2<int> := new int[rows, other.cols];

	  for i := 0 to rows
		  invariant 0 <= i <= rows
		  invariant forall ii, jj :: 0 <= ii < i && 0 <= jj < other.cols ==> resultData[ii, jj] == scalarProduct(ii, data, jj, other.data, cols - 1, 0)
	  {
		  for j := 0 to other.cols 
		    invariant 0 <= j <= other.cols
		    invariant forall ii, jj :: 0 <= ii < i && 0 <= jj < other.cols ==> resultData[ii, jj] == scalarProduct(ii, data, jj, other.data, cols - 1, 0)
		    invariant forall jj :: 0 <= jj < j ==> resultData[i, jj] == scalarProduct(i, data, jj, other.data, cols - 1, 0)
		  {
		    resultData[i, j] := scalarProduct(i, data, j, other.data, cols - 1, 0);
		  }
	  }
	  result := new Matrix(rows, other.cols, resultData);
	}
}

	method Main() {
	  var data1 := new int[2, 2];
	  data1[0, 0], data1[0, 1], data1[1, 0], data1[1, 1] := 1, 2, 3, 4;

	  var data2 := new int[2, 2];
	  data2[0, 0], data2[0, 1], data2[1, 0], data2[1, 1] := 5, 6, 7, 8;

	  var matrix1 := new Matrix(2, 2, data1);
	  var matrix2 := new Matrix(2, 2, data2);

	  var sumMatrix := matrix1.Add(matrix2);
	  var productMatrix := matrix1.Multiply(matrix2);
	  var transposeMatrix := matrix1.Transpose();
	  var kMatrix := matrix1.MultiplyByConst(5);
  }
}
