package ham.matrix

import edu.emory.mathcs.csparsej.tdouble.Dcs_common.Dcs
import edu.emory.mathcs.csparsej.tdouble._

class MatrixFactory(m: Int, n: Int) {
  private val M: Dcs = Dcs_util.cs_spalloc(m, n, 1, true, true)

  private def merge(i: Int, j: Int): Long = i.toLong + (j.toLong << 32)

  def update(i: Int, j: Int, value: Double): Unit = {
    Dcs_entry.cs_entry(M, i, j, value)
  }

  def build: Matrix = {
    val CSC = Dcs_compress.cs_compress(M)
    if(CSC == null) throw new RuntimeException("Compression failed")

    val duplStatus = Dcs_dupl.cs_dupl(CSC)
    if(!duplStatus) throw new RuntimeException("Deduplication failed")
    val dropZerosResult = Dcs_dropzeros.cs_dropzeros(CSC)
    if(dropZerosResult < 0) throw new RuntimeException("Dropping zeros failed")

    new Matrix(CSC)
  }
}

class Matrix(private val M: Dcs) { lhs =>
  require(Dcs_util.CS_CSC(M))

  def n: Int = M.n
  def m: Int = M.m

  def T: Matrix = new Matrix(Dcs_transpose.cs_transpose(M, true))

  def *(rhs: Matrix): Matrix = {
    require(lhs.n == rhs.m, "Dimensions do not match for multiplication")
    new Matrix(Dcs_multiply.cs_multiply(M, rhs.M))
  }
  def +(rhs: Matrix): Matrix = {
    require(lhs.n == rhs.n && lhs.m == rhs.m)
    val res = Dcs_add.cs_add(lhs.M, rhs.M, 1.0, 1.0)
    assert(res != null)
    new Matrix(res)
  }
  def *(rhs: Array[Double]): Array[Double] = {
    assert(rhs.length == M.n)
    val res = new Array[Double](M.m)
    Dcs_gaxpy.cs_gaxpy(M, rhs, res)
    res
  }

  def min: Double = M.x.min
  def max: Double = M.x.max

  def solveCholSol(b: Array[Double]): Array[Double] = {
    val x   = b.clone()
    val res = Dcs_cholsol.cs_cholsol(1, M, x)
    if(!res)
      throw new Exception("CholSol failed")
    x
  }

  def solveQR(b: Array[Double]): Array[Double] = {
    val x   = b.clone()
    val res = Dcs_qrsol.cs_qrsol(0, M, x)
    assert(res)
    x
  }

  def *(factor: Double): Matrix = {
    val fm = new Dcs_common.Dcs
    fm.i = M.i.clone()
    fm.m = M.m
    fm.n = M.n
    fm.nz = M.nz
    fm.nzmax = M.nzmax
    fm.p = M.p.clone()
    fm.x = M.x.map(_ * factor)
    new Matrix(fm)
  }

  def norm1: Double = {
    val res = Dcs_norm.cs_norm(M)
    assert(res >= 0)
    res
  }

  def toVector: Array[Double] = {
    require(n == 1)
    val res = new Array[Double](m)
    foreachValue((i, j, v) => res(i) = v)
    res
  }

  def foreachValue(f: (Int, Int, Double) => Unit): Unit = {
    var col = 0
    while(col < n) {
      var p = M.p(col)
      while(p < M.p(col + 1)) {
        val row   = M.i(p)
        val value = if(M.x == null) 1d else M.x(p)
        f(row, col, value)
        p += 1
      }
      col += 1
    }
  }

  def print(brief: Boolean = false): Unit = Dcs_print.cs_print(M, brief)
}

object Matrix {
  def fromArray(arr: Array[Double]): Matrix = {
    val fac = new MatrixFactory(arr.length, 0)
    arr.indices.foreach(i => fac(i, 0) = arr(i))
    fac.build
  }

  def diagonal(side: Int, value: Double): Matrix = {
    val fac = new MatrixFactory(side, side)
    for(i <- 0 until side)
      fac(i, i) = value
    fac.build
  }

}
