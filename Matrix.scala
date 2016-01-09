class Matrix(val m: Int,val n: Int) {
  var matrix: Array[Array[Double]] = Array.ofDim[Double](m, n)
  def apply(m: Int): Double = matrix(m)(0)
  def apply(m: Int, n: Int): Double = matrix(m)(n)

  def update(m: Int, n: Int, value: Double): Unit = matrix(m)(n) = value
  def update(m: Int, value: Double): Unit = matrix(m)(0) = value

  override def toString(): String = {
    var ret = s"Matrix{height=${height}, width=${width}}\n"
    for (i <- 0 until m) {
      for (l <- 0 until n) 
        ret = ret + matrix(i)(l) + ","
      ret = ret + "\n"
    }
    ret
  }

  def width(): Int = n
  def height(): Int = m

  // 転置行列
  def T():Matrix = {
    val M = this
    val tM  = new Matrix(M.width, M.height)
    for {
      n <- 0 until M.width
      m <- 0 until M.height
    } tM(n,m) = M(m,n)
    tM
  }

  def dot(M: Matrix): Matrix = {
    if (this.width != M.height) throw new RuntimeException(s"縦横サイズが一致していません!  this.height=${this.height} this.width=${this.width} M.height=${M.height} M.width=${M.width}")
    val rM = new Matrix(this.height, M.width)
    //println(s"dot this=${this} height=${this.height} width=${M.width}")
    for {
      m <- 0 until rM.height
      n <- 0 until rM.width
    } {
      var sum = 0.0
      for (i <- 0 until M.height)
        sum += this(m, i) * M(i, n)
      rM(m,n) = sum
    }
    rM
  }

  def inv(): Matrix = {
    val rM = Matrix.identityMatrix(this.height)
    val M = this.copy
    /*
     まず、入力行列が正方行列かチェックする。
     正方行列であれば、同じ大きさの単位行列を作成する。

     n列目の計算
        n行目のn列目を1にするように、a倍する。単位行列のn行目もa倍する。
        n行目以外のi列を0にするように、n行目をb倍してi行に足す。単位行列のi行にも足す。
     */
    for {
      i <- 0 until M.width
    } yield {
      M.checkPivot(i, rM)
      val value = M(i,i)
      val multiplier = 1 / value
      //println(s"M($i,$i) = ${M(i,i)} value=$value multiplier=$multiplier")

      // 1を作る
      for (n <- 0 until M.width) {
        M(i, n) = M(i, n) * multiplier
        rM(i, n) = rM(i, n) * multiplier
      }

      // 1になったi行を使って、他の行のi列を0にする。
      // 1になったi行に、他の行のi列を掛けて、引けば良い。i列が負数であればマイナスも掛ける。
      for (
        l <- 0 until M.width
        if l != i
      ) {
        var tmp = M(l, i)
        for(k <- 0 until M.width) {
          M(l, k) = M(l, k) - M(i, k) * tmp
          rM(l, k) = rM(l, k) - rM(i, k) * tmp
        }
      }
      //println(s"M=${M}")
    }
    rM
  }

  def checkPivot(i: Int, resultMatrix: Matrix) {
    if (this(i,i) != 0)
      return

    val line1 = i
    var line2 = -1
    for (n <- i until height)
      if (this(n, i) != 0)
        line2 = n

    this.swapLine(line1, line2)
    resultMatrix.swapLine(line1, line2)
  }

  def swapLine(line1: Int, line2: Int) {
    println(s"Before Swap")
    println(this)
    for (n <- 0 until width) {
      val tmp = this(line1, n)
      this(line1, n) = this(line2, n)
      this(line2, n) = tmp
    }
    println(s"After Swap")
    println(this)
  }

  def copy(): Matrix = {
    val rM = new Matrix(this.height, this.width);
    for {
      m <- 0 until rM.width
      n <- 0 until rM.height
    } rM(m, n) = this(m, n)
    rM
  }
}

object Matrix {

  def identityMatrix(m: Int): Matrix = {
    val rM = new Matrix(m, m)
    for(i <- 0 until m)
      rM(i,i) = 1
    rM
  }
}
