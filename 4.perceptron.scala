import breeze.linalg._
import breeze.plot._
import org.apache.commons.math3.distribution.NormalDistribution
import breeze.numerics._
/**
  * パーセプトロン
  */
object Perceptron {
  val bias = 1

  def main(args: Array[String]):Unit = {

    // 観測値
    val N = 5
    val tx = new Matrix(N, 1)
    val ty = new Matrix(N, 1)
    val tType = new Matrix(N, 1)

    // t1(-8, 2) = -1
    tx(0) = -8
    ty(0) = 2
    tType(0) = -1

    // t2(-5, 5) = -1
    tx(1) = -5
    ty(1) = 5
    tType(1) = -1

    // t3(5, 5) = 1
    tx(2) = 5
    ty(2) = 5
    tType(2) = 1

    // t4(4, 14)
    tx(3) = 4
    ty(3) = 14
    tType(3) = 1

    // t5(4, 14)
    tx(4) = -10
    ty(4) = -35
    tType(4) = 1

    val w = new Matrix(3, 1)
    w(0) = 0
    w(1) = 0
    w(2) = 0



    val func = (x: Double, y: Double) => {
      w(0) + x * w(1) + y * w(2)
    }

    for (loop <- 0 until 30) {
      for (i <- 0 until N) {
        val x = tx(i)
        val y = ty(i)
        val value = tType(i)
        if (value * (w(0) * bias + w(1) * x + w(2) * y) <= 0) {
          w(0) += value * bias
          w(1) += value * x
          w(2) += value * y
        }
      }
    }

    println(s"w=$w")

    //compute_rms_error(xM, t, ans)
    showGraph(w, null, tx, ty, tType)
  }

  def showGraph(w: Matrix, t: Matrix, tx: Matrix, ty: Matrix, tType: Matrix, name: String = ""): Unit = {
    // グラフの描画
    val f = Figure(name)
    val p = f.subplot(0)
    val x = linspace(-10,10)
    val func = (i: DenseVector[Double], w: Matrix) => {
      i.map{ - _ * w(1) / w(2) - bias * w(0) / w(2) }
    }

    p += plot(x, func(x, w))    // 多項式のグラフ
    p.xlabel = "x axis"
    p.ylabel = "y axis"

    val xList = tx.toList
    val yList = ty.toList
    val typeList = tType.toList

    var list1x = List.empty[Double]
    var list1y = List.empty[Double]

    var list2x = List.empty[Double]
    var list2y = List.empty[Double]

    var count = 0
    for (value <- typeList) {
      if (value >= 0) {
        list1x = xList(count) :: list1x
        list1y = yList(count) :: list1y
      } else {
        list2x = xList(count) :: list2x
        list2y = yList(count) :: list2y
      }
      count += 1
    }

    p += plot(list1x, list1y,'.') // 観測値の点
    p += plot(list2x, list2y,'+') // 観測値の点

  }

  /**
    * wの計算
    */
  def compute_w(M: Int, t: Matrix): Matrix = {
    val N = t.height
    var phi = new Matrix(N, M)
    for {
      m <- 0 until N
      n <- 0 until M
    } phi(m, n) = Math.pow(0.111111*m,n)

    println(" -- phi --")
    println(phi)
    println(" -- (phiT dot phi)inv")
    val w = (phi.T dot phi).inv dot phi.T dot t
    w
  }

  /**
    * Mを変えて最小二乗誤差を計算
    */
  def compute_rms_error(xM: Matrix, t: Matrix, ans:Matrix) {
    val errorM = new Matrix(10, 1)
    val anserrorM = new Matrix(10, 1)
    for (M <- 1 to 10) {
      val w = compute_w(M, t)
      val func_result = (x: Double) => {
        var y = 0.0
          (0 until M).foreach({ i =>
            y += (w(i) * Math.pow(x, i))
          })
        y
      }
      val error = rms_error(func_result, xM, t)
      val ans_error = rms_error(func_result, xM, ans)
      errorM(M-1) = error
      anserrorM(M-1) = ans_error
    }

    for (i <- 0 to 9)
      println(s"i=$i rms_error=${errorM(i)} ans_error=${anserrorM(i)}")

    val f = Figure()
    val p = f.subplot(0)

    val lis = ( 0.0 to 1.0 by 0.111111111).toList;
    var i = -1
    val lis2 = lis.map{v =>
      i = i + 1
      errorM(i)
    }
    i = -1
    val lis3 = lis.map{v =>
      i = i + 1
      anserrorM(i)
    }
    p += plot(lis, lis2,'.') // 観測値の点
    p += plot(lis, lis3,'.') // 観測値の点
  }

  /** 平方根平均二乗誤差を計算
    * f: Double => Double 多項式の関数
    * xM: Matrix テストデータのx値のベクトル
    * yM: Matrix テストデータのy値のベクトル
    */
  def rms_error(f: Double => Double, xM: Matrix, yM: Matrix):Double = {
    var sum = 0d
    for (i <- 0 until xM.height) {
      val diff = f(xM(i)) - yM(i)
      //println(s"diff=$diff")
      sum += diff * diff
    }
    sum / xM.height
  }

  // テストデータを作ろうとしたけどめんどくさいのでやめる
  /*
  def makeData(fromX: Double, fromY: Double, toX: Double, toY: Double, size: Int, value: Int):(Matrix, Matrix, Matrix) = {
    val x = new Matrix(size * size,1)
    val y = new Matrix(size * size,1)
    val t = new Matrix(size * size,1)
    var count = 0
    for (i <- fromX to toX by (fromX - toX) / size) {
      for (l <- fromY to toY by (fromY - toY) / size) {
        x(count) = i
      }
    }
  }
   */
}
