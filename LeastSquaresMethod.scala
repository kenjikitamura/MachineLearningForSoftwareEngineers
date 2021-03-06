import breeze.linalg._
import breeze.plot._
import org.apache.commons.math3.distribution.NormalDistribution
import breeze.numerics._
/**
  * 最小二乗法
  */
object LeastSquaresMethod {
  def main(args: Array[String]):Unit = {
    //val t = Array(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0)

    // 観測値
    val t = new Matrix(10, 1)

    // 答えの関数
    val ans = new Matrix(10, 1)
    val xM = new Matrix(10, 1)
    var l = 0
    for (value <- 0.0 to 1.0 by 0.11111) {
      xM(l) = value
      l = l + 1
    }

    // 正規分布誤差と正弦関数を用いてテストデータの作成
    var norm = new NormalDistribution(0,0.3);
    l = 0
    for(i <- 0 until 10) {
      // 観測値
      t(l) = Math.sin(xM(i) * Math.PI * 2) + norm.sample

      // 答えのサインカーブ
      ans(l) = Math.sin(xM(i) * Math.PI * 2)
      l += 1
    }
    val w4 = compute_w(4, t)
    val w10 = compute_w(10, t)

    // 観測値ベクトル出力
    println(" -- t --")
    println(t)

    // 多項式の係数ベクトル出力
    println(" -- w --")
    println(w4)

    showGraph(w4, t, "最小二乗法 M=3")
    showGraph(w10, t, "最小二乗法 M=9")

    // 誤差の算出
    compute_rms_error(xM, t, ans)
  }

  def showGraph(w: Matrix, t: Matrix, name: String = ""): Unit = {
    // 正弦関数のグラフを描画するための関数
    val sinfunc = (i: DenseVector[Double]) => {
      i.map{x =>
        Math.sin(x * 2 * Math.PI)
      }
    }

    // 最小二乗法によって得た多項式のグラフを描画するための関数
    val func_result = (x: Double, w: Matrix) => {
      var y = 0.0
      (0 until w.height).foreach({ i =>
        y += (w(i) * Math.pow(x, i))
      })
      y
    }
    val func = (i: DenseVector[Double], w: Matrix) => {
      i.map{func_result(_, w)}
    }


    // グラフの描画
    val f = Figure(name)
    val p = f.subplot(0)
    val x = linspace(0.0,1.0)
    p += plot(x, func(x, w))    // 多項式のグラフ
    p += plot(x, sinfunc(x)) // 正弦関数のグラフ
    p.xlabel = "x axis"
    p.ylabel = "y axis"

    val lis = ( 0.0 to 1.0 by 0.111111111).toList;
    var i = -1
    val lis2 = lis.map{v =>
      i = i + 1
      t(i)
    }
    p += plot(lis, lis2,'.') // 観測値の点
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
}
