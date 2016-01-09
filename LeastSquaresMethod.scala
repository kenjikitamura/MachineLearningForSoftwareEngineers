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
    val t = new Matrix(10,1)
    /*
    t(0) = 1.0
    t(1) = 2.5
    t(2) = 3.0
    t(3) = 5.0
    t(4) = 4.0
    t(5) = 6.0
    t(6) = 9.0
    t(7) = 7.3
    t(8) = 9.0
    t(9) =10.0
     */

    t(0) = 0.0
    t(1) = 0.01
    t(2) = 0.2
    t(3) = 0.03
    t(4) = 0.01
    t(5) = 0.1
    t(6) = 0.06
    t(7) = 0.2
    t(8) = 0.60
    t(9) = 1.0

    // 正規分布誤差と正弦関数を用いてテストデータの作成
    var norm = new NormalDistribution(0,0.3);
    var l = 0
    for(i <- 0.0 to 1.0 by 0.111) {
      t(l) = Math.sin(i * Math.PI * 2) + norm.sample
      l += 1
    }
    val M = 10
    val N = t.height
    var phi = new Matrix(N, M)
    for {
      m <- 0 to N-1
      n <- 0 to M-1
    } yield {
      phi(m, n) = Math.pow(0.111111*m,n)
    }

    println(" -- phi --")
    println(phi)
    println(" -- (phiT dot phi)inv")
    val w = (phi.T dot phi).inv dot phi.T dot t

    // 最小二乗法によって得た多項式のグラフを描画するための関数
    val func = (i: DenseVector[Double]) => {
      i.map{x =>
        var y = 0.0
        (0 to M-1).foreach({ i =>
          y = y + (w(i) * Math.pow(x, i))
        })
        y
      }
    }

    // 正弦関数のグラフを描画するための関数
    val sinfunc = (i: DenseVector[Double]) => {
      i.map{x =>
        Math.sin(x * 2 * Math.PI)
      }
    }

    // 観測値ベクトル出力
    println(" -- t --")
    println(t)

    // 多項式の係数ベクトル出力
    println(" -- w --")
    println(w)

    // グラフの描画
    val f = Figure()
    val p = f.subplot(0)
    val x = linspace(0.0,1.0)
    p += plot(x, func(x))    // 多項式のグラフ
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
    f.saveas("lines.png")
  }

  /*
  def rms_error(x: Double, f: Double => Double):Double {

  }
   */
}
