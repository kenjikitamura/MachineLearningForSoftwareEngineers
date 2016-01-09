/**
  * Matrix Test
  */
object MatrixTest {
  def main(args: Array[String]):Unit = {
    var m3 = new Matrix(4,4)
    m3(0,0) = 1
    m3(0,1) = 1
    m3(0,2) = 1
    m3(0,3) = -1

    m3(1,0) = 1
    m3(1,1) = 1
    m3(1,2) = -1
    m3(1,3) = 1

    m3(2,0) = 1
    m3(2,1) = -1
    m3(2,2) = 1
    m3(2,3) = 1

    m3(3,0) = -1
    m3(3,1) = 1
    m3(3,2) = 1
    m3(3,3) = 1

    println("Input Matrix")
    println(m3)

    println("Inv Matrix")
    println(m3.inv)


    var m4 = new Matrix(4,4)
    m4(0,0) = 0
    m4(0,1) = 0
    m4(0,2) = 0
    m4(0,3) = 1

    m4(1,0) = 0
    m4(1,1) = 0
    m4(1,2) = 1
    m4(1,3) = 0

    m4(2,0) = 0
    m4(2,1) = 1
    m4(2,2) = 0
    m4(2,3) = 0

    m4(3,0) = 1
    m4(3,1) = 0
    m4(3,2) = 0
    m4(3,3) = 0

    println("Input Matrix")
    println(m4)

    println("Inv Matrix")
    println(m4.inv)

    var m5 = new Matrix(2,2)
    m5(0,0) = 0
    m5(0,1) = 1
    m5(1,0) = 1
    m5(1,1) = 0

    println("Input Matrix")
    println(m5)

    println("Inv Matrix")
    println(m5.inv)

    var m6 = new Matrix(2,2)
    m6(0,0) = 1
    m6(0,1) = -1
    m6(1,0) = -2
    m6(1,1) = 3

    var m7 = new Matrix(2,2)
    m7(0,0) = 1
    m7(0,1) = 2
    m7(1,0) = 3
    m7(1,1) = 4

    println(m6 dot m7)
  }
}

/*
Input Matrix
Matrix{height=4, width=4}
0.0,0.0,0.0,1.0, 1 0 0 0
0.0,0.0,1.0,0.0, 0 1 0 0
0.0,1.0,0.0,0.0, 0 0 1 0
1.0,0.0,0.0,0.0, 0 0 0 1

1.0,0.0,0.0,0.0, 0 0 0 1
0.0,1.0,0.0,0.0, 0 0 1 0
0.0,0.0,1.0,0.0, 0 1 0 0
0.0,0.0,0.0,1.0, 1 0 0 0
 */
