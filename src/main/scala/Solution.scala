import util.{Pixel, Util}

import scala.annotation.tailrec
import scala.collection.immutable.Nil

// Online viewer: https://0xc0de.fr/webppm/

object Solution {
  type Sir = List[Char]
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image =
    def split(s: Sir): List[Sir] = {
      def op(c: Char, acc: List[Sir]): List[Sir] =
        acc match {
          case Nil => if (c == ' ' || c == '\n') Nil else List(List(c))
          case x :: xs => if (c == ' ' || c == '\n') Nil :: acc else (c :: x) :: xs
        }

      s.foldRight(Nil: List[Sir])(op)
    }
    val width = image.dropWhile(_ != '\n')
      .tail
      .takeWhile(_.isDigit).mkString.toInt
    val height = image.dropWhile(_ != '\n')
      .tail.dropWhile(_ != ' ')
      .tail.takeWhile(_.isDigit).mkString.toInt
    val pixels = image.dropWhile(_ != '\n')
      .tail.dropWhile(_ != '\n')
      .tail.dropWhile(_ != '\n')

    val res = split(pixels).tail
      .map(_.mkString.toInt)
      .grouped(3).toList
      .map(x => Pixel(x(0), x(1), x(2)))
      .grouped(width).toList
    res



  def toStringPPM(image: Image): List[Char] =
    val width = image.head.length.toString + " "
    val height = image.length.toString + "\n"

    val fromPixelToString =
      image.flatten.map(pixel => s"${pixel.red} ${pixel.green} ${pixel.blue}\n")

    val completeImg = "P3\n" :: width:: height :: "255\n" :: fromPixelToString

    val listChar = completeImg.flatten
//    print(listChar)
    listChar
//    val reconvertedImg: List[Char] =


  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image =
    image1 ++ image2

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image =
    image1.zip(image2).map(x => x._1 ++ x._2)

  // ex 3
  def rotate(image: Image, degrees: Integer): Image =
    def transpose(img: Image): Image=
      img match {
        case Nil :: _ => Nil
        case _ => img.map(_.head) :: transpose(img.map(_.tail))
      }

    @tailrec
    def transposeNtimes(img: Image, times: Int): Image=
      if times == 0 then img
      else transposeNtimes(transpose(img).reverse, times - 1)

    transposeNtimes(image, degrees / 90)

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def edgeDetection(image: Image, threshold : Double): Image = {
    val grayscaledImg = image.map(_.map(Util.toGrayScale))
    val blurredImg = applyConvolution(grayscaledImg, gaussianBlurKernel)
    val Mx = applyConvolution(blurredImg, Gx)
    val My = applyConvolution(blurredImg, Gy)
    val imgWidth = Mx.head.size
    val Mcombined = Mx.flatten.zip(My.flatten)
                    .map(x => Math.abs(x._1) + Math.abs(x._2))
    val finalImg = Mcombined.map(elem => {
      if (elem < threshold) Pixel(0, 0, 0)
      else Pixel(255, 255, 255)
    }).grouped(imgWidth).toList
    finalImg
  }

//  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage =
//    def generateElem(img: GrayscaleImage): Double =
//      img.take(kernel.size) //luam doar primele randuri
//        .flatMap(_.take(kernel.size)) //luam doar primele coloane
//        .zip(kernel.flatten).map(_ * _).sum
//
//    def generateLine(img: GrayscaleImage): List[Double] =
//      if img.head.size == kernel.size - 1 then Nil
//      else generateElem(img) :: generateLine(img.map(_.tail))
//
//    if image.size == kernel.size - 1 then Nil
//    else generateLine(image) :: applyConvolution(image.drop(1), kernel)

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage =
    def generateElem(img: GrayscaleImage, kernel: GrayscaleImage): Double =
      img.take(kernel.size) //luam doar primele randuri
        .flatMap(_.take(kernel.size)) //luam doar primele coloane
        .zip(kernel.flatten).map(_ * _).sum

    Util.getNeighbors(image, kernel.length / 2).map(_.map(generateElem(_, kernel)))

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image =
    @tailrec
    def nextRow(current: List[Int], newLine: List[Int]): List[Int] =
      current match {
        case _ :: Nil => newLine.reverse ++ List.fill(size - newLine.size)(-1)
        case -1 :: _ => newLine.reverse ++ List.fill(size - newLine.size)(-1)
        case 1 :: -1 :: _ => nextRow(current.tail, 1 ::newLine)
        case _ => nextRow(current.tail, ((current.head + current.tail.head) % m) ::newLine)
      }
    def genTriangle(curr: List[Int], rem: Int): List[List[Int]] =
      if rem == 0 then Nil
      else curr :: genTriangle(nextRow(curr, List(1)), rem - 1)

    val firstRow = 1 :: List.fill(size - 1)(-1)


    genTriangle(firstRow, size).map(_.map(x => if x == -1  then Pixel(0, 0, 0) else funct(x)))



}
