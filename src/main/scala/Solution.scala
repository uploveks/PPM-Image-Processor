import util.Pixel
import util.Util.{getNeighbors, toGrayScale}


// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]


  /**
   *
   * Converts a string representation of a PPM image to a two-dimensional list of pixels.
   * @param image a list of characters representing the PPM image
   * @return a two-dimensional list of pixels
   */
  def fromStringPPM(image: List[Char]): Image = {
    val lines = image.mkString.split("\n").toList
    val Array(width, height) = lines(1).split(" ").map(_.toInt)
    val pixelLines = lines.drop(3).grouped(width).toList.map { line =>
      line.map { pixelLine =>
        val Array(r, g, b) = pixelLine.split(" ").map(_.toInt)
        Pixel(r, g, b)
      }
    }

    pixelLines
  }

  /**
   *
   * Converts a two-dimensional list of pixels to a string representation of a PPM image.
   * @param image a two-dimensional list of pixels
   * @return a list of characters representing the PPM image
   */
  def toStringPPM(image: Image): List[Char] = {
    // get the dimensions of the image
    val width = image.headOption.map(_.size).getOrElse(0)
    val height = image.size

    // create the PPM header
    val header = s"P3\n$width $height\n255\n"

    // create a string representation of each pixel and join them with newline characters
    val pixelStrings = image.flatten.map { pixel =>
      s"${pixel.red} ${pixel.green} ${pixel.blue}"
    }
    val pixels = pixelStrings.mkString("\n")

    // concatenate the header and the pixels and convert them to a list of characters
    (header + pixels + "\n").toList
  }

  // ex 1

  /**
   *
   * Concatenates two images vertically.
   * @param image1 the first image
   * @param image2 the second image
   * @return a new image that is the concatenation of the two input images
   */
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2
  }

  // ex 2

  /**
   *
   * Concatenates two images horizontally.
   * @param image1 the first image
   * @param image2 the second image
   * @return a new image that is the concatenation of the two input images
   */
  def horizontalConcat(image1: Image, image2: Image): Image = {
    image1.zip(image2).map { case (line1, line2) => line1 ++ line2 }
  }


  /**
   * Reverses a list of integers.
   * @param l the list to be reversed
   * @return the reversed list
   */
  def reverse(l: List[Int]): List[Int] = {
    /*
    Helper function to reverse the list using tail recursion.
    @param acc the accumulator for the reversed list
    @param l the list to be reversed
    @return the reversed list
     */
    def loop(acc: List[Int], l: List[Int]): List[Int] =
      l match {
        case Nil => acc
        case x :: xs => loop(x :: acc, xs)
      }

    loop(Nil, l)
  }

  /**
   *
   * Transposes an image.
   * @param image the image to be transposed
   * @return the transposed image
   */
  def transpose(image: Image): Image =
    image match {
      case Nil :: _ => Nil
      case _ => image.map(_.head) :: transpose(image.map(_.tail))
    }

  // ex 3

  /**
   *
   * Rotates an image by the specified number of degrees.
   * @param image   the image to be rotated
   * @param degrees the number of degrees to rotate the image by
   * @return the rotated image
   */
  def rotate(image: Image, degrees: Int): Image = {
    val numRotations = (360 - degrees) / 90
    val rotatedImage = numRotations match {
      case 1 => image.transpose.map(_.reverse)
      case 2 => image.map(_.reverse).reverse
      case 3 => image.transpose.reverse
      case _ => image // no rotation
    }
    rotatedImage
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List(1, 4, 7, 4, 1),
    List(4, 16, 26, 16, 4),
    List(7, 26, 41, 26, 7),
    List(4, 16, 26, 16, 4),
    List(1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx: GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy: GrayscaleImage = List(
    List(1, 2, 1),
    List(0, 0, 0),
    List(-1, -2, -1)
  )


  /**
   * Applies the Sobel edge detection algorithm to a grayscale image.
   * @param image the grayscale image to apply the algorithm to
   * @param threshold the theshold value for edge detection
   * @return the image after the edge detection algorithm has been applied
   */
  def edgeDetection(image: Image, threshold: Double): Image = {
    val grayScaleImage = image.map(_.map(toGrayScale))
    val gaussianBlurImage = applyConvolution(grayScaleImage, gaussianBlurKernel)
    val GxImage = applyConvolution(gaussianBlurImage, Gx)
    val GyImage = applyConvolution(gaussianBlurImage, Gy)
    val MxMyImage = GxImage.zip(GyImage).map(x => x._1.zip(x._2).map(y => Math.abs(y._1) + Math.abs(y._2)))
    val thresholdImage = MxMyImage.map(_.map(x => if (x < threshold) 0 else 255))
    thresholdImage.map(_.map(x => Pixel(x, x, x)))

  }


  /**
   * Applies a convolution operation on an image using a given kernel.
   * @param image the grayscale image to apply the convolution to
   * @param kernel the convolution kernel
   * @return the image after the convolution has been applied
   */
  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val neighbors = getNeighbors(image, kernel.length / 2)
    val result = neighbors.map(_.map(x => x.zip(kernel).map(y => y._1.zip(y._2).map(z => z._1 * z._2).sum).sum))
    result
  }

  // ex 5

  /**
   * Generates an image of size size where each pixel is determined by
   * applying the function funct to the modulo of the
   * corresponding element of Pascal's triangle with respect to m.
   * @param m The value to be used for the modulo operation
   * @param funct The function to be applied to the modulo of the elements of Pascal's triangle
   * @param size The size of the image to be generated
   * @return The generated image
   */
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    /*
    Generates the next row of Pascal's triangle given the previous row.
    @param row The previous row of Pascal's triangle.
    @return The next row of Pascal's triangle.
     */
    def nextRow(row: List[Int]): List[Int] = {
      (0 :: row).zip(row :+ 0).map { case (a, b) => (a + b) % m }
    }
    val triangleRows: LazyList[List[Int]] = LazyList.iterate(List(1))(nextRow)
    val triangle = triangleRows.take(size).toArray
    val image = Seq.tabulate(size, size) { (i, j) =>
      if (j <= i) funct(triangle(i)(j))
      else Pixel(0, 0, 0) // initialize the pixel with black color
    }.toList.map(_.toList) // convert the Seq to List of Lists

    image
  }
}
