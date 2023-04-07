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


  def reverse(l: List[Int]): List[Int] = {
    def loop(acc: List[Int], l: List[Int]): List[Int] =
      l match {
        case Nil => acc
        case x :: xs => loop(x :: acc, xs)
      }

    loop(Nil, l)
  }

  def transpose(image: Image): Image =
    image match {
      case Nil :: _ => Nil
      case _ => image.map(_.head) :: transpose(image.map(_.tail))
    }

  // ex 3
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


  //please complete the edge detection function and apply convolution function

  def edgeDetection(image: Image, threshold: Double): Image = {
    //applu grayscale on fucntion
    val grayScaleImage = image.map(_.map(toGrayScale))
    //apply gaussian blur on convolution
    val gaussianBlurImage = applyConvolution(grayScaleImage, gaussianBlurKernel)
    //apply Gx and Gy on convolution
    val GxImage = applyConvolution(gaussianBlurImage, Gx)
    val GyImage = applyConvolution(gaussianBlurImage, Gy)
    //Se vor combina Mx și My, adunându-se (element cu element) fiecare pixel în modul (astfel, obtinem o aproximare destul de buna a schimbarii de intensitate in jurul fiecarui pixel)
    //use this formula to get MxMy Sobel filtering involves applying two 3 x 3 convolutional kernels (also called filters) to an image. The kernels are usually called Gx and Gy, and they are shown in the following figure. These two kernels detect the edges in the image in the horizontal and vertical directions. They are applied separately and then combined to produce a pixel value in the output image at each position in the input image.
    //
    //The output value is approximated by:
    //
    //G = |Gx| + |Gy|
    //where Gx and Gy are the values of the two kernels at the same position in the image.
    //write the code to get MxMy
    val MxMyImage = GxImage.zip(GyImage).map(x => x._1.zip(x._2).map(y => Math.abs(y._1) + Math.abs(y._2)))
    //A threshold T will be set, so that every pixel with a value lower than T will be black (rgb 0 0 0) and every pixel above T will be white (rgb 255 255 255)
    val thresholdImage = MxMyImage.map(_.map(x => if (x < threshold) 0 else 255))
    //write the final result
    thresholdImage.map(_.map(x => Pixel(x, x, x)))

  }


  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    //In order to perform convolution on an image, following steps should be taken.
    //
    //Flip the mask (horizontally and vertically) only once
    //Slide the mask onto the image.
    //Multiply the corresponding elements and then add them
    //Repeat this procedure until all values of the image has been calculated.
    //write the code to perform convolution
    //hint: use getNeighbors function
    val neighbors = getNeighbors(image, kernel.length / 2)
    val result = neighbors.map(_.map(x => x.zip(kernel).map(y => y._1.zip(y._2).map(z => z._1 * z._2).sum).sum))
    result
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    // create a function that generates the next row of the Pascal triangle
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
