import scala.collection.immutable.ArraySeq 
import scala.io.Source



/**
 * Main app containg program loop
 */
object Main extends App {

  println("Starting application")

  val status = run()

  println("\nExiting application")
  println(s"Final status: ${status.message}")

  /**
   * Read action from Stdin and execute it
   * Exit if action is 'exit' or if an error occured (status > 0)
   * DO NOT MODIFY THIS FUNCTION
   */
  def run(canvas: Canvas = Canvas()): Status = {
    println("\n======\nCanvas:")
    canvas.display

    print("\nAction: ")

    val action = scala.io.StdIn.readLine()

    val (newCanvas, status) = execute(ArraySeq.unsafeWrapArray(action.split(' ')), canvas)

    if (status.error) {
      println(s"ERROR: ${status.message}")
    }

    if (status.exit) {
      status
    } else {
      run(newCanvas)
    }
  }

  /**
   * Execute various actions depending on an action command and optionnaly a Canvas
   */
  def execute(action: Seq[String], canvas: Canvas): (Canvas, Status) = {
    val execution: (Seq[String], Canvas) => (Canvas, Status) = action.head match {
      case "exit" => Canvas.exit
      case "dummy" => Canvas.dummy
      case "dummy2" => Canvas.dummy2
      case "new_canvas" => Canvas.newCanvas
      // TODO: Add command here
      case "draw line" => Canvas.drawLine
      case "draw rectangle" => Canvas.DrawRectangle 
      case "load_image"=> Canvas.load_image
      case _ => Canvas.default

    }

    execution(action.tail, canvas)
  }
}

/**
 * Define the status of the previous execution
 */
case class Status(
  exit: Boolean = false,
  error: Boolean = false,
  message: String = ""
)

case class DrawRectangle(x1: Int, y1: Int, x2: Int, y2: Int) 
case class BucketFill(x: Int, y: Int, filler: Char) 

/**
 * A pixel is defined by its coordinates along with its color as a char
 */
case class Pixel(x: Int, y: Int, color: Char = ' ') {
  override def toString(): String = {
    color.toString
  }
}

/**
 * Companion object of Pixel case class
 */
object Pixel {
  /**
   * Create a Pixel from a string "x,y"
   */
  def apply(s: String): Pixel = {
    val coordinates = s.split(',')
    val x = coordinates(0).toInt
    val y = coordinates(1).toInt
    Pixel(x, y, '.')
  }

  /**
   * Create a Pixel from a string "x,y" and a color
   */
  def apply(s: String, color: Char): Pixel = {
   val coordinates = s.split(',')
    val x = coordinates(0).toInt
    val y = coordinates(1).toInt
    Pixel(x, y, color)
  }
}

/**
 * A Canvas is defined by its width and height, and a matrix of Pixel
 */
case class Canvas(width: Int = 0, height: Int = 0, pixels: Vector[Vector[Pixel]] = Vector()) {

  /**
   * Print the canvas in the console
   */
  def display: Unit = {

    if (pixels.size == 0) {
      println("Empty Canvas")
    } else {
      println(s"Size: $width x $height")
      for(y <- 0 until height) {
        for(x <- 0 until width) {
          print(pixels(y)(x))
        }
        println
      }
    }
  }

  /**
   * Takes a pixel in argument and put it in the canvas
   * in the right position with its color
   */
  def update(pixel: Pixel): Canvas = {

    val newPixels = pixels // TODO - Update pixels

    this.copy(pixels = newPixels)
  }

  /**
   * Return a Canvas containing all modifications
   */
  def updates(pixels: Seq[Pixel]): Canvas = {
    pixels.foldLeft(this)((f, p) => f.update(p))
  }

  // TODO: Add any useful method
}

/**
 * Companion object for Canvas case class
 */
object Canvas {
  /**
   * Exit execution
   */
  def exit(arguments: Seq[String], canvas: Canvas): (Canvas, Status) =
    (canvas, Status(exit = true, message="Received exit signal"))

  /**
   * Default execution for unknown action
   */
  def default(arguments: Seq[String], canvas: Canvas): (Canvas, Status) =
    (canvas, Status(error = true, message = s"Unknown command"))

  /**
   * Create a static Canvas
   */
  def dummy(arguments: Seq[String], canvas: Canvas): (Canvas, Status) =
    if (arguments.size > 0)
      (canvas, Status(error = true, message = "dummy action does not excpect arguments"))
    else  {
      val dummyCanvas = Canvas(
        width = 3,
        height = 4,
        pixels = Vector(
          Vector(Pixel(0, 0, '#'), Pixel(1, 0, '.'), Pixel(2, 0, '#')),
          Vector(Pixel(0, 1, '#'), Pixel(1, 1, '.'), Pixel(2, 1, '#')),
          Vector(Pixel(0, 2, '#'), Pixel(1, 2, '.'), Pixel(2, 2, '#')),
          Vector(Pixel(0, 3, '#'), Pixel(1, 3, '.'), Pixel(2, 3, '#'))
        )
      )

      (dummyCanvas, Status())
    }



  /**
   * Create canvas from image load
   */
  def load_image(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = {
  if (arguments.size != 1) {
    return (canvas, Status(error = true, message = "load_image action expects 1 argument"))
  }

  val fileName = arguments.head
  val fileContent = Source.fromFile(fileName).getLines().toVector

  val pixelMatrix = fileContent.zipWithIndex.map {
    case (line, y) => line.zipWithIndex.map { case (char, x) => Pixel(x, y, char) }.toVector
  }

  val updatedCanvas = Canvas(
    width = pixelMatrix.head.length,
    height = pixelMatrix.length,
    pixels = pixelMatrix
  )

  (updatedCanvas, Status(message = "Image loaded successfully"))
}



  /**
   * Create a static canvas using the Pixel companion object
   */
  def dummy2(arguments: Seq[String], canvas: Canvas): (Canvas, Status) =
    if (arguments.size > 0)
      (canvas, Status(error = true, message = "dummy action does not excpect arguments"))
    else  {
      val dummyCanvas = Canvas(
        width = 3,
        height = 1,
        pixels = Vector(
          Vector(Pixel("0,0", '#'), Pixel("1,0"), Pixel("2,0", '#')),
        )
      )

      (dummyCanvas, Status())
    }

    /**
   * Create a static Canvas
   */
  def newCanvas(arguments: Seq[String], canvas: Canvas): (Canvas, Status) =
    if (arguments.size !=3)
      (canvas, Status(error = true, message = "action newCanvas expects 3 arguments"))
    else  {
      val width = arguments(0).toInt
      val height = arguments(1).toInt
      val color = arguments(2).charAt(0)

      var pixels = (0 until height).map(y => (0 until width).map(x => Pixel(x, y, color )).toVector).toVector

      val dummyCanvas = Canvas(
        width = width,
        height = height,
        pixels = pixels
      )

      (dummyCanvas, Status())
    }

    /**Draw a line */

def drawLine(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = {
  if (arguments.length != 4) {
    return (canvas, Status(error = true, message = "Incorrect number of arguments for draw line"))
  }

  val pixel1 = Pixel(arguments(0))
  val pixel2 = Pixel(arguments(1))
  val color = arguments(2).charAt(0)

  if (pixel1.x != pixel2.x && pixel1.y != pixel2.y) {
    return (canvas, Status(error = true, message = "Invalid arguments for draw line: only vertical and horizontal lines are supported"))
  }

  val (start, end) = if (pixel1.x == pixel2.x) {
    if (pixel1.y < pixel2.y) (pixel1, pixel2) else (pixel2, pixel1)
  } else {
    if (pixel1.x < pixel2.x) (pixel1, pixel2) else (pixel2, pixel1)
  }

  val linePixels = if (start.x == end.x) {
    (start.y to end.y).map(y => Pixel(start.x, y, color))
  } else {
    (start.x to end.x).map(x => Pixel(x, start.y, color))
  }

  val newCanvas = canvas.updates(linePixels)
  (newCanvas, Status(message = "Line drawn successfully"))
}
   
       /**
   * Create a Rectangle
   */ 
   def DrawRectangle(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = {
    // parse arguments to get x1, y1, x2, y2
    if (arguments.length != 4) {
      return (canvas, Status(error = true, message = "Incorrect number of arguments for DrawRectangle"))
    }
    val x1 = arguments(0).toInt
    val y1 = arguments(1).toInt
    val x2 = arguments(2).toInt
    val y2 = arguments(3).toInt

    // validate the arguments to make sure they form a rectangle
    if (x1 >= x2 || y1 >= y2) {
      return (canvas, Status(error = true, message = "Invalid arguments for DrawRectangle"))
    }

    // draw the rectangle on the canvas
    var newCanvas = canvas
    for (x <- x1 to x2) {
      newCanvas = newCanvas.update(Pixel(x, y1, 'x'))
      newCanvas = newCanvas.update(Pixel(x, y2, 'x'))
    }
    for (y <- y1 to y2) {
      newCanvas = newCanvas.update(Pixel(x1, y, 'x'))
      newCanvas = newCanvas.update(Pixel(x2, y, 'x'))
    }

    (newCanvas, Status(message = "DrawRectangle complete"))
  }
  
   def BucketFill(action: Seq[String], canvas: Canvas): (Canvas, Status) = {
    if (action.length != 3) {
      return (canvas, Status(error = true, message = "ERROR: Invalid number of arguments for command 'BucketFill'"))
    }

    try {
      val x = action(0).toInt
      val y = action(1).toInt
      val filler = action(2)(0)

      if (x < 0 || x >= canvas.width || y < 0 || y >= canvas.height) {
        return (canvas, Status(error = true, message = "ERROR: Coordinates outside of canvas"))
      }

      def fill(x: Int, y: Int, color: Char, pixels: Vector[Vector[Pixel]]): Vector[Vector[Pixel]] = {
        if (x < 0 || x >= canvas.width || y < 0 || y >= canvas.height) {
          return pixels
        }

        val currentPixel = pixels(y)(x)

        if (currentPixel.color == color) {
          return pixels
        }

        val newPixel = currentPixel.copy(color = color)

        fill(x + 1, y, color, fill(x - 1, y, color, fill(x, y + 1, color, fill(x, y - 1, color, pixels.updated(y, pixels(y).updated(x, newPixel)))))
        )}

      val newPixels = fill(x, y, filler, canvas.pixels)
      (canvas.copy(pixels = newPixels), Status(message = "Bucket filled successfully"))

    } catch {
      case e: NumberFormatException => (canvas, Status(error = true, message = "ERROR: Invalid arguments for command 'BucketFill'"))
    }
  }
   
def update_pixel(canvas: String, pixel: (Int, Int), color: Char): (String, Status) = {
  val (x, y) = pixel
  val rows = canvas.split('\n')
  val currentRow = rows(y)
  val updatedRow = currentRow.updated(x, color)
  val updatedRows = rows.updated(y, updatedRow)
  val updatedCanvas = updatedRows.mkString("\n")
  (updatedCanvas, Status(message = "Pixel updated successfully."))
}
  def drawFill(canvas: Array[Array[Char]], position: (Int, Int), newColor: Char): (Array[Array[Char]], Status) = {
  val rows = canvas.length
  val columns = canvas(0).length
  val color = canvas(position._1)(position._2)
  val visited = Array.ofDim[Boolean](rows, columns)


  def fill(x: Int, y: Int): Unit = {
    if (x < 0 || x >= rows || y < 0 || y >= columns || canvas(x)(y) != color || visited(x)(y)) {
      return
    }
    canvas(x)(y) = newColor
    visited(x)(y) = true
    fill(x - 1, y)
    fill(x + 1, y)
    fill(x, y - 1)
    fill(x, y + 1)
  }

  fill(position._1, position._2)
  (canvas, Status(message ="OK"))
}


}  // TODO: Add any useful method
   


