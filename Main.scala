import scala.collection.immutable.ArraySeq

package commands



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
      case "load_image"=> Canvas.load_image
      case _ => Canvas.default

    }

    execution(action.tail, canvas)
  }
}

case class Triangle () extends Polygon ():

  private val triangle = flawf ("Triangle")

  def addPoints (x: VectorD, y: VectorD): Unit =
    if x.dim != 3 || y.dim != 3 { triangle ("addPoints", "need exactly 3 vertices to make a triangle")}
    for i <- x.indices do addPoint (x(i).toInt, y(i).toInt)
  end addPoints
  def setFrame (tx: Double, ty: Double, w: Double, h: Double): Unit =
    val x = VectorD (0, 0, w) + tx
    val y = VectorD (0, h, h) + ty
    addPoints (x, y)
  end setFrame

end Triangle


type PolygonShapeRectangular = RectangularShape | Polygon

extension (s: PolygonShapeRectangular)
  def CenterOfXIs(): Double =
    s match
      case _: RectangularShape => s.asInstanceOf [RectangularShape].getCenterX ()
      case _: Polygon => s.asInstanceOf [Polygon].getBounds2D ().getCenterX ()
  end CenterOfXIs
  def CenterOfYIs(): Double =
    s match
      case _: RectangularShape => s.asInstanceOf [RectangularShape].getCenterY ()
      case _: Polygon => s.asInstanceOf [Polygon].getBounds2D ().getCenterY ()
  end CenterOfYIs
  def WidthIs(): Double =
    s match
      case _: RectangularShape => s.asInstanceOf [RectangularShape].getWidth ()
      case _: Polygon => s.asInstanceOf [Polygon].getBounds2D ().getWidth ()
  end WidthIs
  def HeightIs(): Double =
    s match
      case _: RectangularShape => s.asInstanceOf [RectangularShape].getHeight ()
      case _: Polygon => s.asInstanceOf [Polygon].getBounds2D ().getHeight ()
  end HeightIs
  def setFrame (x: Double, y: Double, w: Double, h: Double): Unit =
    s match
      case _: RectangularShape => s.asInstanceOf [RectangularShape].setFrame (x, y, w, h)
      case _: Polygon => s.asInstanceOf [Polygon].setFrame (x, y, w, h)
  end setFrame

/**
 * Define the status of the previous execution
 */
case class Status(
  exit: Boolean = false,
  error: Boolean = false,
  message: String = ""
)

case class DrawRectangle(x1: Int, y1: Int, x2: Int, y2: Int) extends Command

case class BucketFill(x: Int, y: Int, filler: Char) extends Command

case class DrawLine(x1: Int, y1: Int, x2: Int, y2: Int) extends Command

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
  def load_image(arguments: Seq[String], canvas: Canvas): (Canvas, Status) =
    val happy:Vector[String] =Source.fromFile("my_file").getLines().toVector
      (load_imageCanvas, Status())
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

  // TODO: Add any useful method
}
