package Life

import Panda.Engine

import javax.swing.{JPanel}
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.Graphics
import java.awt.Point
import scala.collection.mutable.{Map, ListBuffer}


class Game extends Engine
{
	val title: String = "CONWAY'S GAME OF LIFE"

	val windowSize: Dimension = new Dimension(1000, 700)
	val cellSize: Int = 10

	val framerate: Int = 24
	var gamePanel: JPanel = null

	var speed: Double = 2 // iterations / second
	private var currentTime: Double = 0

	var isPlaying: Boolean = false

	val colors: Map[String, Color] = Map(
		"playing-grid" 	-> new Color(70, 70, 70),
		"grid" 			-> new Color(200, 200, 200),
		"playing-bg" 	-> Color.darkGray,
		"bg"			-> Color.white,
		"cell"			-> Color.pink
	)

	class Cell(val x: Int = -1,
			   val y: Int = -1,
			   val neighbours: ListBuffer[(Int, Int)] = ListBuffer.empty[(Int, Int)],
			   var enabled: Boolean = false)
	{
		def copy: Cell =
		{
			val cell: Cell = new Cell(x, y, neighbours, enabled)
			return cell
		}

		def getAbsolutePos: Point = new Point(cellSize * x, cellSize * y)
	}
	object Cell
	{
		def getCell(x: Int, y: Int): Cell =
		{
			val relX = ((x + offsetFix.x) / cellSize).toInt
			val relY = ((y + offsetFix.y) / cellSize).toInt
			return cells(relX)(relY)
		}
	}
	var cells: Array[Array[Cell]] = null


	override def start: Unit = 
	{
		gamePanel = new GamePanel

		super.start

		createCells
	}

	def createCells: Unit =
	{
		val dims: Dimension = frame.getSize()
		val countX: Int = (dims.width / cellSize).toInt
		val countY: Int = (dims.height / cellSize).toInt
		cells = new Array[Array[Cell]](countX)
		for (x <- 0 to countX - 1)
			cells(x) = new Array[Cell](countY)
			for (y <- 0 to countY - 1)
				cells(x)(y) = new Cell(x, y)
		
		// setting neighbours
		for (x <- 0 to countX - 1)
			for (y <- 0 to countY - 1)
				if (y > 0)
					cells(x)(y).neighbours += ((x, y - 1))			//  ^
					if (x > 0)
						cells(x)(y).neighbours += ((x - 1, y - 1)) 	// <^
				if (y < countY - 1)
					cells(x)(y).neighbours += ((x, y + 1)) 			//  v
					if (x < countX - 1)
						cells(x)(y).neighbours += ((x + 1, y + 1)) 	// >v
				if (x > 0)
					cells(x)(y).neighbours += ((x - 1, y))			// <
					if (y < countY - 1)
						cells(x)(y).neighbours += ((x - 1, y + 1))	// <v
				if (x < countX - 1)
					cells(x)(y).neighbours += ((x + 1, y))			// >
					if (y > 0)
						cells(x)(y).neighbours += ((x + 1, y - 1))	// >^
	}

	override def run(delta: Double): Unit =
	{
		super.run(delta)

		if (isPlaying)
		{
			if (currentTime > 0)
				currentTime -= delta / 1000d
				return
			currentTime = 1.0d / speed

			val newCells: Array[Array[Cell]] = new Array[Array[Cell]](cells.size)
			for (i: Int <- 0 to cells.size - 1)
			{
				newCells(i) = new Array[Cell](cells(i).size)

				for (j: Int <- 0 to cells(i).size - 1)
				{
					val cell: Cell = cells(i)(j).copy

					val n: Int = getActiveNeighbourCount(cell)

					if (cell.enabled)
					{
						if (n <= 1)
							cell.enabled = false
						else if (n >= 4)
							cell.enabled = false
					}
					else
					{
						if (n == 3)
							cell.enabled = true
					}
					newCells(i)(j) = cell
				} 
			}
			cells = newCells
		}
	}

	def getActiveNeighbourCount(cell: Cell): Int =
	{
		var n: Int = 0
		for ((x: Int, y: Int) <- cell.neighbours)
		{
			if (cells(x)(y).enabled)
				n += 1
		}
		return n
	}

	override def keyDown(key: Int): Unit = 
	{
		super.keyDown(key)

		key match
			case 'P' => isPlaying = !isPlaying
			case '.' => speed += 1
			case ',' => speed = math.max(speed - 1, 1)
			case default => ;
	}

	def keyUp(key: Int): Unit = 
	{
		key match
			case default => ;
	}

	def keyKB(key: Int): Unit =
	{
		
	}

	def onClick(x: Int, y: Int): Unit =
	{
		val cell: Cell = Cell.getCell(x, y)
		cell.enabled = !cell.enabled
	}


	class GamePanel extends JPanel
	{
		override def paint(g: Graphics): Unit =
		{
			super.paint(g)
			
			if (isPlaying)
				g.setColor(colors("playing-bg"))
				g.fillRect(0, 0, frame.getWidth(), frame.getHeight())
			else
				g.setColor(colors("bg"))
				g.fillRect(0, 0, frame.getWidth(), frame.getHeight())

			drawGrid(g)
		}

		def drawGrid(g: Graphics): Unit =
		{
			for (column: Array[Cell] <- cells)
			{
				val absoluteX: Int = column(0).getAbsolutePos.x
				g.setColor(if isPlaying then colors("playing-grid") else colors("grid"))
				g.drawLine(absoluteX, 0, absoluteX, frame.getHeight())

				for (cell: Cell <- column)
				{
					if (cell.enabled)
						g.setColor(colors("cell"))
						g.fillRect(cell.getAbsolutePos.x + 1, cell.getAbsolutePos.y + 1, cellSize - 1, cellSize - 1)
				}
			}
			for (row: Cell <- cells(0))
			{
				val absoluteY: Int = row.getAbsolutePos.y
				g.setColor(if isPlaying then colors("playing-grid") else colors("grid"))
				g.drawLine(0, absoluteY, frame.getWidth(), absoluteY)
			}
		}
	}
}