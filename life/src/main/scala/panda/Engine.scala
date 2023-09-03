package Panda

import java.util.Timer
import java.util.TimerTask

import javax.swing.{JPanel, JFrame}
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.Graphics
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.awt.event.KeyListener
import java.awt.event.KeyEvent
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import java.awt.Point


abstract class Engine
{
	val title: String

	val windowSize: Dimension

	val offsetFix: Point = new Point(-6, -32)

	val framerate: Int

	private var isRunning = true

	private var _frame: JFrame = null
	def frame = _frame

	private var _panel: JPanel = null
	def panel = _panel

	var gamePanel: JPanel

	/**
	  * This function should be called every frame.
		* @param delta time between last frame.
	  */
	def run(delta: Double): Unit =
	{
		//println("Running")

		gamePanel.repaint()
	}
	def run: Unit =
	{
		start

		var t1 = System.currentTimeMillis()
		while (isRunning)
			Thread.sleep(1000L / framerate)
			run(System.currentTimeMillis - t1)
			t1 = System.currentTimeMillis()
	}

	/**
	  * Called once when the game starts.
	  */
	def start: Unit =
	{
		_frame = new JFrame(title)
		frame.setSize(windowSize)
		frame.setResizable(false)
		frame.setBackground(Color.cyan)
		frame.setLocationRelativeTo(null)
		frame.add(gamePanel)

		frame.addWindowListener(new WindowAdapter()
		{
			override def windowClosing(e: WindowEvent): Unit =
				quit
				super.windowClosing(e)
		})

		frame.addKeyListener(new KeyListener {
			override def keyPressed(e: KeyEvent): Unit = keyDown(e.getKeyCode())
			override def keyReleased(e: KeyEvent): Unit = keyUp(e.getKeyCode())
			override def keyTyped(e: KeyEvent): Unit = keyKB(e.getKeyCode())
		})

		frame.addMouseListener(new MouseListener
		{
			override def mouseClicked(e: MouseEvent): Unit = onClick(e.getPoint().x, e.getPoint().y)
			override def mouseEntered(e: MouseEvent): Unit = {}
			override def mouseExited(e: MouseEvent): Unit = {}
			override def mousePressed(e: MouseEvent): Unit = {}
			override def mouseReleased(e: MouseEvent): Unit = {}
		})

		frame.show()
	}

	def quit: Unit =
	{
		frame.dispose()
		isRunning = false
	}


	/**
	  * Key pressed listener.
	  *
	  * @param key Key that has just been pressed.
	  */
	def keyDown(key: Int): Unit =
	{
		key match
			case 'Q' => quit
			case default => ;
	}

	/**
	  * Key released listener.
	  *
	  * @param key Key that has just been released.
	  */
	def keyUp(key: Int): Unit

	/**
	  * Key typed listener.
	  *
	  * @param key Key that has just been typed.
	  */
	def keyKB(key: Int): Unit

	/**
	  * Mouse click listener.
	  *
	  * @param x X pixel (from left).
	  * @param y Y pixel (from top).
	  */
	def onClick(x: Int, y: Int): Unit
}