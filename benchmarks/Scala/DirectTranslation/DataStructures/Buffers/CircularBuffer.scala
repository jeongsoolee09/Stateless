package DataStructures.Buffers

import java.util.Random
import java.util.concurrent.atomic.AtomicInteger
import java.lang.Thread

class CircularBuffer(var buffer_size: Int) {

  if (!isPowerOfTwo(buffer_size)) {
    throw new IllegalArgumentException()
  }

  val _buffer_size = buffer_size
  private var _buffer: Array[Char] = new Array[Char](buffer_size )
  private var _write_index: Int = 0
  private var _read_index: Int = 0
  private var _readable_data : AtomicInteger = new AtomicInteger(0)

  def isPowerOfTwo(i: Int): Boolean = (i % (i-1)) == 0

  def getTrueIndex(i: Int): Int = i % _buffer_size

  def readOutChar(): Character = {
    var result: Character = null

    if (_readable_data.get() > 0) {
      result = Character.valueOf(_buffer(getTrueIndex(_read_index)))
      _readable_data.decrementAndGet()
      _read_index += 1
    }

    result
  }

  def writeToCharBuffer(c: Char): Boolean = {
    var result: Boolean = false

    if (_readable_data.get() < _buffer_size) {
      _buffer(getTrueIndex(_write_index)) = c
      _readable_data.incrementAndGet()
      _write_index += 1
      result = true
    }

    result
  }

  private class TestWriteWorker(var cb: CircularBuffer) extends Runnable {
    var _alphabet: String = "abcdefghijklmnopqrstuvwxyz0123456789"
    var _random: Random = new Random()
    var _buffer : CircularBuffer= cb

    private def getRandomChar(): Char = {
      _alphabet.charAt(_random.nextInt(_alphabet.length()))
    }

    def run(): Unit = {
      while (!Thread.interrupted) {
        if (!_buffer.writeToCharBuffer(getRandomChar())) {
          Thread.`yield`()
          try {
            Thread.sleep(10)
          } catch {
            case e: InterruptedException => return
          }
        }
      }
    }
  }

  private class TestReadWorker(var vb: CircularBuffer) extends Runnable {
    var _buffer: CircularBuffer = vb

    override def run(): Unit = {
      println("Printing Buffer:")
      while (!Thread.interrupted()) {
        val c: Character = _buffer.readOutChar()
        if (c != null) {
          print(c.charValue())
        } else {
          Thread.yield()
          try {
            Thread.sleep(10)
          } catch (InterruptedException) {
            case e: InterruptedException => {
              println()
              return
            }
          }
        }
      }
    }
  }
}

object Main extends App = {
  def main(): Unit = {
    val buffer_size = 1024
    val cb: CircularBuffer = new CircularBuffer(buffer_size)

    val write_thread: Thread = new Thread(new TestWriteWorker(cb))
    val read_thread: Thread = new Thread(new TestReadWorker(cb))
    read_thread.start()
    write_thread.start()

    Thread.sleep(10000)

    write_thread.interrupt()
    read_thread.interrupt()
  }
}
