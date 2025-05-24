import java.io.{OutputStreamWriter, PrintWriter}
import java.net.Socket
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Using

object SocketClient {
  def main(args: Array[String]): Unit = {
    val host = "127.0.0.1"
    val port = 9999

    println(s"🔌 Connexion à $host:$port...")

    val steps = List(
      Array(
        Array("F", "F", "F", "F", "F"),
        Array("F", "E", "F", "F", "F"),
        Array("F", "F", "F", "F", "F")
      ),
      Array(
        Array("F", "E", "F", "F", "F"),
        Array("E", "E", "E", "F", "F"),
        Array("F", "E", "F", "F", "F")
      ),
      Array(
        Array("E", "E", "E", "F", "F"),
        Array("E", "E", "E", "E", "F"),
        Array("E", "E", "E", "F", "F")
      )
    )

    Using(new Socket(host, port)) { socket =>
      val writer = new PrintWriter(new OutputStreamWriter(socket.getOutputStream), true)

      for ((grid, i) <- steps.zipWithIndex) {
        println(s"🚀 Étape ${i + 1} - Envoi de la grille...")

        // Envoi ligne par ligne
        for (row <- grid) {
          writer.println(row.mkString(" "))
        }

        writer.print("\n\n") // IMPORTANT : déclenche le rendu côté Pygame
        writer.flush()

        println("⏳ Attente avant prochaine étape...")
        Thread.sleep(1000)
      }

      println("✅ Grilles envoyées.")
    }.recover {
      case e => e.printStackTrace()
    }
  }
}
