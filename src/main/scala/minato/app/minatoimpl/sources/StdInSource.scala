package minato.app.minatoimpl.sources

import java.util.concurrent.Executors

import minato.app.minatoimpl.CargoImpl
import minato.core.cargo.Cargo
import minato.core.passage.{PreparedSource, Source}

import scala.concurrent.ExecutionContext

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */
class StdInJsonSource extends Source {
  override def prepare(handler: (Cargo) => Unit) = {
    val pool = Executors.newCachedThreadPool()
    implicit val _ = ExecutionContext.fromExecutorService(pool)
    () => {
      scala.concurrent.Future {
        while (true) {
          try {
            val json = io.StdIn.readLine()
            handler(CargoImpl.JSON(json))
          }catch{
            case e => e.printStackTrace()
          }
        }
      }
    }
  }
}
