package functional.part6
import java.util.Calendar

object printUtils {

  def detailedPrintln(str: String): Unit = {
    val now = Calendar.getInstance()
    val h = now.get(Calendar.HOUR)
    val m = now.get(Calendar.MINUTE)
    val s = now.get(Calendar.SECOND)
    val ms = now.get(Calendar.MILLISECOND)
    println(s"[${Thread.currentThread().getName}] [$h:$m::$s.$ms] ${str} ")
  }

}
