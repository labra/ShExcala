package es.weso.utils

object PerformanceUtils {

  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }

  def getTimeNow(): Long = System.nanoTime

  def getTimeFrom(from: Long): Long = (System.nanoTime - from) / 1000

  def showTime(micros: Long): Unit = {
    println("** %d microseconds".format(micros))
  }

  def showRuntimeMemory(runtime: Runtime): Unit = {
    // memory info
    // Code from: http://alvinalexander.com/scala/how-show-memory-ram-use-scala-application-used-free-total-max
    val mb = 1024 * 1024
    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    println("** Free Memory:  " + runtime.freeMemory / mb)
    println("** Total Memory: " + runtime.totalMemory / mb)
    println("** Max Memory:   " + runtime.maxMemory / mb)
  }

}