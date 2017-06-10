package ar.edu.tadp.pokemon

object bla {
  println("Welcome to the Scala worksheet")
  def bla(r: String): Int = {
    r
    r
    r
    r
    10
  }
  
  bla("10" + {() => println("aaaaaaahhh"); "asdasd"}.apply())
      
  def bla(r: => String): Int = {
    r
    r
    r
    r
    10
  }
                                                  
  class Persona(nombre: String, amigo: => Persona) {
  	def amigo2 = amigo
  	def toString = nombre
  }
  
  
  lazy val tito: Persona = new Persona("tito", jose)
  lazy val jose: Persona = new Persona("jose", tito)
  tito.amigo2
}