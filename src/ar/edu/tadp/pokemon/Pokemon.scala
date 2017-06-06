package ar.edu.tadp.pokemon

package object v1 {

  case class Pokemon(
      especie: Especie,
      caracteristicas: Caracteristicas
      ){
    def energia = caracteristicas.energia
    def energiaMaxima = caracteristicas.energiaMaxima
    def fuerza = caracteristicas.fuerza
    def velocidad = caracteristicas.velocidad
    def experiencia = caracteristicas.experiencia
   
    def energia(delta: Int) = 
      copy(caracteristicas = caracteristicas.copy(energia = energia 
          + delta min energiaMaxima))
		def fuerza(delta: Int) = 
		  copy(caracteristicas = caracteristicas.copy(fuerza = fuerza + delta))
		def velocidad(delta: Int) = 
		  copy(caracteristicas = caracteristicas.copy(velocidad = velocidad + delta))
		def experiencia(delta: Int) = 
		  copy(caracteristicas = caracteristicas.copy(experiencia = experiencia + delta))
		  
	  def ganarExp(exp: Int) = {
			val crecido = copy(caracteristicas = caracteristicas.copy(experiencia = experiencia + exp))
			
			//Parte 5
			if(crecido.nivel > this.nivel){
			  especie.condicionEvolutiva
			    .map(_.subioDeNivel(crecido))
			    .foldLeft(crecido)({(semilla, potencialmenteEvolucionado) => 
			      if(potencialmenteEvolucionado.especie != this.especie)
			        potencialmenteEvolucionado
			      else semilla})
			} else crecido
			
		}
		def descansar2() = descansar(this)
		def levantarPesas(kilos : Int)= 
      if(kilos > 10) 
        energia(-10)
      else
        this match {
          case tipo(Fantasma) => this
          case tipo(Pelea) | tipoSecundario(Pelea) 
            => experiencia(kilos*2)
          case _ if especie.tipoPrimario == Fantasma => this
          case _ => experiencia(kilos)
        }
    
   	def nivel: Int = {
			def nivelR(expNivelAnterior: Int, nivelAnterior: Int): Int = {
				val expNivelSiguiente = 2 * expNivelAnterior + especie.resistenciaEvolutiva
				if (this.experiencia > expNivelSiguiente) nivelAnterior 
				else nivelR(expNivelSiguiente, nivelAnterior + 1)
			}
			nivelR(0, 1)
		}
  }

  case class Caracteristicas(
      experiencia: Int,
      energiaMaxima: Int,
      fuerza: Int,
      velocidad: Int,
      energia: Int)
  {
    require(experiencia > 0, "La Experiencia debe ser positiva")
		require(energiaMaxima > 0, "La Energia Maxima debe ser positiva")
		require(0 to energiaMaxima contains energia, s"La Energia debe ir de 0 a $energiaMaxima")
		require(1 to 100 contains fuerza, "La Fuerza debe ir de 1 a 100")
		require(1 to 100 contains velocidad, "La Velocidad debe ir de 1 a 100")
  }
  
  case class Especie(
      incrementos: Incrementos,
      tipoPrimario: Tipo,
      tipoSecundario: Option[Tipo] = None,
      condicionEvolutiva: List[CondicionEvolutiva],
      resistenciaEvolutiva : Int
    )
  
  trait CondicionEvolutiva
    { def subioDeNivel(pokemon: Pokemon) = pokemon	}
   
  case class subirDeNivel(nivel: Int, evolucion: Especie) 
      extends CondicionEvolutiva
  case class intercambio(evolucion: Especie) 
      extends CondicionEvolutiva
  case class usarPiedra(tipo:Tipo,evolucion: Especie) 
      extends CondicionEvolutiva
  
  object tipo {
    def unapply(pokemon : Pokemon) : Option[Tipo] = Some(pokemon.especie.tipoPrimario)
  }
  object tipoSecundario {
    def unapply(pokemon : Pokemon) : Option[Tipo] = pokemon.especie.tipoSecundario
  }
  
  case class Incrementos (
		energiaMaxima: Int = 0,
		peso: Int = 0,
		fuerza: Int = 0,
		velocidad: Int = 0
	)
	
  sealed trait Tipo
  case object Fuego extends Tipo
  case object Electrico extends Tipo
  case object Pelea extends Tipo
  case object Fantasma extends Tipo
  
  type Actividad = Pokemon => Pokemon
  
 
  object descansar{
    def apply(pokemon: Pokemon) = 
      pokemon.energia(pokemon.energiaMaxima)
  }
    
  val actividad : Actividad = _ levantarPesas 5
	val pikachu = Especie(Incrementos(1,1,1,1), Electrico, None, ???, ???)
	val unPikachu = Pokemon(pikachu, 
	    Caracteristicas(
	        energiaMaxima = 3,
	        fuerza = 5,
	        energia = 1,
	        velocidad = 3,
	        experiencia = 2))
	unPikachu.levantarPesas(10).descansar2
}
