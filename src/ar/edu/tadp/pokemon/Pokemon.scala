package ar.edu.tadp.pokemon

package object v1 {

  import scala.util.Try
  import scala.util.Failure
  case class Pokemon(
      especie: Especie,
      caracteristicas: Caracteristicas) {

    val energia = {
      caracteristicas.energia + energiaMaxima
    }
    val energiaMaxima = {
      caracteristicas.energiaMaxima + energia
    }
    val fuerza = caracteristicas.fuerza
    val velocidad = caracteristicas.velocidad
    val experiencia = caracteristicas.experiencia

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
      if (crecido.nivel > this.nivel) {
        especie.condicionEvolutiva
          .map(_.subioDeNivel(crecido))
          .foldLeft(crecido)({ (semilla, potencialmenteEvolucionado) =>
            if (potencialmenteEvolucionado.especie != this.especie)
              potencialmenteEvolucionado
            else semilla
          })
      } else crecido

    }
    def descansar2() = descansar(this)
    def levantarPesas(kilos: Int) =
      if (kilos > 10)
        energia(-10)
      else
        this match {
          case tipo(Fantasma) =>
            throw new RuntimeException("Los fantasmas no pueden leventar pesas")
          case tipo(Pelea) | tipoSecundario(Pelea)   => experiencia(kilos * 2)
          case _ if especie.tipoPrimario == Fantasma => this
          case _                                     => experiencia(kilos)
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
      energia: Int) {
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
    resistenciaEvolutiva: Int)

  trait CondicionEvolutiva { def subioDeNivel(pokemon: Pokemon) = pokemon }

  case class subirDeNivel(nivel: Int, evolucion: Especie)
    extends CondicionEvolutiva
  case class intercambio(evolucion: Especie)
    extends CondicionEvolutiva
  case class usarPiedra(tipo: Tipo, evolucion: Especie)
    extends CondicionEvolutiva

  object tipo {
    def unapply(pokemon: Pokemon): Option[Tipo] = Some(pokemon.especie.tipoPrimario)
  }
  object tipoSecundario {
    def unapply(pokemon: Pokemon): Option[Tipo] = pokemon.especie.tipoSecundario
  }

  case class Incrementos(
    energiaMaxima: Int = 0,
    peso: Int = 0,
    fuerza: Int = 0,
    velocidad: Int = 0)

  sealed trait Tipo
  case object Fuego extends Tipo
  case object Electrico extends Tipo
  case object Pelea extends Tipo
  case object Fantasma extends Tipo

  type Actividad = Pokemon => Pokemon

  object descansar {
    def apply(pokemon: Pokemon) =
      pokemon.energia(pokemon.energiaMaxima)
  }

  def bla(r: => String): Int = {
    r
    r
    r
    r
    10
  }

  def bla(r: () => String): Int = {
    r.apply()

    val f: PartialFunction[String, String] = {
      case p if p.length() > 10 => "mayor a 10"
      case "hola"               => "chau"
    }
    val g: PartialFunction[String, String] = {
      case p => p
    }
    val forg = f.orElse(g)

    val fog = f compose g
    val gof = f andThen g

    val map: Map[String, Int] = Map()
    map.filter(tupla => tupla._1 == "hola")
    map.filter({ case (key, value) => key == "hola" })
    map.filter({
      case ("hola", value) => true
      case _               => false
    })

    10
  }

  bla("10" + "asdasd")

  case class Rutina(nombre: String, actividades: List[Actividad]) {
    def apply(pokemon: Pokemon) =
      actividades.foldLeft(Try(pokemon)) { (tryPokemon, actividad) =>
        tryPokemon.map(actividad)
      }
  }

  type Criterio = (Pokemon, Pokemon) => Boolean

  def mejorSegun(pokemon: Pokemon, criterio: Criterio, rutinas: List[Rutina]) //
  : Option[String] = {

    val crit: (Rutina, Rutina) => Boolean = { (rut1, rut2) =>
      val poke1: Pokemon = rut1(pokemon).get
      val poke2: Pokemon = rut2(pokemon).get

      criterio(poke1, poke2)
    }

    val fort: List[Rutina] => Option[Rutina] = { rutinas =>
      rutinas.foldLeft(None: Option[Rutina]) { (acum, otraRutina) =>
        val poke1: Option[Pokemon] = acum
          .flatMap(_.apply(pokemon).toOption)
        val poke2: Option[Pokemon] = otraRutina(pokemon).toOption

        (poke1, poke2) match {
          case (None, None) => None
          case (_, None)    => acum
          case (None, _)    => Some(otraRutina)
          case (Some(pok1), Some(pok2)) =>
            if (criterio(pok1, pok2)) acum else Some(otraRutina)
        }

        val posibleMejorRutina = for {
          mejorRutina <- acum
          poke1 <- mejorRutina(pokemon).toOption
          poke2 <- otraRutina(pokemon).toOption
        } yield if (criterio(poke1, poke2)) mejorRutina else otraRutina

        
        posibleMejorRutina.fold {
//          otraRutina(pokemon).map(_ => Option(otraRutina)).orElse(acum)
          otraRutina(pokemon) match {
            case Failure(_) => acum
            case _ => Option(otraRutina)
          }
        }(Option(_))
        
      }
    }

    val mejorRutina: Option[Rutina] = rutinas
      .filter(_.apply(pokemon).isSuccess)
      .sortWith(crit)
      .headOption
    //      val mejorRutina: Option[Rutina] = rutinas
    //      .filter(_.apply(pokemon).isSuccess)
    //      .sortWith(crit)
    //      .headOption
    mejorRutina.map(_.nombre)
  }

  //  val actividad: Actividad = (pokemon => pokemon levantarPesas 5)
  val actividad: Actividad = _ levantarPesas 5
  val pikachu = Especie(Incrementos(1, 1, 1, 1), Electrico, None, ???, ???)
  val unPikachu = Pokemon(pikachu,
    Caracteristicas(
      energiaMaxima = 3,
      fuerza = 5,
      energia = 1,
      velocidad = 3,
      experiencia = 2))
  unPikachu.levantarPesas(10).descansar2

}
