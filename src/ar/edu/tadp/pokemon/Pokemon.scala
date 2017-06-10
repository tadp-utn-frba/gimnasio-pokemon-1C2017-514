package ar.edu.tadp.pokemon

package object v1 {
  import scala.util.Try

  case class Pokemon(
      especie: Especie,
      caracteristicas: Caracteristicas) {
    def energia = caracteristicas.energia
    def energiaMaxima = caracteristicas.energiaMaxima + especie.incrementos.energiaMaxima
    def fuerza = caracteristicas.fuerza + especie.incrementos.fuerza
    def velocidad = caracteristicas.velocidad + especie.incrementos.velocidad
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
    def especie(evolucion: Especie) =
      copy(especie = evolucion)

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
          case tipo(Fantasma)                        => this
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

    def evolucionar(cond: (Pokemon, CondicionEvolutiva) => Pokemon): Pokemon = {
      especie.condicionEvolutiva.foldLeft(this)(cond)
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
      extends CondicionEvolutiva {

    override def subioDeNivel(pokemon: Pokemon) = {
      if (pokemon.nivel >= nivel) pokemon.copy(evolucion) else pokemon
    }
  }
  case class intercambio(evolucion: Especie)
      extends CondicionEvolutiva {
  }
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

  trait Piedra {
    def apply(pokemon: Pokemon, tipo: Tipo): Boolean = false
  }
  case class PiedraTipo(tipoPiedra: Tipo) extends Piedra {
    override def apply(pokemon: Pokemon, tipo: Tipo) = {
      tipoPiedra == tipo
    }
  }
  case object PiedraLunar extends Piedra {
    val especies: List[Especie] = List() // Jigglypuff, etc
    override def apply(pokemon: Pokemon, tipo: Tipo) = {
      especies.contains(pokemon.especie)
    }
  }

  type Actividad = Pokemon => Pokemon

  object descansar {
    def apply(pokemon: Pokemon) =
      pokemon.energia(pokemon.energiaMaxima)
  }

  object Intercambiar {
    def apply(pokemon: Pokemon) = {
      val evolucion = pokemon.evolucionar {
        (pokemon, condicion) =>
          condicion match {
            case intercambio(evolucion) => pokemon.especie(evolucion)
            case _                      => pokemon
          }
      }

      if (evolucion.especie == pokemon.especie) pokemon.energia(-10) else evolucion

    }
  }

  object UsarPiedra2 {

    def apply(piedra: Piedra)(pokemon: Pokemon): Pokemon = {
      pokemon.evolucionar { (pokemon, condicion) =>
        condicion match {
          case usarPiedra(tipo, evolucion) if piedra.apply(pokemon, tipo) => pokemon.especie(evolucion)
          case _ => pokemon
        }
      }
    }

  }

  case class Rutina(nombre: String, actividades: List[Actividad]) {
    def apply(pokemon: Pokemon): Try[Pokemon] = {
      actividades.foldLeft(Try(pokemon)) { (pok, actividad) =>
        pok.map(actividad)
      }
    }
  }

  type Criterio = (Pokemon, Pokemon) => Boolean

    def mejorRutinaSegun(rutinas: List[Rutina], pokemon: Pokemon, criterio: Criterio) = {
      rutinas.filter(rutina => rutina.apply(pokemon).isSuccess) //
      .sortWith {(rut1, rut2) => criterio(rut1.apply(pokemon).get, rut2.apply(pokemon).get)}.headOption.map(_.nombre)
    }

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
