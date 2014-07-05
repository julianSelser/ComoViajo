package transportes

import mock.ModuloT
import TP.funcionalObjetos._

abstract class Transporte{
  def linea: String
  def compania: String

  def costoEntre(origen: Direccion, destino: Direccion): Float
  def duracionEntre(origen: Direccion, destino: Direccion): Float
}

trait TransporteConEstaciones {
  def estaciones: Seq[Estacion]
}

case class Colectivo(linea: String = "", compania: String = "") extends Transporte 
{
  def costoEntre(origen: Direccion, destino: Direccion): Float = {
    val distancia = ModuloT.distanciaColectivoEntre(origen, destino, this)

    distancia match {
      case _ if distancia <= 3f => 2.5f
      case _ if distancia > 3f && distancia < 6f => 2.75f
      case _ if distancia >= 6f => 2.85f
    }
  }

  def duracionEntre(origen: Direccion, destino: Direccion) = {
    val distancia = ModuloT.distanciaColectivoEntre(origen, destino, this)
    
    (distancia*15)/60 //en minutos...por regla de 3 con lo que dice en el enunciado
  }
}

case class Subte(linea: String = "", compania: String = "", estaciones: Seq[Estacion]) extends Transporte with TransporteConEstaciones 
{
  //falta implementar...para mostrarlo saco las NotImplementedExceptions
  def costoEntre(origen:Direccion, destino:Direccion) = 5
  def duracionEntre(origen: Direccion, destino: Direccion) = 5
}

case class Tren(estaciones: Seq[Estacion], linea: String = "", compania: String = "", precios: Map[Int, Float] = null) extends Transporte with TransporteConEstaciones 
{
  //falta implementar...para mostrarlo saco las NotImplementedExceptions
  def costoEntre(origen: Direccion, destino: Direccion) = 5
  def duracionEntre(origen: Direccion, destino: Direccion) = 5
}

case class Punto(val direccion: Direccion, val transporte: Transporte)