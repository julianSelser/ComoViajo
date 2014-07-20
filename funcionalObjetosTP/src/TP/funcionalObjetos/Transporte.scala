package TP.funcionalObjetos

import mock._
import math._

abstract class Transporte extends
{
  def linea: String
  def compania: String

  def costoEntre(origen: Direccion, destino: Direccion): Float
  def duracionEntre(origen: Direccion, destino: Direccion): Float
  def nombre = this.toString
  override def toString = this.getClass.toString.split('.').last
}

trait TransporteConEstaciones {
  def estaciones: Seq[Estacion]
  
  //el numero de estaciones es el valor absoluto de al diferencia entre indices
  def cantEstacionesEntreDirecciones(origen:Direccion, destino:Direccion) = 
    abs(estaciones.indexWhere(e => e.direccion == origen) - estaciones.indexWhere(e => e.direccion == destino))
  
}

case class Colectivo(linea: String = "", compania: String = "") extends Transporte 
{
  def costoEntre(origen: Direccion, destino: Direccion): Float = {
    val distancia = ModuloT.distanciaColectivoEntre(origen, destino, this)

    distancia match {
      case _ if distancia <= 3f => 2.5f
      case _ if distancia < 6f => 2.75f
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
  def costoEntre(origen:Direccion, destino:Direccion) = 4.5f //segun lo que dice el enunciado
  
  def duracionEntre(origen: Direccion, destino: Direccion) = 2*cantEstacionesEntreDirecciones(origen, destino)
}

case class Tren(estaciones: Seq[Estacion], linea: String = "", compania: String = "", precios: Int=>Float = preciosPorEstacionDefault) extends Transporte with TransporteConEstaciones 
{
  def costoEntre(origen: Direccion, destino: Direccion) = precios(cantEstacionesEntreDirecciones(origen, destino))
  
  def duracionEntre(origen: Direccion, destino: Direccion) = 3*cantEstacionesEntreDirecciones(origen, destino)
}

case class Punto(val direccion: Direccion, val transporte: Transporte)