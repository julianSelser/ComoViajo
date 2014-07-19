package TP

import mock._
import scala.collection.mutable.MutableList

package object funcionalObjetos {
  def comoViajoEntre(origen: Direccion, destino: Direccion): Seq[RecorridoBase] = {
    val transportesEnOrigen = ModuloT.transportes(origen)
    val transportesEnDestino = ModuloT.transportes(destino)

    for {
      Po <- transportesEnOrigen
      Pd <- transportesEnDestino
    } yield armarRecorrido(origen, Po, Pd, destino)
  }

  def armarRecorrido(origen: Direccion, POrigen: Punto, PDestino: Punto, destino: Direccion): RecorridoBase = {

    val viaje = vaDirectoOCombina(POrigen, PDestino)

    if (viaje nonEmpty) {
      return new RecorridoBase(
        Camninando(origen, POrigen.direccion, tardanzaCaminandoEntre(origen, POrigen.direccion))
          ::
          viaje
          :::
          Camninando(PDestino.direccion, destino, tardanzaCaminandoEntre(PDestino.direccion, destino))
          :: Nil)
    } 
    else null
  }

  def vaDirectoOCombina = seLlegaDirecto orElse combina

  def combina: PartialFunction[(Punto, Punto), List[Tramo]] = { case (x, y) => armarCombinaciones(x, y) }

  def seLlegaDirecto: PartialFunction[(Punto, Punto), List[Tramo]] = {
    case (x, y) if x.transporte == y.transporte =>
      Viajando(x.transporte, x.direccion, y.direccion, x.transporte.costoEntre(x.direccion, y.direccion), x.transporte.duracionEntre(x.direccion, y.direccion)) :: Nil
  }

  def decime(recorrido: Recorrido) = {
    ModuloEstadistico.recorridos += recorrido

    recorrido.decite
  }

  //cada tren tiene sus propias tarifas segun cantidad de estaciones, pero si no se especifica...
  //las tarifas default son las que dice el enunciado, dadas en la siguiente funcion
  def preciosPorEstacionDefault(numeroEstaciones:Int) = numeroEstaciones match{
    case _ if numeroEstaciones <= 5 => 2f
    case _ if numeroEstaciones <= 8 => 3.5f
    case _ => 4.75f
  } 
  
  def tardanzaCaminandoEntre(o: Direccion, d: Direccion) = 2.5f * ModuloT.distanciaAPieEntre(o, d)

  def armarCombinaciones(Po: Punto, Pd: Punto) = ArmarCombinacion()(Po, Pd)

  implicit def seqRecorridos(recorridos: Seq[RecorridoBase]) = new SeqRecorridos(recorridos)

  class SeqRecorridos(recorridos: Seq[RecorridoBase]) {
    def masBarato = recorridos.minBy(_.precio)
    def masRapido = recorridos.minBy(_.duracion)
  }
  
  //funciones para las tarjetas
  def todo(a:Boolean, b:Boolean) = a && b
  def alguno(a:Boolean, b:Boolean) = a || b
}