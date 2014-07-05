package TP

import mock._
import transportes._
import scala.collection.mutable.MutableList

package object funcionalObjetos
{ 
  def comoViajoEntre(origen: Direccion, destino: Direccion):Seq[Recorrido] = {
    val transportesEnOrigen = ModuloT.transportes(origen)
    val transportesEnDestino = ModuloT.transportes(destino)
 
    def vaDirectoOCombina = seLlegaDirecto orElse combina
    
    for {
      Po <- transportesEnOrigen
      Pd <- transportesEnDestino
    } 
    yield new Recorrido(
        Camninando(origen, Po.direccion, tardanzaCaminandoEntre(origen, Po.direccion))
        ::
        vaDirectoOCombina(Po, Pd)
        :::
        Camninando(Pd.direccion, destino, tardanzaCaminandoEntre(Pd.direccion, destino))::Nil)
  }  
  
  def combina:PartialFunction[(Punto, Punto), List[Tramo]] = { case (x, y) => armarCombinaciones(x, y)}
  
  def seLlegaDirecto:PartialFunction[(Punto, Punto), List[Tramo]] = { case (x, y) if x.transporte == y.transporte => 
    Viajando(x.transporte, x.direccion, y.direccion, x.transporte.costoEntre(x.direccion, y.direccion), x.transporte.duracionEntre(x.direccion, y.direccion))::Nil
  } 
  
  def decime(recorrido:Recorrido) = {
    ModuloEstadistico.recorridos += recorrido
    
    recorrido.tramos.foldLeft("")((comoViajar, tramo) => comoViajar + tramo.decite)
  }
      
  def tardanzaCaminandoEntre(o:Direccion, d:Direccion) =  2.5f*ModuloT.distanciaAPieEntre(o, d)

  def armarCombinaciones(Po:Punto, Pd:Punto) = ArmarCombinacion()(Po, Pd)

  implicit def seqRecorridos(recorridos: Seq[Recorrido]) = new SeqRecorridos(recorridos)
  
  class SeqRecorridos(recorridos: Seq[Recorrido]) {
    def masBarato = recorridos.minBy(_.precio) 
    def masRapido = recorridos.minBy(_.duracion) 
  }
}