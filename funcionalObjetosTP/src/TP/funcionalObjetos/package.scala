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

  def armarRecorrido(origen: Direccion, POrigen: Punto, PDestino: Punto, destino: Direccion): RecorridoBase =
  {
    val viaje = vaDirectoOCombina(POrigen, PDestino)

    if (viaje nonEmpty) {
      new RecorridoBase(
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
      Viaje(x.transporte, x.direccion, y.direccion, x.transporte.costoEntre(x.direccion, y.direccion), x.transporte.duracionEntre(x.direccion, y.direccion)) :: Nil
  }

  def decime(recorrido: Recorrido) = {
    ModuloEstadistico.viajes ++= recorrido.viajes

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
  
  //----FUNCIONES AUXILIARES PARA TARJETAS---
  def todo(a:Boolean, b:Boolean) = a && b
  def alguno(a:Boolean, b:Boolean) = a || b
  
  //-----FUNCIONES DE MODULO ESTADISTICO-----

  //filtros  
  def paraTodos = cumple(viaje => true)
  
  def paraZona(zona:String) = cumple(viaje => viaje.zona == zona)
    
  def paraCompania(compania:String) = cumple(viaje => viaje.transporte.compania == compania)
    
  def paraTransporte(transporte:String) = cumple(viaje => viaje.transporte.linea == transporte)
    
  def paraTipoDeTransporte(tipoTransporte:Transporte) = cumple(viaje => viaje.transporte == tipoTransporte)
  
  def cumple(esCierto:Viaje=>Boolean) = (viaje:Viaje) => if (esCierto(viaje))  Some(viaje) else None  
    
    
  //estadisticas  
  def porcentajeUtilizacion = estadisticaSobreTotal(_)
  def facturacionTotal = foldEstadisticas(viaje => viaje.costo)(_)
  def costoPromedio = foldEstadisticas(viaje => viaje.costo, (res, numElems) => res/numElems)(_)
  def tiempoPromedio = foldEstadisticas(viaje => viaje.duracion, (res, numElems) => res/numElems)(_)
  
  def estadisticaSobreTotal(comp: Map[String, List[Viaje]]) = {
    val estadistica = foldEstadisticas(viaje => 1)(comp)
    
    val total = estadistica.values.foldLeft(0f)((res, valor) => res + valor)
    
    estadistica.mapValues(res => res/total)    
  }
  
  def foldEstadisticas(fviaje: Viaje => Float, flista: (Float, Float) => Float = (x, y) => x)(comp: Map[String, List[Viaje]]) = {
    comp.mapValues(lsViajes => flista(lsViajes.foldLeft(0f)((res, viaje) => res + fviaje(viaje)), lsViajes.length))
  }
}