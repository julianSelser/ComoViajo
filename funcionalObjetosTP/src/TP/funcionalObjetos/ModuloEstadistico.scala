package TP.funcionalObjetos

import scala.collection.mutable.MutableList

object ModuloEstadistico
{  
  val viajes:MutableList[Viaje] = MutableList()
  
  def compararZonas = comparar(viaje => viaje.zona)(_)
  def compararCompanias = comparar(viaje => viaje.transporte.compania)(_)
  def compararTransportes = comparar(viaje => viaje.transporte.linea)(_)
  def compararTiposTransporte = comparar(viaje => viaje.transporte.nombre)(_)
   
  //arma un mapa de "zona, transportes, companias o tiposde transporte" a lista de viajes
  //por ejemplo, si comparamos por tipo de transporte:
  //        Map(
  //            Tren-> Viaje1, Viaje2, Viaje3
  //            Subte-> Viaje1, Viaje2
  //            Colectivo -> ViajeN 
  //			)
  //despues restringe los valores segun una funcion(por zona, tipoTransporte, etc)
  // y finalmente calcula las estadisticas que nos piden pasando la funcion que lo hace(costo, etc)
  def comparar[TipoClave](clave:Viaje => TipoClave)(restringir:Viaje=>Option[Viaje])(estadistica:Map[TipoClave, List[Viaje]]=>Map[TipoClave, Float]):Map[TipoClave, Float] = 
  {    
    val comparacion = viajes.foldLeft(Map.empty[TipoClave, List[Viaje]])((comp, v) => comp + (clave(v) -> (v::(comp get (clave(v)) getOrElse Nil))))

    val comparacionFiltrada = comparacion.mapValues(lsViajes => lsViajes.map(viaje => restringir(viaje)).flatten)

    estadistica(comparacionFiltrada)
  }
}