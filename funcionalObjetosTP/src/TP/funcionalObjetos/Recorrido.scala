package TP.funcionalObjetos

trait Recorrido
{
  def precio:Float
  def duracion:Float
  def decite:String
  def tramos:List[Tramo]
  def viajes:List[Viaje]
}

class RecorridoBase(val tramos:List[Tramo]) extends Recorrido
{
  def precio = tramos.foldLeft(0f)((resultado, tramo) => resultado + tramo.costo)
  
  def duracion = tramos.foldLeft(0f)((resultado, tramo) => resultado + tramo.duracion)
  
  def decite = tramos.foldLeft("")((comoViajar, tramo) => comoViajar + tramo.decite) + " Costo: " + precio.toString

  def viajes = tramos.foldLeft(List.empty[Viaje])((viajes,tramo) => tramo match{case t:Viaje=>t::viajes;case _ => viajes})
}