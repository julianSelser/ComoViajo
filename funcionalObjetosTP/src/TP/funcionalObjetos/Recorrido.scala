package TP.funcionalObjetos

trait Recorrido
{
  def precio:Float
  def duracion:Float
  def decite:String
  def tramos:List[Tramo]
}

class RecorridoBase(val tramos:List[Tramo]) extends Recorrido
{
  def precio = tramos.foldLeft(0f)((resultado, tramo) => resultado + tramo.costo)
  
  def duracion = tramos.foldLeft(0f)((resultado, tramo) => resultado + tramo.duracion)
  
  def decite = tramos.foldLeft("")((comoViajar, tramo) => comoViajar + tramo.decite)
  
  def conTarjeta(tarjeta:Tarjeta):Recorrido = {
    tarjeta.recorrido = this
    
    return tarjeta
  }
}