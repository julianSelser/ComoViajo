package TP.funcionalObjetos

class Recorrido(val tramos:List[Tramo])
{
  def precio = tramos.foldLeft(0f)((resultado, tramo) => resultado + tramo.costo)
  
  def duracion = tramos.foldLeft(0f)((resultado, tramo) => resultado + tramo.duracion)
}