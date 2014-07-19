package TP.funcionalObjetos

abstract class Tarjeta extends Recorrido
{
  def recorrido:Recorrido
  
  def precio = recorrido.precio
  def duracion = recorrido.duracion
  def decite = recorrido.decite
  def tramos = recorrido.tramos
}

case class TarjetaTurismo(recorrido:Recorrido, barrio:String, todoOAlguno:(Boolean,Boolean)=>Boolean = todo) extends Tarjeta
{
  override def precio = {
    if (tramos.foldLeft(true)((resultado, tramo) => todoOAlguno(resultado, tramo.origen.barrio == barrio && tramo.destino.barrio == barrio)))     
      precio * 0.9f
    else
      precio
  }
}

case class TarjetaDiscapacitado(recorrido:Recorrido) extends Tarjeta
{  
  override def precio = 0f
}

case class TarjetaYendoAlTrabajo(recorrido:Recorrido) extends Tarjeta
{
  override def precio = {
    val origen = tramos.head.origen
    val destino = tramos.last.destino
    
    val siVieneDeLiniersOLaBoca = origen.zona == "Liniers" || origen.barrio == "La Boca"
    
    if(siVieneDeLiniersOLaBoca && destino.zona == "Centro")
      precio - 1.5f
    else
      precio  
  }  
}

