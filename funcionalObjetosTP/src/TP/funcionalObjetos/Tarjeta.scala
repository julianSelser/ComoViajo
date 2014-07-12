package TP.funcionalObjetos

abstract class Tarjeta(var recorrido:Recorrido = null) extends Recorrido
{
  def precio = recorrido.precio
  def duracion = recorrido.duracion
  def decite = recorrido.decite
  def tramos = recorrido.tramos
}

case class Turismo(todoOAlguno:(Boolean,Boolean)=>Boolean = todo, barrio:String) extends Tarjeta
{
  override def precio = {
    if (tramos.foldLeft(true)((resultado, tramo) => todoOAlguno(resultado, tramo.origen.barrio == barrio && tramo.destino.barrio == barrio)))     
      precio * 0.9f
    else
      precio
  }
}

case class Discapacitado extends Tarjeta
{  
  override def precio = 0f
}

case class YendoAlTrabajo extends Tarjeta
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

