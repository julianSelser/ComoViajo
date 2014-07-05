package TP.funcionalObjetos

case class Direccion(calle:String = "", altura:String = "", barrio:String = "")

case class Estacion(nombre:String = "", direccion:Direccion = Direccion("calle", "altura", "barrio"))