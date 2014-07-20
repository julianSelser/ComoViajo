package TP.funcionalObjetos

case class Direccion(calle:String = "", altura:String = "", barrio:String = "", zona:String = "")

case class Estacion(nombre:String = "", direccion:Direccion = Direccion("calle", "altura", "barrio", "zona"))