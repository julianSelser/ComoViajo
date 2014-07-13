package TP.funcionalObjetos

case class Zona(nombre:String = "")

case class Direccion(calle:String = "", altura:String = "", barrio:String = "", zona:Zona = new Zona(""))

case class Estacion(nombre:String = "", direccion:Direccion = Direccion("calle", "altura","barrio", new Zona("zona")))