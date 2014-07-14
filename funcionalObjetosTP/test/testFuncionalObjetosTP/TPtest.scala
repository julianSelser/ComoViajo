package testFuncionalObjetosTP

import TP.funcionalObjetos._
import org.junit._
import org.junit.Assert._
import mock.ModuloT
import transportes._

class TPtest 
{      
  @Test
  def `prueba del metodo "decime"` 
  {    
    ModuloT.transportes devuelve Seq(Punto(Direccion("Campus", "1420"), Colectivo("114")))
    ModuloT.transportes devuelve Seq(Punto(Direccion("Medrano", "2455"), Colectivo("114")))
   
    ModuloT.distanciaColectivoEntre setRetornoDefault 4
    ModuloT.distanciaAPieEntre setRetornoDefault 3
    
    //Da solo un recorrido directo segun lo planeado
    val comoViajo = comoViajoEntre(Direccion("Cordoba", "980"), Direccion("Ramallo", "109"))
    val test = decime(comoViajo masBarato)
    val deberiaDar = "Caminar 3.0 metros desde calle Cordoba al 980 hasta calle Medrano al 2455. En la calle Medrano al 2455 subirse al Colectivo y viajar durante 1.0 minutos antes de bajarse en la calle Campus.Caminar 3.0 metros desde calle Campus al 1420 hasta calle Ramallo al 109."
    
    assertEquals(test, deberiaDar)        
  }  
     
  @Test
  def `prueba de tarjeta de discapacitados` 
  {    
    ModuloT.transportes devuelve Seq(Punto(Direccion("Campus", "1420"), Colectivo("114")))
    ModuloT.transportes devuelve Seq(Punto(Direccion("Medrano", "2455"), Colectivo("114")))
   
    ModuloT.distanciaColectivoEntre setRetornoDefault 4
    ModuloT.distanciaAPieEntre setRetornoDefault 3
    
    val comoViajo = comoViajoEntre(Direccion("Cordoba", "980"), Direccion("Ramallo", "109"))
    val recorrido =  (comoViajo masBarato).conTarjeta(Discapacitado())
    val deberiaDar = 0f
    
    assertEquals(recorrido.precio, deberiaDar, 0f)        
  } 
  
  @Test
  def `prueba de armado de recorridos directos` 
  {    
    ModuloT.transportes devuelve Seq(Punto(Direccion("calle"), Colectivo("11")))
    ModuloT.transportes devuelve Seq(Punto(Direccion("CALLE"), Colectivo("11")))
   
    ModuloT.distanciaColectivoEntre setRetornoDefault 4
    ModuloT.distanciaAPieEntre setRetornoDefault 3
    
    //Da solo un recorrido directo segun lo planeado
    val tramosDelRecorrido = comoViajoEntre(Direccion("calle1"), Direccion("calle2")).head.tramos
    val deberiaDar = List(Camninando(Direccion("calle1","",""),Direccion("CALLE","",""),7.5f,0.0f), Viajando(Colectivo("11",""),Direccion("CALLE","",""),Direccion("calle","",""),2.75f,1.0f), Camninando(Direccion("calle","",""),Direccion("calle2","",""),7.5f,0.0f))
    
    assertEquals(tramosDelRecorrido, deberiaDar)        
  }  
  
  @Test
  def `prueba de armar combinaciones con colectivos y NO HAY combinacion` 
  {
    val elOnce = new Colectivo("11", "Compania2")
    val elSiete = new Colectivo("7", "Compania1")
    val direccion = new Direccion("Direccion", "Altura", "Barrio",new Zona("Zona"))
    
    ModuloT.hayCombinacion setRetornoDefault None
    
    assertEquals(armarCombinaciones(Punto(direccion, elOnce),Punto(direccion, elSiete)), Nil)
  }
  
  @Test
  def `prueba de armar combinaciones con colectivos y HAY combinacion` 
  {
    val colectivoOrigen = new Colectivo("11", "Compania2")
    val colectivoDestino = new Colectivo("7", "Compania1")
    
    val direccionPrincipio = new Direccion("dPrincipio", "Altura", "Barrio")
    val direccionFinal = new Direccion("dFinal", "Altura", "Barrio")
    val direccionCombinacionBajar = new Direccion("dCBajas", "Altura", "Barrio")
    val direccionCombinacionSubir = new Direccion("dCSubis", "Altura", "Barrio")
    
    ModuloT.hayCombinacion setRetornoDefault Some(direccionCombinacionBajar)
    ModuloT.transportes setRetornoDefault Seq(Punto(direccionCombinacionSubir, colectivoDestino))
    ModuloT.distanciaAPieEntre setRetornoDefault 5
    ModuloT.distanciaColectivoEntre setRetornoDefault 3
       
    val elTestDeberiaDar = List(Viajando(Colectivo("11","Compania2"),Direccion("dPrincipio","Altura","Barrio"),Direccion("dCBajas","Altura","Barrio"),2.5f,0.75f), Combinando(12.5f,0.0f,null,null), Viajando(Colectivo("7","Compania1"),Direccion("dCSubis","Altura","Barrio"),Direccion("dFinal","Altura","Barrio"),2.5f,0.75f))
    val test = armarCombinaciones(Punto(direccionPrincipio, colectivoOrigen),Punto(direccionFinal, colectivoDestino))
    
    assertEquals(test, elTestDeberiaDar)
  }  
    
  @Test
  def `prueba de combinacion entre subtes` 
  {
    val direccionFinal = new Direccion("calleFinal")
    val direccionInical = new Direccion("calleIncial")   
    val direccionCombinacion = new Direccion("calleCombinacion")
    val dNoImporta = new Direccion("calleNoImporta")
    
    val A = new Subte("A", "Compania1", Seq(Estacion("rob", dNoImporta), Estacion("berto", direccionCombinacion)))
    val E = new Subte("E", "Compania2", Seq(Estacion("Car", dNoImporta), Estacion("los", dNoImporta), Estacion("berto", direccionCombinacion)))

    val test = armarCombinaciones(Punto(direccionInical, A),Punto(direccionFinal, E))
    val deberiaDar = List(Viajando(Subte("A","Compania1",List(Estacion("rob",Direccion("calleNoImporta","","")), Estacion("berto",Direccion("calleCombinacion","","")))),Direccion("calleIncial","",""),Direccion("calleCombinacion","",""),5.0f,5.0f), Combinando(4.0f,-4.5f,null,null), Viajando(Subte("E","Compania2",List(Estacion("Car",Direccion("calleNoImporta","","")), Estacion("los",Direccion("calleNoImporta","","")), Estacion("berto",Direccion("calleCombinacion","","")))),Direccion("calleCombinacion","",""),Direccion("calleFinal","",""),5.0f,5.0f))    
    
    assertEquals(test, deberiaDar)        
  }
  
  @Test
  def `prueba de combinacion entre trenes` 
  {
    val direccionFinal = new Direccion("calleFinal")
    val direccionInical = new Direccion("calleIncial")   
    val direccionCombinacion = new Direccion("calleCombinacion")
    val dNoImporta = new Direccion("calleNoImporta")
    
    val Mitre = new Tren(Seq(Estacion("rob", dNoImporta), Estacion("berto", direccionCombinacion)), "Mitre")
    val Sarmiento = new Tren(Seq(Estacion("Car", dNoImporta), Estacion("los", dNoImporta), Estacion("berto", direccionCombinacion)), "Sarmiento")

    val test = armarCombinaciones(Punto(direccionInical, Mitre),Punto(direccionFinal, Sarmiento))
    val deberiaDar = List(Viajando(Tren(List(Estacion("rob",Direccion("calleNoImporta","","")), Estacion("berto",Direccion("calleCombinacion","",""))),"Mitre","",null),Direccion("calleIncial","",""),Direccion("calleCombinacion","",""),5.0f,5.0f), Combinando(6.0f,0.0f,null,null), Viajando(Tren(List(Estacion("Car",Direccion("calleNoImporta","","")), Estacion("los",Direccion("calleNoImporta","","")), Estacion("berto",Direccion("calleCombinacion","",""))),"Sarmiento","",null),Direccion("calleCombinacion","",""),Direccion("calleFinal","",""),5.0f,5.0f))    
    
    assertEquals(test, deberiaDar)        
  }
  
  
  @Test
  def `prueba de "comoViajo con combinacion"` 
  {
    
    val direccionPrincipio = new Direccion("dPrincipio")
    val direccionFinal = new Direccion("dFinal")
    val direccionCombinacionBajar = new Direccion("dCBajas")
    val direccionCombinacionSubir = new Direccion("dCSubis")
    
    val colectivoOrigen = new Colectivo("11")
    val trenDestino = new Tren(Seq(Estacion("rob", Direccion()), Estacion("berto", direccionCombinacionSubir)), "Mitre")
    
    
    ModuloT.hayCombinacion setRetornoDefault Some(direccionCombinacionBajar)
    ModuloT.transportes setRetornoDefault Seq(Punto(direccionCombinacionSubir, colectivoOrigen))
    ModuloT.distanciaAPieEntre setRetornoDefault 5
    ModuloT.distanciaColectivoEntre setRetornoDefault 3
    
    val test = comoViajoEntre(direccionPrincipio, direccionFinal).head.tramos
    val deberiaDar = List(Camninando(Direccion("dPrincipio","",""),Direccion("dCSubis","",""),12.5f,0.0f), Viajando(Colectivo("11",""),Direccion("dCSubis","",""),Direccion("dCSubis","",""),2.5f,0.75f), Camninando(Direccion("dCSubis","",""),Direccion("dFinal","",""),12.5f,0.0f))
    
    assertEquals(test, deberiaDar)
  }   
  
  @Test
  def `prueba de "modulo estadistico almacena el transporte"` 
  {
    ModuloT.transportes devuelve Seq(Punto(Direccion("calle"), Colectivo("11")))
    ModuloT.transportes devuelve Seq(Punto(Direccion("CALLE"), Colectivo("11")))
   
    ModuloT.distanciaColectivoEntre setRetornoDefault 4
    ModuloT.distanciaAPieEntre setRetornoDefault 3
    
    comoViajoEntre(Direccion("calle1"), Direccion("calle2")).head.tramos
    
    assertTrue(ModuloEstadistico.recorridos.exists(r => r.transportes.contains("Colectivo")))
  }
}