package testFuncionalObjetosTP

import TP.funcionalObjetos._
import org.junit._
import org.junit.Assert._
import mock.ModuloT

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
    val deberiaDar = "Caminar 3.0 metros desde calle Cordoba al 980 hasta calle Medrano al 2455. En la calle Medrano al 2455 subirse al Colectivo y viajar durante 1.0 minutos antes de bajarse en la calle Campus.Caminar 3.0 metros desde calle Campus al 1420 hasta calle Ramallo al 109. Costo: 2.75"

    assertEquals(test, deberiaDar)        
  }  
     
  @Test
  def `prueba de tarjeta de discapacitados` 
  {    
    ModuloT.transportes devuelve Seq(Punto(Direccion("Campus", "1420"), Colectivo("114")))
    ModuloT.transportes devuelve Seq(Punto(Direccion("Medrano", "2455"), Colectivo("114")))
   
    ModuloT.distanciaColectivoEntre setRetornoDefault 4
    ModuloT.distanciaAPieEntre setRetornoDefault 3
    
    val recorrido = comoViajoEntre(Direccion("Cordoba", "980"), Direccion("Ramallo", "109")) masBarato
    val recorridoConTarjeta =  TarjetaDiscapacitado(recorrido)
    
    assertEquals(recorridoConTarjeta.precio, 0f, 0f)        
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
    val deberiaDar = List(Camninando(Direccion("calle1","",""),Direccion("CALLE","",""),7.5f,0.0f), Viaje(Colectivo("11",""),Direccion("CALLE","",""),Direccion("calle","",""),2.75f,1.0f), Camninando(Direccion("calle","",""),Direccion("calle2","",""),7.5f,0.0f))
    
    assertEquals(tramosDelRecorrido, deberiaDar)        
  }  
  
  @Test
  def `prueba de armar combinaciones con colectivos y NO HAY combinacion` 
  {
    val elOnce = new Colectivo("11", "Compania2")
    val elSiete = new Colectivo("7", "Compania1")
    val direccion = new Direccion("Direccion", "Altura", "Barrio")
    
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
       
    val elTestDeberiaDar = List(Viaje(Colectivo("11","Compania2"),Direccion("dPrincipio","Altura","Barrio"),Direccion("dCBajas","Altura","Barrio"),2.5f,0.75f), Combinando(12.5f,0.0f,null,null), Viaje(Colectivo("7","Compania1"),Direccion("dCSubis","Altura","Barrio"),Direccion("dFinal","Altura","Barrio"),2.5f,0.75f))
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
    val deberiaDar = List(Viaje(Subte("A","Compania1",List(Estacion("rob",Direccion("calleNoImporta","","","")), Estacion("berto",Direccion("calleCombinacion","","","")))),Direccion("calleIncial","","",""),Direccion("calleCombinacion","","",""),4.5f,4.0f), Combinando(4.0f,-4.5f,null,null), Viaje(Subte("E","Compania2",List(Estacion("Car",Direccion("calleNoImporta","","","")), Estacion("los",Direccion("calleNoImporta","","","")), Estacion("berto",Direccion("calleCombinacion","","","")))),Direccion("calleCombinacion","","",""),Direccion("calleFinal","","",""),4.5f,6.0f))    
    
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

    //por alguna razon no reconoce a la funcion de precios default
    val f = Mitre.precios
    val g = Sarmiento.precios
    
    val test = armarCombinaciones(Punto(direccionInical, Mitre),Punto(direccionFinal, Sarmiento))
    val deberiaDar = List(Viaje(Tren(List(Estacion("rob",Direccion("calleNoImporta","","","")), Estacion("berto",Direccion("calleCombinacion","","",""))),"Mitre","",f),Direccion("calleIncial","","",""),Direccion("calleCombinacion","","",""),2.0f,6.0f), Combinando(6.0f,0.0f,null,null), Viaje(Tren(List(Estacion("Car",Direccion("calleNoImporta","","","")), Estacion("los",Direccion("calleNoImporta","","","")), Estacion("berto",Direccion("calleCombinacion","","",""))),"Sarmiento","",g),Direccion("calleCombinacion","","",""),Direccion("calleFinal","","",""),2.0f,9.0f))    
    
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
    val trenDestino = new Tren(Seq(Estacion("rob"), Estacion("berto", direccionCombinacionBajar)), "Mitre")  
    
    ModuloT.distanciaColectivoEntre setRetornoDefault 5;
    ModuloT.distanciaAPieEntre setRetornoDefault 3
    ModuloT.hayCombinacion devuelve Some(direccionCombinacionSubir)
    ModuloT.transportes setRetornoDefault Seq(Punto(direccionCombinacionSubir, colectivoOrigen))
    ModuloT.transportes devuelve Seq(Punto(direccionFinal, trenDestino))
    ModuloT.transportes devuelve Seq(Punto(direccionPrincipio, colectivoOrigen))
    
    val test = decime(comoViajoEntre(direccionPrincipio, direccionFinal) masBarato)
    
    val deberiaDar = "Caminar 3.0 metros desde calle dPrincipio al  hasta calle dPrincipio al . En la calle dPrincipio al  subirse al Colectivo y viajar durante 1.25 minutos antes de bajarse en la calle dCSubis. Hacer una combinacion que tardara 7.5minutos. En la calle calle al altura subirse al Tren y viajar durante 3.0 minutos antes de bajarse en la calle dFinal.Caminar 3.0 metros desde calle dFinal al  hasta calle dFinal al . Costo: 4.75"
    
    assertEquals(test, deberiaDar)
  }   
  
  @Test
  def `test para comparar transportes y conseguir su facturacion`
  {
    mockRecorridosEnModuloEstadistico

    val estadistica = ModuloEstadistico.compararTransportes(paraTodos)(facturacionTotal).toString

    //los transportes que se usaron fueron el tren Mitre, el colectivo 11 y el 114
    assertEquals(estadistica, "Map(Mitre -> 2.0, 11 -> 2.75, 114 -> 2.75)")
  }
  
  @Test
  def `test para comparar tipos de transporte restringido a uno en particular y conseguir su facturacion`
  {
    mockRecorridosEnModuloEstadistico

    val estadistica = ModuloEstadistico.compararTiposTransporte(paraTransporte("Mitre"))(facturacionTotal).toString

    //Solo hay un viaje en el mitre, de una estacion, entonces la facturacion es 2 pesos
    assertEquals(estadistica, "Map(Tren -> 2.0, Colectivo -> 0.0)")
  }

  @Test
  def `test estadistico de porcentaje de utilizacion de transporte en un zona`
  {
    mockRecorridosEnModuloEstadistico

    val estadistica = ModuloEstadistico.compararTiposTransporte(paraZona("Centro"))(porcentajeUtilizacion).toString

    //segun el mock, en la zona centro hay 100% (1.0) 
    assertEquals(estadistica, "Map(Tren -> 0.0, Colectivo -> 1.0)")
  }
  
  //este metodo arma dos recorridos que resultan en agregar 3 viajes al modulo estadistico
  def mockRecorridosEnModuloEstadistico
  {
    ModuloEstadistico.viajes.clear
    
    val direccionPrincipio = new Direccion("dPrincipio", "1211", "Soldati", "Sur")
    val direccionFinal = new Direccion("dFinal", "92", "Patricios", "Centro")
    val direccionCombinacionBajar = new Direccion("dCBajas", "110", "Urquiza", "Centro")
    val direccionCombinacionSubir = new Direccion("dCSubis", "11", "Nuñez", "Centro")
    
    val colectivoOrigen = new Colectivo("11", "CompaniaX")
    val trenDestino = new Tren(Seq(Estacion("rob"), Estacion("berto", direccionCombinacionBajar)), "Mitre", "TBA")  
    
    ModuloT.distanciaColectivoEntre setRetornoDefault 5;
    ModuloT.distanciaAPieEntre setRetornoDefault 3
    ModuloT.hayCombinacion devuelve Some(direccionCombinacionSubir)
    ModuloT.transportes setRetornoDefault Seq(Punto(direccionCombinacionSubir, colectivoOrigen))
    ModuloT.transportes devuelve Seq(Punto(direccionFinal, trenDestino))
    ModuloT.transportes devuelve Seq(Punto(direccionPrincipio, colectivoOrigen))
    
    decime(comoViajoEntre(direccionPrincipio, direccionFinal) masBarato)
    
    ModuloT.transportes devuelve Seq(Punto(Direccion("Campus", "1420", "Lugano", "Oeste"), Colectivo("114", "Chevalier")))
    ModuloT.transportes devuelve Seq(Punto(Direccion("Medrano", "2455", "Villa Crespo", "Centro"), Colectivo("114", "Chevalier")))
    
    decime(comoViajoEntre(Direccion("Cordoba", "980", "Villa Crespo", "Centro"), Direccion("Ramallo", "109", "Lugano", "Oeste")) masBarato)    
  }
}