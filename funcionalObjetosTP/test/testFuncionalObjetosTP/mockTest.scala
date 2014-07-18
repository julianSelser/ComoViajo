package testFuncionalObjetosTP

import org.junit._
import org.junit.Assert._
import TP.funcionalObjetos._
import mock.ModuloT
import mock._
import TP.funcionalObjetos.Colectivo

class mockTest 
{  
  @Test(expected = classOf[RetornoNoConfigurado])
  def `un metodo mockeado no configurado tira excepcion` 
  {
    val metodoMock = new MetodoMock[Unit, Float]
   
    metodoMock() //metodo mock con retorno sin configurar
  }
  
  @Test
  def `se puede configurar el retorno de un metodo mock` 
  {
    val metodoMock = new MetodoMock[Unit, String]
    
    metodoMock devuelve "Hola Mundo"
   
    assertEquals(metodoMock(), "Hola Mundo") 
  }
  
  @Test
  def `los valores de retorno se pueden apilar y se van devolviendo en orden inverso`
  {
    val metodoMock = new MetodoMock[Unit, String]
    
    metodoMock devuelve "Mundo"  devuelve "Hola "
    
    assertEquals(metodoMock()+metodoMock(), "Hola Mundo")
  }
  
  @Test
  def `se pueden tener parametros (y son ignorados)` 
  {
    val metodoMock = new MetodoMock[(String, String, String), String]
    
    metodoMock devuelve "Hola Mundo"
   
    assertEquals(metodoMock("lol", "muchos", "parametros"), "Hola Mundo") 
  }
  
  @Test
  def `se pueden mockear metodos en un objeto` 
  {
    class Clase(val metodoMockeado:MetodoMock[Unit, String] = new MetodoMock)
    
    val objeto = new Clase
    
    objeto.metodoMockeado devuelve "Un Retorno" //configuracion del metodo
   
    assertEquals(objeto.metodoMockeado(), "Un Retorno") 
  }
  
  @Test
  def `test del mock del modulo de transportes` 
  {
    val elSiete = new Colectivo("7", "Compania1")
    val elOnce = new Colectivo("11", "Compania2")

    val direccion = new Direccion("Direccion", "Altura", "Barrio")
    
    ModuloT.transportes devuelve Seq(Punto(direccion, elSiete))	//Seq con al "siete" en una direccion
    ModuloT.transportes devuelve Seq(Punto(direccion, elOnce))  //pero antes al "once"
       
    //salen en orden inverso
    assertEquals(ModuloT.transportes(direccion), Seq(Punto(direccion, elOnce)))  
    assertEquals(ModuloT.transportes(direccion), Seq(Punto(direccion, elSiete)))
  }
}

