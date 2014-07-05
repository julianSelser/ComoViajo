package mock

import mock._
import transportes._
import TP.funcionalObjetos._
import util.MetodoMock

object ModuloT 
{
  var transportes = new MetodoMock[(Direccion), Seq[Punto]]
  var hayCombinacion = new MetodoMock[(Colectivo, Colectivo), Option[Direccion]]
  var distanciaAPieEntre = new MetodoMock[(Direccion, Direccion), Float]
  var distanciaColectivoEntre = new MetodoMock[(Direccion, Direccion, Colectivo), Float]

  def init = {
    transportes = new MetodoMock[(Direccion), Seq[Punto]]
    hayCombinacion = new MetodoMock[(Colectivo, Colectivo), Option[Direccion]]
    distanciaAPieEntre = new MetodoMock[(Direccion, Direccion), Float]
    distanciaColectivoEntre = new MetodoMock[(Direccion, Direccion, Colectivo), Float]   
  }
}