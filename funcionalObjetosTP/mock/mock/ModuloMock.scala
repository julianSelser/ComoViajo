package mock

import mock._
import transportes._
import TP.funcionalObjetos._
import mock.MetodoMock

object ModuloT 
{
  val transportes = new MetodoMock[(Direccion), Seq[Punto]]
  val hayCombinacion = new MetodoMock[(Colectivo, Colectivo), Option[Direccion]]
  val distanciaAPieEntre = new MetodoMock[(Direccion, Direccion), Float]
  val distanciaColectivoEntre = new MetodoMock[(Direccion, Direccion, Colectivo), Float]
}