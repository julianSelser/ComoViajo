package TP.funcionalObjetos

import mock.ModuloT
import transportes._

trait Tramo {
  def origen: Direccion
  def destino: Direccion
  def costo: Float
  def duracion: Float
}

case class Camninando(origen: Direccion, destino: Direccion, duracion: Float, costo: Float = 0) extends Tramo

case class Viajando(transporte: Transporte, origen: Direccion, destino: Direccion, costo: Float, duracion: Float) extends Tramo

case class Combinando(duracion: Float, costo:Float, origen: Direccion = null, destino: Direccion = null) extends Tramo

case class ArmarCombinacion(seguir:Boolean = true)
{
  def apply(p1: Punto, p2: Punto): List[Tramo] = {

    (p1.transporte, p2.transporte) match {
      case (t1: Colectivo, t2: Colectivo) if hayCombinacionEntreColectivos(t1, t2)=> {
        ModuloT.hayCombinacion(t1, t2) match {
          case Some(dir1d) => {
            val dir2o = ModuloT.transportes(dir1d).find(_.transporte == p2.transporte).get.direccion

            listaCombinacion(p1, p2, dir1d, dir2o, tardanzaCaminandoEntre(dir1d, dir2o))
          }
          
          case None => Nil
        }
      }

      case (t1: Colectivo, t2: TransporteConEstaciones) if hayCombinacionesColecTransConEstaciones(t1, t2) => {
        val combinacion = combinacionesColecTransConEstaciones(t1, t2).head
        val dt1d = combinacion._1.direccion
        val dt2o = combinacion._2.direccion
        
        listaCombinacion(p1, p2, dt1d, dt2o, tardanzaCaminandoEntre(dt1d, dt2o))
      }

      case (t1: Subte, t2: Subte) if hayCombinacionEntreEstaciones(t1, t2) => {
        val combinacion = combinacionesEntreEstaciones(t1, t2).head

        listaCombinacion(p1, p2, combinacion.direccion, combinacion.direccion, 4, -4.5f) 
      }
      
      case (t1: Tren, t2: Subte) if hayCombinacionEntreEstaciones(t1, t2) => {
        val combinacion = combinacionesEntreEstaciones(t1, t2).head
        
        listaCombinacion(p1, p2, combinacion.direccion, combinacion.direccion, 5)
      }  
      
      case (t1: Tren, t2: Tren) if hayCombinacionEntreEstaciones(t1, t2) => {
        val combinacion = combinacionesEntreEstaciones(t1, t2).head
        
        listaCombinacion(p1, p2, combinacion.direccion, combinacion.direccion, 6)
      }      
      
      case otro if seguir => ArmarCombinacion(false)(p2, p1).reverse //sino: damos vuelta los parametros y volvemos a probar...
      
      case otro => Nil
    }
  }

  def hayCombinacionEntreEstaciones(t1: TransporteConEstaciones, t2: TransporteConEstaciones) = combinacionesEntreEstaciones(t1, t2).nonEmpty

  def combinacionesEntreEstaciones(t1: TransporteConEstaciones, t2: TransporteConEstaciones) = (t1.estaciones intersect t2.estaciones)
  
  def hayCombinacionesColecTransConEstaciones(t1:Colectivo, t2:TransporteConEstaciones) = combinacionesColecTransConEstaciones(t1, t2).nonEmpty

  def hayCombinacionEntreColectivos(t1: Colectivo, t2: Colectivo) = {
    val combinacion = ModuloT.hayCombinacion(t1, t2)

    None != ModuloT.hayCombinacion(t1, t2) && ModuloT.transportes(combinacion.get).exists(_.transporte == t2)
  }
  
  def combinacionesColecTransConEstaciones(t1:Colectivo, t2:TransporteConEstaciones) = {
    for {
      estacion <- t2.estaciones
      puntos = ModuloT.transportes(estacion.direccion)
      punto <- puntos
      if punto.transporte == t1
    } yield (punto, estacion) 
  }
  
  def listaCombinacion(p1: Punto, p2: Punto, dt1d: Direccion, dt2o: Direccion, duracion: Float, costo: Float = 0) = {
    val tr1 = p1.transporte
    val tr2 = p2.transporte
    
    List(
      Viajando(tr1, p1.direccion, dt1d, tr1.costoEntre(p1.direccion, dt1d), tr1.duracionEntre(p1.direccion, dt1d)),
      Combinando(duracion, costo),
      Viajando(tr2, dt2o, p2.direccion, tr2.costoEntre(dt2o, p2.direccion), tr2.duracionEntre(dt2o, p2.direccion)))
  }
}

