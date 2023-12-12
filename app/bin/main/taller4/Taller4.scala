/**
  * Taller 3 - Programación Funcional
  * Autores: miguel angel cebalos 2259619, Cristhian leonardo Albarracin zapata 1968253 ,Nicolás Gutiérrez Ramírez 2259515
  * Profesor: Carlos A Delgado
  */
package taller4


import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.util.Random
//import common._

object Taller4 {

  def saludo() = "Taller 4 2023-II"

  def main(args: Array[String]): Unit = {
    val letrasCorrectas = Seq(Seq('a', 'b'),Seq('a', 'a'))
    val listaCadenas = Seq('c', 'd')
    val buscadorCadena1 =  new BuscadorCadena("acca", Seq('a','b','c','d'))
    //val resultado = buscadorCadena1.gemerarCombinaciones(listaCadenas,2,Seq(Seq('c'),Seq('d')))
    val resultado = buscadorCadena1.PRC_turbo_mejorada()
    println(resultado)
    println(saludo())
    // Imprime o realiza acciones con el resultado según sea necesario
  }
}
