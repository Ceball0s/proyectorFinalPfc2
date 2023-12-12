/**
 * Clase BuscadorCadena
 */

package taller4

class BuscadorCadena(cadenaObjetivo: String, alfabeto: Seq[Char]) {
  type Oraculo = Seq[Char] => Boolean
  val oraculo: Oraculo = (subcadena: Seq[Char]) => cadenaObjetivo.contains(subcadena.mkString)
  val n: Int = cadenaObjetivo.length

  def agregarcombinaciones(lista: Seq[Seq[Char]],listaCadeneasAconcatenar: Seq[Char]): Seq[Seq[Char]] = {
    if (lista.isEmpty) Nil 
    else listaCadeneasAconcatenar.map{cadena => lista.head :+ cadena} ++ agregarcombinaciones(lista.tail,listaCadeneasAconcatenar) 
  }

  def gemerarCombinaciones(cadenaAconcatenar: Seq[Char],tamañodeseado: Int, lista: Seq[Seq[Char]]): Seq[Seq[Char]] = {
    if (tamañodeseado == 0) lista 
    else{
      val accResul = agregarcombinaciones(lista,cadenaAconcatenar)
      gemerarCombinaciones(cadenaAconcatenar,tamañodeseado-1,accResul)
    }   
  }

  def PRC_Ingenuo(): Option[Seq[Char]] = {
    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    val combinaciones = gemerarCombinaciones(alfabeto,n-1,alfabetoenForma)
    combinaciones.find(oraculo)
  }

  def PRC_Mejorado(): Option[Seq[Char]]  = {
    def PRC_MejoradoAux(sck : Seq[Seq[Char]]): Option[Seq[Char]]  = {
      if (sck.isEmpty) None 
      else {
        val combinaciones = agregarcombinaciones(sck,alfabeto).filter(oraculo(_))
        if(combinaciones.filter(_.size == n).size != 0) Some(combinaciones.head)
        else PRC_MejoradoAux(combinaciones) 
      }
    }
    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    PRC_MejoradoAux(alfabetoenForma)
  }

  def combinar(lista1: Seq[Seq[Char]], lista2: Seq[Seq[Char]]): Seq[Seq[Char]] = { 
    //else agregarcombinaciones(lista2,lista1.head) ++ combinar(lista1.tail,lista2)
    if (lista1.isEmpty) Nil 
    else lista2.map{cadena => cadena ++ lista1.head} ++ combinar(lista1.tail,lista2)
  }


  def PRC_turbo(): Option[Seq[Char]]  = {
    def PRC_turboAux(sck : Seq[Seq[Char]]): Option[Seq[Char]]  = {
      if (sck.isEmpty) None 
      else if(sck.head.size >= n) None
      else {
        val combinaciones = combinar(sck,sck).filter(oraculo(_))
        if(combinaciones.filter(_.size == n).size != 0) Some(combinaciones.head)
        else PRC_turboAux(combinaciones) 
      }
    }
    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    PRC_turboAux(alfabetoenForma)
  }

  def filtro_cadenas(s: Seq[Char],sck: Seq[Seq[Char]],k: Int): Boolean = {
    def estaContenido(s: Seq[Char],sck: Seq[Seq[Char]]): Boolean = {
      if(sck.isEmpty) false
      else if(sck.head == s) true
      else estaContenido(s,sck.tail)
    }
    if(s.length == k) estaContenido(s.take(k),sck)
    else {
      val subcadena = s.take(k)
      if(estaContenido(subcadena,sck)) filtro_cadenas(s.tail,sck,k)
      else false
    } 
  }

  def PRC_turbo_mejorada(): Option[Seq[Char]]  = {
    def PRC_turbo_mejoradaAux(sck : Seq[Seq[Char]]): Option[Seq[Char]]  = {
      if (sck.isEmpty) None 
      else if(sck.head.size >= n) None
      else {
        val combinaciones = combinar(sck,sck).filter(filtro_cadenas(_,sck,sck.head.size)).filter(oraculo(_))
        if(combinaciones.filter(_.size == n).size != 0) Some(combinaciones.head)
        else PRC_turbo_mejoradaAux(combinaciones) 
      }
    }
    val alfabetoenForma = alfabeto.map(cadena => Seq(cadena))
    PRC_turbo_mejoradaAux(alfabetoenForma)
  }
}



  
