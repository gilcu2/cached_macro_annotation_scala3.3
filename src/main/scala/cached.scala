import scala.annotation.{MacroAnnotation, experimental}
import scala.collection.mutable
import scala.quoted.{Expr, Quotes}

trait Cache[K, V] {
  def put(key: K, value: V): Option[V]

  def get(key: K): Option[V]
}


class MapCache[K, V] extends Cache[K, V] {

  private val map = mutable.Map.empty[K, V]
  override def put(key: K, value: V): Option[V] =
    println(s"put $key $value")
    map.put(key, value)


  override def get(key: K): Option[V] =
    println(s"get $key")
    map.get(key)

}

@experimental
class cached extends MacroAnnotation {

  override def transform(using quotes: Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = {
    import quotes.reflect._

    tree match {
      case DefDef(name, params, returnType, Some(rhs)) =>
        val flattenedParams = params.map(_.params).flatten
        val paramTermRefs = flattenedParams.map(_.asInstanceOf[ValDef].symbol.termRef)
        val paramTuple = Expr.ofTupleFromSeq(paramTermRefs.map(Ident(_).asExpr))

        (paramTuple, rhs.asExpr) match {
          case ('{ $p: paramTupleType }, '{ $r: rhsType }) =>
            val cacheName = Symbol.freshName(name + "Cache")
            val cacheType = TypeRepr.of[Cache[paramTupleType, rhsType]]
            val cacheRhs = '{ new MapCache[paramTupleType, rhsType] }.asTerm
            val cacheSymbol = Symbol.newVal(tree.symbol.owner, cacheName, cacheType, Flags.Private, Symbol.noSymbol)
            val cache = ValDef(cacheSymbol, Some(cacheRhs))
            val cacheRef = Ref(cacheSymbol).asExprOf[Cache[paramTupleType, rhsType]]

            def buildNewRhs(using q: Quotes) = {
              import q.reflect._

              '{
                val key = ${ paramTuple.asExprOf[paramTupleType] }
                $cacheRef.get(key) match {
                  case Some(value) =>
                    println(s"Cached found $key $value")
                    value
                  case None =>
                    println(s"Cached not found $key")
                    val result = ${ rhs.asExprOf[rhsType] }
                    println("Result get")
                    $cacheRef.put(key, result)
                    println(s"Cached $key saved")
                    result
                }
              }
            }

            val newRhs =  buildNewRhs(using tree.symbol.asQuotes).asTerm
            val expandedMethod = DefDef.copy(tree)( name, params, returnType, Some(newRhs))
            List(cache, expandedMethod)
        }

      case _ =>
        println("Not a method")
        report.error("Annottee must be a method")
        List(tree)

    }
  }

}
