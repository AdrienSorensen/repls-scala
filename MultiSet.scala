package repls

import repls.MultiSet.empty

case class MultiSet[T](multiplicity: Map[T, Int]) {

    def *(that: MultiSet[T]): MultiSet[T] = {
        val result = that.multiplicity.keys.flatMap { elem =>
            val count1 = this.multiplicity.getOrElse(elem, 0)
            val count2 = that.multiplicity.getOrElse(elem, 0)
            if (count1 > 0 && count2 > 0) Some(elem -> math.min(count1, count2)) else None
        }.toMap

        MultiSet(result)
    }

    def +(that: MultiSet[T]): MultiSet[T] = {
        val result = (this.multiplicity.keySet ++ that.multiplicity.keySet).map { elem =>
            val count1 = this.multiplicity.getOrElse(elem, 0)
            val count2 = that.multiplicity.getOrElse(elem, 0)
            elem -> (count1 + count2)
        }.toMap

        MultiSet(result)
    }

    def -(that: MultiSet[T]): MultiSet[T] = {
        val result = this.multiplicity.collect {
            case (elem, count1) if count1 > 0 =>
                val count2 = that.multiplicity.getOrElse(elem, 0)
                val remainingCount = count1 - count2
                if (remainingCount > 0) Some(elem -> remainingCount) else None
        }.flatten.toMap

        MultiSet(result) // return the resulting multiset
    }


    def toSeq: Seq[T] = {
        multiplicity.toSeq.flatMap { case (elem, count) => Seq.fill(count)(elem) } // create a sequence with duplicated elements
    }

    val MaxCountForDuplicatePrint = 5


    override def toString: String = {
        def elemToString(elem: T): String = {
            val count = multiplicity(elem)
            if (count >= MaxCountForDuplicatePrint)
                elem.toString + " -> " + count.toString
            else Seq.fill(count)(elem).mkString(",")
        }
        val keyStringSet = multiplicity.keySet.map(elemToString)
        "{" + keyStringSet.toSeq.sorted.mkString(",") + "}"
    }
}

object MultiSet {
    def empty[T]: MultiSet[T] = MultiSet(Map[T, Int]())

    def apply[T](elements: Seq[T]): MultiSet[T] = {
        val counts = elements.groupBy(identity).view.mapValues(_.size).toMap // create a count map
        MultiSet(counts) // and return the MultiSet
    }
}
