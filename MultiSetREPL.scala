package repls

import scala.collection.mutable.ListBuffer

class MultiSetREPL extends REPLBase {
    override type Base = MultiSet[String]
    override val replName: String = "multiset-repl"

    override def zeroConstant: MultiSet[String] = MultiSet.empty[String]
    override def checkIfZero(value: MultiSet[String]): Boolean = value.multiplicity.isEmpty // we check if MultiSet is empty
    override def checkIfOne(value: MultiSet[String]): Boolean = false

    override def stringToInteger(input: String): MultiSet[String] = {
        val elements = ListBuffer[String]()
        val trimmedInput = input.trim

        // we check if the input is a valid MultiSet format
        if (!isIntegerString(trimmedInput)) throw new IllegalArgumentException("Invalid MultiSet format")

        val currentElement = new StringBuilder
        var insideSet = false

        for (char <- trimmedInput) {
            char match {
                case '{' =>
                    insideSet = true // start of MultiSet
                case '}' =>
                    if (currentElement.nonEmpty) {
                        elements += currentElement.toString
                        currentElement.clear()
                    }
                    insideSet = false // end
                case ',' if insideSet =>
                    if (currentElement.nonEmpty) {
                        elements += currentElement.toString
                        currentElement.clear()
                    }
                case _ if insideSet =>
                    currentElement.append(char)
                case _ => // we ignore characters outside braces
            }
        }

        MultiSet(elements.toList) // create and return multiSet from collected elements
    }


    override def isIntegerString(input: String): Boolean =
        input.startsWith("{") && input.endsWith("}") // validate multiSet format


    def compute(opName: String, lhs: MultiSet[String], rhs: MultiSet[String]): MultiSet[String] = opName match {
        case "+" => lhs + rhs // summation
        case "-" => lhs - rhs // subtraction
        case "*" => lhs * rhs // intersection
    }

}
