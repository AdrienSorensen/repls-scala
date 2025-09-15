package repls

class IntREPL extends REPLBase {
    type Base = Int
    override val replName: String = "int-repl"

    override def compute(operator: String, left: Int, right: Int): Int = operator match {
        case "+" => left + right
        case "-" => left - right
        case "*" => left * right
    }

    override def checkIfZero(value: Int): Boolean = {

        Math.abs(value) < 1
    }

    override def checkIfOne(value: Int): Boolean = {
        value == 1
    }

    override def isIntegerString(input: String): Boolean = {
        input.matches("-?\\d+")
    }

    override def stringToInteger(str: String): Int = {
        str.toInt
    }

    override def zeroConstant: Int = 0

}