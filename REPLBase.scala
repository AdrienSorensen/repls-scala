package repls

import scala.collection.mutable

/*
    The parent class of IntREPL and MultiSetREPL.
 */
abstract class REPLBase extends REPL {
    type Base

    val variables: mutable.Map[String, Base] = mutable.Map()

    // abstract methods to be implemented by child classes.
    def checkIfZero(value: Base): Boolean
    def checkIfOne(value: Base): Boolean
    def isIntegerString(element: String): Boolean
    def stringToInteger(element: String): Base
    def zeroConstant: Base
    def compute(opName: String, lhs: Base, rhs: Base): Base


    def variableAssignment(variable: String, expression: String): String = {
        val assignmentResult = {
            val parsedExpression = expressionTree(expression) //get the expression tree
            val evaluatedValue = evaluation(parsedExpression) //evaluate expression

            variables(variable) = evaluatedValue
            evaluatedValue
        }

        s"$variable = $assignmentResult" //return the assignment result as a string.
    }


    def readEval(command: String): String = {
        val elements = command.split("\\s+").toList

        elements match {
            case variable :: "=" :: expressionParts =>
                val expression = expressionParts.mkString(" ") // we jion remaining parts for the assignment expression
                variableAssignment(variable, expression)

            case "@" :: expressionParts =>
                val expression = expressionParts.mkString(" ") // we join parts for simplification
                val parsedExpression = expressionTree(expression)
                val simplifiedExpression = simplification(parsedExpression).toString
                simplifiedExpression

            case _ =>
                val parsedExpression = expressionTree(command)
                evaluation(parsedExpression).toString // evaluate the parsed expression and return the result
        }
    }

    // normal mathematcial precedence
    val precedence: Map[String, Int] = Map(
        "+" -> 1,
        "-" -> 1,
        "*" -> 2,
    )


    abstract class Expression { //i know atze said traits were recommended better than a abstract class but i'm more familiar with abstract classes
        def toString: String
    }

    case class Constant(value: Base) extends Expression {
        override def toString: String = value.toString
    }

    case class Variable(variableName: String) extends Expression { // not applied in Atze's code, well needed
        override def toString: String = variableName
    }

    case class Operator(lhs: Expression, operator: String, rhs: Expression) extends Expression {
        override def toString: String = {
            def isEmptySet(expr: Expression): Boolean = expr match {
                case Constant(value: MultiSet[_]) => value.multiplicity.isEmpty
                case _ => false
            }

            //helper function for determining precedence in the abstract class
            def precedence(op: String): Int = op match {
                case "+" | "-" => 1
                case "*" => 2
                case _ => 0
            }

            (lhs, operator, rhs) match {
                case (Operator(_, leftOp, _), middleOp, Operator(_, rightOp, _)) if
                  precedence(leftOp) == 1 && precedence(middleOp) == 2 && precedence(rightOp) == 2 =>
                    s"(${lhs.toString}) $middleOp ${rhs.toString}"

                case (Operator(_, leftOp, _), middleOp, Operator(_, rightOp, _)) if
                  precedence(leftOp) == 1 && precedence(middleOp) == 2 && precedence(rightOp) == 1 =>
                    s"( ${lhs.toString} ) $middleOp ( ${rhs.toString} )"

                case (Operator(_, leftOp, _), middleOp, Operator(_, rightOp, _)) if
                  precedence(leftOp) == 2 && precedence(middleOp) == 2 && precedence(rightOp) == 1 =>
                    s"${lhs.toString} $middleOp (${rhs.toString})"

                case (_, middleOp, Operator(_, rightOp, _)) if
                  precedence(middleOp) == 2 && precedence(rightOp) == 1 =>
                    s"${lhs.toString} $middleOp ( ${rhs.toString} )"

                case (Operator(_, rightOp, _), middleOp, _) if
                  precedence(middleOp) == 2 && precedence(rightOp) == 1 =>
                    s"( ${lhs.toString} ) $middleOp ${rhs.toString}"

                case (left, "*", right) if isEmptySet(left) || isEmptySet(right) => "{}" //case for multiplication with "{}".
                case (left, "-", right) if left == right => "{}" //case for subtraction of the same expression.

                case _ => s"${lhs.toString} $operator ${rhs.toString}"
            }
        }
    }



    def simplification(expression: Expression): Expression = {
        expression match {

            //handle case where a constant is multiplied by itself or by another constant
            case Operator(Constant(lhs), "*", Constant(rhs)) =>
                Constant(compute("*", lhs, rhs)) //compute the result of the constant multiplication

            case Operator(left, "*", right) if left == right =>
                left //handle intersection by itself

            case Operator(Operator(l1, "+", l2), "*", Operator(r1, "+", r2)) if l1 == r1 && l2 == r2 =>
                Operator(l1, "+", l2) //handle composite case


            case Operator(left, operator, right) =>
                simplifyOperator(left, operator, right)

            case Variable(variableName) =>
                resolveVariable(variableName)

            // base case
            case other => other
        }
    }

    //helper function to simplify operators
    private def simplifyOperator(left: Expression, operator: String, right: Expression): Expression = {
        val simplifiedLeft = simplification(left)
        val simplifiedRight = simplification(right)

        (simplifiedLeft, operator, simplifiedRight) match {
            //0 add
            case (lhs, "+", Constant(zero)) if checkIfZero(zero) => lhs
            case (Constant(zero), "+", rhs) if checkIfZero(zero) => rhs
            //0 mulitply
            case (Constant(zero), "*", _) if checkIfZero(zero) => Constant(zero)
            case (_, "*", Constant(zero)) if checkIfZero(zero) => Constant(zero)
            //math id rules
            case (lhs, "*", Constant(one)) if checkIfOne(one) => lhs
            case (Constant(one), "*", rhs) if checkIfOne(one) => rhs
            //sub same
            case (lhs, "-", rhs) if lhs == rhs => Constant(zeroConstant)

            //distri rules
            case (Operator(left1, "*", right1), "+", Operator(left2, "*", right2)) if left1 == left2 =>
                Operator(left1, "*", simplification(Operator(right1, "+", right2)))
            case (Operator(right1, "*", left1), "+", Operator(left2, "*", right2)) if left1 == left2 =>
                Operator(left1, "*", simplification(Operator(right1, "+", right2)))
            case (Operator(left1, "*", right1), "+", Operator(right2, "*", left2)) if left1 == left2 =>
                Operator(left1, "*", simplification(Operator(right1, "+", right2)))
            case (Operator(right1, "*", left1), "+", Operator(right2, "*", left2)) if left1 == left2 =>
                Operator(left1, "*", simplification(Operator(right1, "+", right2)))



            //constant calculations
            case (Constant(lhs), op, Constant(rhs)) =>
                Constant(compute(op, lhs, rhs))

            case _ => Operator(simplifiedLeft, operator, simplifiedRight)
        }
    }

    // helper function to resolve variable values
    private def resolveVariable(variableName: String): Expression = {
        variables.get(variableName) match {
            case Some(value) => Constant(value) // we resolve variable to its value
            case None => Variable(variableName) // and keep it if not defined
        }
    }

    def expressionTree(expression: String): Expression = {
        val polishNotation = shuntingYardAlgorithm(expression)
        reversePolishToExpression(polishNotation)
    }

    def shuntingYardAlgorithm(expression: String): List[String] = {
        val output = mutable.ListBuffer[String]()
        val operatorStack = mutable.Stack[String]()
        val tokens = expression.split("\\s+") //split expression by spaces.

        tokens.foreach {
            case token if isOperand(token) => output += token
            case "(" => operatorStack.push("(")
            case ")" => handleClosingParenthesis(operatorStack, output)
            case token if isOperator(token) => handleOperator(token, operatorStack, output)
        }

        while (operatorStack.nonEmpty) {
            output += operatorStack.pop() //pop all remaining operators from the stack to the output.
        }
        output.toList
    }

    //helper function to check if the token is an operand.
    private def isOperand(token: String): Boolean =
        token.matches("-?\\d+") || isIntegerString(token) || token.matches("[a-zA-Z]+")

    //helper function to check if the token is an operator.
    private def isOperator(token: String): Boolean =
        precedence.contains(token)

    //helper function to handle closing parenthesis in the Shunting Yard algorithm.
    private def handleClosingParenthesis(stack: mutable.Stack[String], output: mutable.ListBuffer[String]): Unit = {
        while (stack.nonEmpty && stack.top != "(") {
            output += stack.pop() // Pop operators until left parenthesis.
        }
        if (stack.nonEmpty) stack.pop() // Discard the left parenthesis.
    }

    //helper function to handle operators in the Shunting Yard algorithm.
    private def handleOperator(token: String, stack: mutable.Stack[String], output: mutable.ListBuffer[String]): Unit = {
        while (stack.nonEmpty && isOperator(stack.top) && (precedence(token) <= precedence(stack.top))) {
            output += stack.pop()
        }
        stack.push(token)
    }

    //changed up atze's code a tiny bit but mostly same logic
    def reversePolishToExpression(tokens: List[String]): Expression = {
        val stack = mutable.Stack[Expression]()

        for (token <- tokens) {
            if (isIntegerString(token)) {
                stack.push(Constant(stringToInteger(token)))
            } else if (isOperator(token)) {
                val rhs = stack.pop()
                val lhs = stack.pop()
                stack.push(Operator(lhs, token, rhs))
            } else {
                stack.push(Variable(token))
            }
        }
        stack.top
    }


    def evaluation(expression: Expression): Base = {
        expression match {
            case Constant(value) =>
                value

            case Variable(name) =>
                variables.get(name) match {
                    case Some(value) => value //return the value of the variable.
                }

            case Operator(lhs, operator, rhs) => //recursively evaluate left-hand side and right-hand side.
                val (lhsEvaluated, rhsEvaluated) = (evaluation(lhs), evaluation(rhs))
                compute(operator, lhsEvaluated, rhsEvaluated)
        }
    }

}
