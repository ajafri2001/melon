package ast

import codegen.LambdaSyntheticMethodName

def nameLambdas(stat: Stat): Stat =

    lazy val nameLambdasInTerm: Term => Term = (term: Term) =>
        term match
            case Term.Lambda(_, params, rhs, retType) =>
                Term.Lambda(
                  name = Some(LambdaSyntheticMethodName.getName),
                  params = params,
                  rhs = nameLambdasInTerm(rhs),
                  returnType = retType
                )

            case Term.Block(stats) =>
                Term.Block(stats.map {
                    case s: Stat => nameLambdas(s)
                    case t: Term => nameLambdasInTerm(t)
                })

            case Term.Apply(qn, args) =>
                Term.Apply(qn, args.map(nameLambdasInTerm))

            case other => other

    stat match
        case defn @ Defn(_, _, rhs, _, _) =>
            defn.copy(rhs = nameLambdasInTerm(rhs))
