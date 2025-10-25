import ast.*
import codegen.*
import java.nio.file.Files
import java.nio.file.Paths
import java.lang.reflect.AccessFlag
import codegen.Conversions.given

import scala.util.chaining.scalaUtilChainingOps

import scala.jdk.CollectionConverters._
import io.github.classgraph.HasName

@main def foo =
    val program = Source(
      List(
        // five = 5
        Defn.Value(
          name = "five",
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC, AccessFlag.FINAL),
          decltpe = Some(Type.TypeInt),
          rhs = Term.Literal(Lit.IntLit(5))
        ),

        // add(a: int, b: int) = a + b
        Defn.Method(
          name = "add",
          params = List(
            Param("x", Some(Type.TypeInt)),
            Param("y", Some(Type.TypeInt))
          ),
          decltpe = Some(Type.TypeUnit),
          rhs = Term.Apply(
            Term.Name("identity"),
            List(Term.Name("x"), Term.Name("y"))
          )
        ),
        Defn.Method(
          name = "identity",
          decltpe = Some(Type.TypeUnit),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          params = List(
            Param("a", Some(Type.TypeInt)),
            Param("b", Some(Type.TypeInt))
          ),
          rhs = Term.Block(
            List(
              Defn.Value(
                "apple",
                rhs = Term.Literal(Lit.StringLit("hello")),
                decltpe = Some(Type.TypeString)
              )
            )
          )
        )
      ),
      "Main"
    )

    val newProg =
        Source(
          List(
            Defn.Method(
              name = "foo",
              decltpe = Some(Type.TypeInt),
              rhs = Term.Block(
                List(
                  Defn.Method("x", rhs = Term.Literal(Lit.IntLit(10)), decltpe = Some(Type.TypeInt)),
                  Term.Name("x")
                )
              )
            ),
            Defn.Method(
              name = "bar",
              decltpe = Some(Type.TypeString),
              rhs = Term.Block(
                List(
                  Defn.Method(
                    "y",
                    rhs = Term.Literal(Lit.StringLit("HELLO")),
                    decltpe = Some(Type.TypeString)
                  ),
                  Term.Name("y")
                )
              )
            ),
            Defn.Method(
              name = "baz",
              decltpe = Some(Type.TypeString),
              rhs = Term.Block(
                List(
                  Defn.Method(
                    "z",
                    rhs = Term.Literal(Lit.StringLit("HELLO THIS IS MEEEE")),
                    decltpe = Some(Type.TypeString)
                  ),
                  Term.Name("z")
                )
              )
            )
          ),
          "Main"
        )

    val functionCall = Source(
      List(
        Defn.Value(
          name = "banana",
          decltpe = Some(Type.TypeInt),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          rhs = Term.Apply(
            Term.Name("java.lang.math.Max"),
            List(Term.Literal(Lit.IntLit(52)), Term.Literal(Lit.IntLit(42)))
          )
        )
      ),
      "Main"
    )

    // banana = (handler) => 74
    val lambda = Source(
      List(
        Defn.Value(
          name = "banana",
          decltpe = Some(Type.TypeName("java.util.function.IntUnaryOperator")),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          rhs = Term.Block(
            List(
              Term.Lambda(
                name = None,
                params = List(Param("handler", Some(Type.TypeInt))),
                rhs = Term.Name("handler"),
                returnType = Some(Type.TypeInt)
              )
            )
          )
        )
      )
    )

    val newArray = Source(
      List(
        Defn.Method(
          name = "arrayLiteral",
          decltpe = Some(Type.TypeArray(Type.TypeInt)),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          params = List(Param("apple", Some(Type.TypeArray(Type.TypeInt)))),
          rhs = Term.Block(
            List(
              Term.Literal(
                Lit.ArrayLit(List(Lit.IntLit(1), Lit.IntLit(2), Lit.IntLit(43), Lit.IntLit(4), Lit.IntLit(5)))
              )
            )
          )
        ),
        Defn.Method(
          name = "arrayLiteral",
          decltpe = Some(Type.TypeArray(Type.TypeLong)),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          params = List(Param("apple", Some(Type.TypeArray(Type.TypeLong)))),
          rhs = Term.Block(
            List(
              Term.Literal(
                Lit.ArrayLit(
                  List(Lit.LongLit(1), Lit.LongLit(2), Lit.LongLit(43), Lit.LongLit(4), Lit.LongLit(5))
                )
              )
            )
          )
        )
      )
    )

    val systemOutPrintlnAST: Source = Source(
      statements = List(
        Defn.Method(
          name = "main",
          decltpe = Some(Type.TypeUnit),
          mods = List(AccessFlag.STATIC, AccessFlag.PUBLIC),
          params = List(Param("args", Some(Type.TypeArray(Type.TypeString)))),
          rhs = Term.Block(
            stats = List(
              Term.Apply(
                Term.Select(
                  Term.Select(
                    Term.Name("java.lang.System"), // fully-qualified
                    Term.Name("out")
                  ),
                  Term.Name("println")
                ),
                List(Term.Literal(Lit.StringLit("Hello, World!")))
              )
            )
          )
        )
      ),
      name = "Main"
    )

    val mathMaxCallAST: Source = Source(
      statements = List(
        Defn.Value(
          name = "result",
          decltpe = Some(Type.TypeInt),
          Term.Apply(
            Term.Select(
              Term.Select(
                Term.Select(
                  Term.Name("java"),
                  Term.Name("lang")
                ),
                Term.Name("Math")
              ),
              Term.Name("max")
            ),
            List(
              Term.Literal(Lit.IntLit(34)),
              Term.Literal(Lit.IntLit(33))
            )
          ),
          mods = List(AccessFlag.STATIC, AccessFlag.PUBLIC)
        )
      ),
      name = "Main"
    )

    val bytes = emitCode(newArray)

    // import io.github.classgraph.ClassGraph
    //
    // val scanResult = new ClassGraph()
    //     .enableSystemJarsAndModules()
    //     .scan
    //
    // scanResult.close()

    Files.write(Paths.get("Main.class"), bytes)
