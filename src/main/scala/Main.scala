import ast.*
import codegen.*
import java.nio.file.Files
import java.nio.file.Paths
import java.lang.reflect.AccessFlag

@main def foo =
    val program = Source(
      List(
        // five = 5
        Defn.Method(
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
          decltpe = Some(Type.TypeLong),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          rhs = Term.Block(List(Term.Literal(Lit.LongLit(999L))))
        ),
        Defn.Method(
          name = "identity",
          params = List(Param("a", Some(Type.TypeInt))),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          decltpe = Some(Type.TypeUnit),
          rhs = Term.Block(
            List(
              Defn.Method(
                "temporary",
                decltpe = Some(Type.TypeName("java/util/function/IntUnaryOperator")),
                rhs = Term.Lambda(
                  params = List(Param("handler", Some(Type.TypeInt))),
                  rhs = Term.Name("a"),
                  returnType = Some(Type.TypeInt)
                )
              )
            )
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
          decltpe = Some(Type.TypeName("java/util/function/IntUnaryOperator")),
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
        Defn.Value(
          name = "arrayLiteral",
          decltpe = Some(Type.TypeArray(Type.TypeInt)),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          rhs = Term.Literal(
            Lit.ArrayLit(List(Lit.IntLit(1), Lit.IntLit(2), Lit.IntLit(3), Lit.IntLit(4), Lit.IntLit(5)))
          )
        )
      )
    )

    val bytes = emitCode(newArray)

    Files.write(Paths.get("Main.class"), bytes)
