import ast.*
import codegen.*
import java.nio.file.Files
import java.nio.file.Paths
import java.lang.reflect.AccessFlag

@main def foo =
    val program = Source(
      List(
        // five = 5
        Defn(
          name = "five",
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC, AccessFlag.FINAL),
          decltpe = Some(Type.TypeInt),
          rhs = Term.Literal(Lit.IntLit(5))
        ),

        // add(a: int, b: int) = a + b
        Defn(
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
        Defn(
          name = "identity",
          decltpe = Some(Type.TypeUnit),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          params = List(
            Param("a", Some(Type.TypeInt)),
            Param("b", Some(Type.TypeInt))
          ),
          rhs = Term.Block(
            List(
              Defn(
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
            Defn(
              name = "foo",
              decltpe = Some(Type.TypeInt),
              rhs = Term.Block(
                List(
                  Defn("x", rhs = Term.Literal(Lit.IntLit(10)), decltpe = Some(Type.TypeInt)),
                  Term.Name("x")
                )
              )
            ),
            Defn(
              name = "bar",
              decltpe = Some(Type.TypeString),
              rhs = Term.Block(
                List(
                  Defn(
                    "y",
                    rhs = Term.Literal(Lit.StringLit("HELLO")),
                    decltpe = Some(Type.TypeString)
                  ),
                  Term.Name("y")
                )
              )
            ),
            Defn(
              name = "baz",
              decltpe = Some(Type.TypeString),
              rhs = Term.Block(
                List(
                  Defn(
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
        Defn(
          name = "banana",
          decltpe = Some(Type.TypeInt),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          rhs = Term.Literal(Lit.IntLit(999))
        ),
        Defn(
          name = "identity",
          params = List(Param("a", Some(Type.TypeInt))),
          mods = List(AccessFlag.PUBLIC, AccessFlag.STATIC),
          decltpe = Some(Type.TypeUnit),
          rhs = Term.Block(List(Defn("temporary", Some(Type.TypeInt), rhs = Term.Name("banana"))))
        )
      ),
      "Main"
    )

    val bytes = emitCode(functionCall)

    Files.write(Paths.get("Main.class"), bytes)
