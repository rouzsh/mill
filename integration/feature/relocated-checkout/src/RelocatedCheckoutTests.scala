package mill.integration

import mill.testkit.{IntegrationTester, UtestIntegrationTestSuite}
import utest.*

/**
 * Checks that a built checkout keeps working after it is moved to another folder, together
 * with its `out/` folder.
 */
object RelocatedCheckoutTests extends UtestIntegrationTestSuite {
  // The two checkouts must not share an `out/` folder
  override def allowSharedOutputDir: Boolean = false

  // Moves the checkout at `from` into `tester`'s workspace, like `mv`: files are copied with
  // their timestamps, then the original build and sources are deleted. The daemon and lock
  // files are skipped, since the first run's daemon may still hold them.
  def moveCheckout(from: os.Path, tester: IntegrationTester): Unit = {
    val skip = Set("mill-daemon", "mill-no-daemon", "mill-out-lock")
    os.remove.all(tester.workspacePath)
    for (p <- os.walk(from, skip = p => skip(p.last)) if !os.isDir(p))
      os.copy(
        p,
        tester.workspacePath / p.relativeTo(from),
        copyAttributes = true,
        createFolders = true
      )
    for (p <- os.list(from) if p.last != "out") os.remove.all(p)
  }

  // Builds a checkout, moves it, edits a source in the new folder and checks that the edited
  // code runs
  def checkMove(
      setup: os.Path => Unit = _ => (),
      afterMove: IntegrationTester => Unit = _ => ()
  ): Unit = {
    val first = integrationTest { tester =>
      setup(tester.workspacePath)
      assert(tester.eval("run").out.contains("hello"))
      tester.workspacePath
    }
    integrationTest { tester =>
      moveCheckout(first, tester)
      tester.modifyFile(
        tester.workspacePath / "src/app/App.scala",
        _.replace("\"hello\"", "\"moved\"")
      )
      val res = tester.eval("run")
      assert(res.isSuccess)
      assert(res.out.contains("moved"))
      afterMove(tester)
    }
  }

  // Builds a checkout of single-file scripts without a build file, checks it, moves it and
  // checks it again in the new folder. The scripts' cached results are reused after the move,
  // so tasks that store a path as text must notice the move themselves.
  def checkScriptsMove(scripts: (String, String)*)(check: IntegrationTester => Unit): Unit = {
    val first = integrationTest { tester =>
      os.remove(tester.workspacePath / "build.mill.yaml")
      os.remove.all(tester.workspacePath / "src")
      for ((name, code) <- scripts) os.write(tester.workspacePath / name, code)
      check(tester)
      tester.workspacePath
    }
    integrationTest { tester =>
      moveCheckout(first, tester)
      check(tester)
    }
  }

  val tests: Tests = Tests {
    // A YAML build looks up its settings by the build file's path
    test("yamlBuild") - checkMove()
    // `build.mill` code sees its own file name through `sourcecode.File`
    test("millBuild") - checkMove(
      setup = ws => {
        os.remove(ws / "build.mill.yaml")
        os.write(
          ws / "build.mill",
          """package build
            |import mill.*, scalalib.*
            |object `package` extends ScalaModule {
            |  def scalaVersion = "3.8.2"
            |  def buildFile = Task.Input { summon[sourcecode.File].value }
            |}
            |""".stripMargin
        )
      },
      afterMove = tester => {
        // `buildFile` is an input, so this checks the compiled build code, not a cached value
        val res = tester.eval(("show", "buildFile"))
        assert(res.isSuccess)
        assert(os.Path(ujson.read(res.out).str) == tester.workspacePath / "build.mill")
      }
    )
    // `MILL_WORKSPACE_ROOT` is passed to the forked JVM, and the launcher holds absolute
    // classpath entries
    test("javaScript") - checkScriptsMove(
      "Foo.java" ->
        "public class Foo { public static void main(String[] a) { System.out.println(System.getenv(\"MILL_WORKSPACE_ROOT\")); } }"
    ) { tester =>
      val ws = tester.workspacePath
      assert(os.Path(tester.eval("Foo.java").out.trim) == ws)
      assert(tester.eval(("show", "Foo.java:launcher")).isSuccess)
      val launcher =
        ws / "out/Foo.java/launcher.dest" / (if (scala.util.Properties.isWin) "run.bat" else "run")
      assert(os.read(launcher).contains((ws / "out").toString))
    }
    // A Scala script sees its own file name through `sourcecode.File`, and Scala 3 compiles
    // with `-sourceroot <workspace>`
    test("scalaScript") - checkScriptsMove(
      "Foo.scala" -> "def main(args: Array[String]) = println(summon[sourcecode.File].value)"
    ) { tester =>
      val ws = tester.workspacePath
      assert(os.Path(tester.eval("Foo.scala").out.trim) == ws / "Foo.scala")
      val options = ujson.read(tester.eval(("show", "Foo.scala:allScalacOptions")).out)
      assert(options.arr.map(_.str).contains(ws.toString))
    }
    // A Kotlin test script reads `internal` members of the script it tests through
    // `-Xfriend-paths`, which holds absolute paths
    test("kotlinScriptTest") - checkScriptsMove(
      "Bar.kt" -> "internal fun secret() = \"secret\"\nfun main() = println(secret())",
      "BarTests.kt" ->
        "//| extends: [mill.script.KotlinModule.Junit5]\n//| moduleDeps: [Bar.kt]\nclass BarTests { fun check() = secret() }"
    ) { tester =>
      // Edited each time, so the test script recompiles with the current options
      tester.modifyFile(tester.workspacePath / "BarTests.kt", _ + "\n")
      assert(tester.eval("BarTests.kt:compile").isSuccess)
    }
  }
}
