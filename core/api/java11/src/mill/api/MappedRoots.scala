package mill.api

import mill.api.internal.NamedParameterOnlyDummy
import mill.constants.PathVars

import scala.annotation.unused
import scala.util.DynamicVariable

type MappedRoots = Seq[(key: String, path: os.Path)]

object MappedRoots extends MappedRootsImpl

trait MappedRootsImpl {

  private val rootMapping: DynamicVariable[MappedRoots] = DynamicVariable(Seq())

  def get: MappedRoots = rootMapping.value

  def toMap: Map[String, os.Path] = get.map(m => (m.key, m.path)).toMap

  def withMillDefaults[T](
      @unused t: NamedParameterOnlyDummy = new NamedParameterOnlyDummy,
      outPath: os.Path,
      workspacePath: os.Path = BuildCtx.workspaceRoot,
      homePath: os.Path = os.home
  )(thunk: => T): T = withMapping(
    (
      Seq(("MILL_OUT", outPath)) ++
        // The Coursier caches get their own keys, so paths into them don't depend on where the
        // cache lives: `COURSIER_CACHE`, or OS-specific defaults like `~/Library/Caches/Coursier`
        // (macOS) vs `~/.cache/coursier` (Linux).
        CoursierRoots.roots.filter { case (_, p) =>
          p != outPath && p != workspacePath && p != homePath
        } ++
        Seq(
          ("WORKSPACE", workspacePath),
          ("HOME", homePath)
        )
    )
      // Most specific first, whatever the layout, e.g. a `COURSIER_CACHE` inside the workspace
      // or an `--out` folder outside it. Stable, so the usual order is kept for equal depths.
      .sortBy { case (_, p) => -p.segmentCount }
  )(thunk)

  private def logMapping(): Unit = {
    if (sys.env.getOrElse("MILL_LOG_DIRS", "0") == "1")
      System.err.println(s"Mapping: ${rootMapping.value}")
  }

  def withMapping[T](mapping: MappedRoots)(thunk: => T): T = withMapping(_ => mapping)(thunk)

  def withMapping[T](mapping: MappedRoots => MappedRoots)(thunk: => T): T = {
    val newMapping = mapping(rootMapping.value)
    val changed = rootMapping.value != newMapping
    if (changed) {
      var seenKeys = Set[String]()
      var seenPaths = Set[os.Path]()
      newMapping.foreach { case m =>
        require(!m.key.startsWith("$"), "Key must not start with a `$`.")
        require(m.key != PathVars.ROOT, s"Invalid key, '${PathVars.ROOT}' is a reserved key name.")
        require(
          !seenKeys.contains(m.key),
          s"Key must be unique, but '${m.key}' was given multiple times."
        )
        require(
          !seenPaths.contains(m.path),
          s"Paths must be unique, but '${m.path}' was given multiple times."
        )
        seenKeys += m.key
        seenPaths += m.path
      }
      try rootMapping.withValue(newMapping) {
          logMapping()
          thunk
        }
      finally logMapping()
    } else {
      thunk
    }
  }

  def encodeKnownRootsInPath(p: os.Path): String = {
    MappedRoots.get.collectFirst {
      case rep if p.startsWith(rep.path) =>
        s"$$${rep.key}${
            if (p != rep.path) {
              s"/${p.subRelativeTo(rep.path).toString()}"
            } else ""
          }"
    }.getOrElse(p.toString)
  }

  def decodeKnownRootsInPath(encoded: String): String = {
    if (encoded.startsWith("$")) {
      val offset = 1 // "$".length
      MappedRoots.get.collectFirst {
        case mapping if encoded.startsWith(mapping.key, offset) =>
          s"${mapping.path.toString}${encoded.substring(mapping.key.length + offset)}"
      }.getOrElse(encoded)
    } else {
      encoded
    }
  }

  /**
   * Use this to assert at runtime, that a root path with the given `key` is defined.
   * @throws NoSuchElementException when no path was mapped under the given `key`.
   */
  def requireMappedPaths(key: String*): Unit = {
    val map = toMap
    for {
      singleKey <- key
    } {
      if (!map.contains(singleKey))
        throw new NoSuchElementException(s"No root path mapping defined for '${key}'")
    }
  }

}

/** The Coursier caches' locations, looked up once per JVM. */
private object CoursierRoots {
  lazy val roots: Seq[(String, os.Path)] = {
    def root(key: String, dir: => java.nio.file.Path) =
      // Not only non-fatal errors: on Windows, a missing native helper is a `LinkageError`.
      try Some(key -> os.Path(dir.toAbsolutePath))
      catch { case scala.util.control.NonFatal(_) | _: LinkageError => None }
    Seq(
      root(PathVars.COURSIER_CACHE, coursier.paths.CoursierPaths.cacheDirectoryPath()),
      root(
        PathVars.COURSIER_ARCHIVE_CACHE,
        coursier.paths.CoursierPaths.archiveCacheDirectoryPath()
      )
    ).flatten.distinctBy(_._2)
  }
}
