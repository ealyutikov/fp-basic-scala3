ThisBuild / scalacOptions ++= List(
  "-indent",
  "-new-syntax",
  "-Ycheck-all-patmat",
  "-Ycheck-reentrant",
  "-Ycheck-mods",
  "-Yexplicit-nulls",
  "-Yexplain-lowlevel",
  "-Ydebug-type-error"
)
