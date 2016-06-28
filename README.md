# Bone

This program depends on Java 8. To build the compiler plugin, run the command:
```bash
  sbt assembly
```

The resulting jar file will be located at `target/scala-2.11/metric-assembly-1.0.jar`.

To build and run the compiler plugin on its own source code, run the command:
```bash
  ./bootstrap
```

To run the compiler plugin on another project (hereafter called the target project), run `sbt` in interactive mode and enter the following commands:
```sbt
  clean
  set scalacOptions in ThisBuild ++= Seq("-Xplugin:metric-assembly-1.0.jar")
  compile
```

Make sure that the compiler plugin `metric-assembly-1.0.jar` can be found (for example, by copying it to the root of the target project, or by specifying the full path in the above command). These commands are also listed in the file `sbt_commands` and can be passed to `sbt` easily using the following command:
```bash
  cat sbt_commands | sbt
```
