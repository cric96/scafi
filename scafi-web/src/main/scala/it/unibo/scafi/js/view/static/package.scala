package it.unibo.scafi.js.view

import it.unibo.scafi.js.utils.Debug

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel, JSImport}
package object static {
  //TODO improve this setting
  val CssSettings = scalacss.DevDefaults
  /* all static import for scala.js */
  /*CSS*/
  //code mirror css
  @js.native
  @JSImport("codemirror/lib/codemirror.css", JSImport.Namespace)
  object CodeMirrorStyle extends js.Any
  CodeMirrorStyle
  //bootstrap css style
  @js.native
  @JSImport("bootstrap/dist/css/bootstrap.min.css", JSImport.Namespace)
  object BootstrapCSS extends js.Any
  BootstrapCSS
  //simple bar style
  @js.native
  @JSImport("simplebar/dist/simplebar.css", JSImport.Namespace)
  object SimpleBarCSS extends js.Any
  SimpleBarCSS
  //code mirror javascript style
  @js.native
  @JSImport("codemirror/mode/javascript/javascript.js", JSImport.Namespace)
  object JavascriptStyle extends js.Any
  JavascriptStyle
  /*  JAVASCRIPT LIB */
  //bootstrap and popper
  @js.native
  @JSImport("bootstrap/dist/js/bootstrap.bundle.js", JSImport.Namespace)
  object Bootstrap extends js.Any
  Bootstrap
  //bootstrap util
  @js.native
  @JSImport("bootstrap/js/dist/util.js", JSImport.Namespace)
  object UtilBootstrap extends js.Any
  UtilBootstrap
  //phaser lib
  @js.native
  @JSImport("phaser", JSImport.Namespace)
  object PhaserGlobal extends js.Any //global import phaser
  PhaserGlobal
}