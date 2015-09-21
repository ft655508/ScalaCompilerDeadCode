package localhost

import scala.reflect.runtime.universe._

import scala.tools.nsc
import scala.compat.Platform.EOL
import scala.language.postfixOps
import scala.tools.nsc.ast.Printers
import scala.tools.nsc.transform.Transform
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.reflect.api.Trees
import scala.reflect.runtime.universe._
import java.lang.String
import java.io._
import scala._
import scala.Predef

class OutputLineFile(val global:Global) extends Plugin {
      import global._
      val name = "outputlinefile"
      val description = "My OutputLineFile"
      val components = List[PluginComponent](Component) 

      private object Component extends PluginComponent {
        override def toString = ""
          val global:OutputLineFile.this.global.type = OutputLineFile.this.global
          val runsAfter = List[String]("typer");
          
          val phaseName = OutputLineFile.this.name
          def newPhase(_prev: Phase) = new OutputLineFilePhase(_prev)
          def newTransformer(unit:CompilationUnit) = new MyTransformer
          
          /*
           * The number of "println"s we insert
           */
          var lineCounter : Int = 0
          
          class MyTransformer extends Transformer {
            
            
            /*
             * make a "println" statement with the given source file name and line number
             */
            def generatePrintln(source : String, line : Int) : Tree = { 
              lineCounter = lineCounter + 1
              val liter = Literal(Constant(source.split(",")(0) + " " + line + "\n"))
              liter.setType(typeOf[String])              
              val newPrint: Tree = gen.mkMethodCall(definitions.PredefModule.info.decl(newTermName("print")), List(liter)) 
              newPrint.setType(typeOf[Unit])
              newPrint
            }            
              
              /*
               * The transformation function, which transforms the original AST. 
               * Call a recursive function "helpTransform" to do the work.
               */
              override def transform(tree : Tree) : Tree = helpTransform(tree, Nil)
              
            /*
             * The most important part for the plugin. "helpTransform" inserts "println"s into different classes of ASTs
             * recursively, using treeCopy method to create new trees with "println"s. In the match-cases block, "helpTransform"
             * transforms ASTs which are main components of a source file.
             */
              def helpTransform(tree : Tree, printList : List[Tree]) : Tree = {    
                  tree match {       
                    case p @ PackageDef(pid, stats) => {   
                      /*
                       * 4 files are excluded from transformation, because they are used in the definition of "println" itself.
                       * So insert "println" statements into these files can cause infinite recursion or initial exception.
                       */
                      if(p.pos.focus.toString.lastIndexOf("library/scala/io/AnsiColor.scala") == -1   
                      && p.pos.focus.toString.lastIndexOf("library/scala/Console.scala") == -1
                      && p.pos.focus.toString.lastIndexOf("library/scala/Predef.scala") == -1
                      && p.pos.focus.toString.lastIndexOf("library/scala/util/DynamicVariable.scala") == -1)
                      {                                                                                                 
                        val PackagePos = generatePrintln(p.pos.focus.toString, p.pos.focus.line)
                          val newStats = p.stats.foldLeft(List[Tree]())((list, i) => list ::: List(helpTransform(i, PackagePos :: printList)))
                          treeCopy.PackageDef(p, p.pid, newStats)    
                      }   
                      else p
                    }
                    
                    case m @ ModuleDef(mods, name, impl) => {
                        val ClassDeclPos = (m.pos.focusStart.line to m.pos.focusEnd.line).toList.foldLeft(List[Tree]())((list, i) => 
                        generatePrintln(m.pos.focus.toString, i) :: list)
                        val ModuleValPos = m.impl.body.foldLeft(List[Tree]())((list, i) => {
                            val newPrint = i match {
                            case v @ ValDef(mods, name, tpt, rhs) => {
                              (v.pos.focusStart.line to v.pos.focusEnd.line).toList.foldLeft(List[Tree]())((list, i) => 
                                generatePrintln(v.pos.focus.toString, i) :: list)
                            }
                            case t => Nil
                          }
                          newPrint ::: list  
                          })
                        val newStats = m.impl.body.foldLeft(List[Tree]())((list, i) => list ::: List(helpTransform(i, ClassDeclPos ::: printList)))
                        
                      treeCopy.ModuleDef(m, m.mods, m.name, 
                          treeCopy.Template(m.impl, m.impl.parents, m.impl.self, newStats ::: ModuleValPos))      
                    }
                    
                    case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                       if(name.toString != "<init>" && !mods.isLazy){   
                         rhs match {
                           case l @ Literal(value) => {
                            val newRhs = 
                            treeCopy.Block(l, generatePrintln(l.pos.focus.toString, l.pos.focus.line) :: 
                                generatePrintln(d.pos.focus.toString, d.pos.focus.line) :: printList, l)
                            treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt, newRhs)
                           }
                           case s @ Select(qualifier, selector) => {
                             val newRhs = 
                             treeCopy.Block(s, generatePrintln(s.pos.focus.toString, s.pos.focus.line) :: 
                                 generatePrintln(d.pos.focus.toString, d.pos.focus.line) :: printList, s)
                             treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt, newRhs)
                           }
                           case i @ Ident(name) => {
                             val newRhs =
                             treeCopy.Block(i, generatePrintln(i.pos.focus.toString, i.pos.focus.line) :: 
                                 generatePrintln(d.pos.focus.toString, d.pos.focus.line) :: printList, i)
                             treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt, newRhs)
                           }
                           case th @ This(qual) => {
                             val newRhs =
                             treeCopy.Block(th, generatePrintln(th.pos.focus.toString, th.pos.focus.line) :: 
                                 generatePrintln(d.pos.focus.toString, d.pos.focus.line) :: printList, th)
                             treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt, newRhs)
                           }
                           case t => treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt, 
                               helpTransform(rhs, generatePrintln(d.pos.focus.toString, d.pos.focus.line) :: printList))                                                                                      
                        }
                       }
                       else d                 
                    }
                    
                    case a @ Apply(fun, args) => {
                        val ApplyPos = (a.pos.focusStart.line to a.pos.focusEnd.line).toList.foldLeft(List[Tree]())(
                            (list, i) => generatePrintln(a.pos.focus.toString, i) :: list)
                        val newArgs = a.args.foldLeft(List[Tree]())((list, i) => list ::: List(transform(i)))
                        treeCopy.Block(a, ApplyPos ::: printList, treeCopy.Apply(a, a.fun, newArgs))                 
                    }
                    
                    case b @ Block(stats, expr) => {
                        val newStats = b.stats.foldLeft(List[Tree]())((list, i) => list ::: {
                              i match {
                                case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => List(transform(i))
                                case t => List(transform(i), generatePrintln(i.pos.focus.toString, i.pos.focus.line)) 
                              }  
                        }
                       )
                        treeCopy.Block(b, 
                            List(generatePrintln(b.pos.focus.toString, b.pos.focus.line)) ::: printList ::: newStats ::: List(generatePrintln(b.expr.pos.focus.toString, b.expr.pos.focus.line)), 
                              transform(b.expr))
                    } 
                                                             
                    case r @ Return(expr) => {
                        treeCopy.Block(r, printList ::: List(generatePrintln(r.pos.focus.toString, r.pos.focus.line), generatePrintln(expr.pos.focus.toString, expr.pos.focus.line)),
                            treeCopy.Return(r, transform(r.expr)))
                    }
                          
                    case v @ ValDef(mods, name, tpt, rhs) => {              
                      val ValPos = (v.pos.focusStart.line to v.pos.focusEnd.line).toList.foldLeft(List[Tree]())((list, i) =>
                        generatePrintln(v.pos.focus.toString, i) :: list)
                       treeCopy.ValDef(v, mods, name, tpt,helpTransform(rhs, ValPos ::: printList))                       
                    }
                    
                    case ite @ If(cond, thenp, elsep) => {                          
                        val newThenp = 
                        thenp match {
                          case l @ Literal(value) => treeCopy.Block(l, 
                              generatePrintln(l.pos.focus.toString, l.pos.focus.line) ::
                              generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: printList, l)
                          case s @ Select(qualifier, selector) => treeCopy.Block(s, 
                              generatePrintln(s.pos.focus.toString, s.pos.focus.line) ::
                              generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: printList, s) 
                          case i @ Ident(name) => treeCopy.Block(i, 
                              generatePrintln(i.pos.focus.toString, i.pos.focus.line) ::
                              generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: printList, i)
                           case th @ This(qual) => treeCopy.Block(th, 
                              generatePrintln(th.pos.focus.toString, th.pos.focus.line) ::
                              generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: printList, th)
                           case t => helpTransform(thenp, generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: 
                               generatePrintln(thenp.pos.focus.toString, thenp.pos.focus.line) :: printList)                                                                             
                        }
                        val newElsep = 
                        elsep match {
                          case l @ Literal(value) => treeCopy.Block(l, 
                              generatePrintln(l.pos.focus.toString, l.pos.focus.line) ::
                              generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: printList, l)
                          case s @ Select(qualifier, selector) => treeCopy.Block(s, 
                              generatePrintln(s.pos.focus.toString, s.pos.focus.line) ::
                              generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: printList, s)  
                          case i @ Ident(name) => treeCopy.Block(i, 
                              generatePrintln(i.pos.focus.toString, i.pos.focus.line) ::
                              generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: printList, i)
                           case th @ This(qual) => treeCopy.Block(th, 
                              generatePrintln(th.pos.focus.toString, th.pos.focus.line) ::
                              generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: printList, th)
                           case t => helpTransform(elsep, generatePrintln(ite.pos.focus.toString, ite.pos.focus.line) :: 
                               generatePrintln(elsep.pos.focus.toString, elsep.pos.focus.line) :: printList)                                                                             
                        }
                        treeCopy.If(ite, cond, newThenp, newElsep)
                    }
                                                          
                    case s @ Select(qualifier, selector) => {                    
                        treeCopy.Select(s, helpTransform(s.qualifier, generatePrintln(s.pos.focus.toString, s.pos.focus.line) :: printList), selector)
                    }                   
                  
                    case m @ Match(selector, cases) => {      
                      val newCases = cases.foldLeft(List[CaseDef]())((list, i) => 
                        list ::: List(treeCopy.CaseDef(i, i.pat, 
                             i.guard, treeCopy.Block(i.body, 
                                 generatePrintln(i.pos.focus.toString, i.pos.focus.line) ::
                                 generatePrintln(m.pos.focus.toString, m.pos.focus.line) :: 
                                 generatePrintln(selector.pos.focus.toString, selector.pos.focus.line) ::
                                 generatePrintln(i.body.pos.focus.toString, i.body.pos.focus.line) ::
                                 printList, transform(i.body)))))
                      treeCopy.Match(m, m.selector, newCases)
                    }
                    
                   case n @ New(tpt) => {                    
                      treeCopy.New(n, helpTransform(tpt, generatePrintln(n.pos.focus.toString, n.pos.focus.line) :: printList))
                    }
                    
                    case a @ Assign(lhs, rhs) => {
                      rhs match {
                           case l @ Literal(value) => {
                            val newRhs = 
                            treeCopy.Block(l, generatePrintln(l.pos.focus.toString, l.pos.focus.line) :: printList, l)
                            treeCopy.Assign(a, lhs, newRhs)
                           }
                           case s @ Select(qualifier, selector) => {
                             val newRhs =
                             treeCopy.Block(s, generatePrintln(s.pos.focus.toString, s.pos.focus.line) :: printList, s)
                             treeCopy.Assign(a, lhs, newRhs)
                           }
                           case i @ Ident(name) => {
                             val newRhs = 
                             treeCopy.Block(i, generatePrintln(i.pos.focus.toString, i.pos.focus.line) :: printList, i)
                             treeCopy.Assign(a, lhs, newRhs)
                           }
                           case th @ This(qual) => {
                             val newRhs =
                             treeCopy.Block(th, generatePrintln(th.pos.focus.toString, th.pos.focus.line) :: printList, th)
                             treeCopy.Assign(a, lhs, newRhs)
                           }
                           case t => treeCopy.Assign(a, lhs,
                               helpTransform(rhs, generatePrintln(a.pos.focus.toString, a.pos.focus.line) :: printList))                                                                                      
                        }
                    }
                                                            
                    case t @ Try(block, catches, finalizer) => {       
                        val newCatches = catches.foldLeft(List[CaseDef]())((list, i) => 
                          list ::: List(treeCopy.CaseDef(i, i.pat, 
                              i.guard, treeCopy.Block(i.body, generatePrintln(i.pos.focus.toString, i.pos.focus.line) :: 
                                  generatePrintln(t.pos.focus.toString, t.pos.focus.line) :: 
                                  generatePrintln(block.pos.focus.toString, block.pos.focus.line) ::
                                  printList, transform(i.body)))))
                        treeCopy.Try(t, transform(block), newCatches, transform(finalizer))
                    }
               
                    case t @ Throw(expr) => {                        
                        treeCopy.Block(t, generatePrintln(t.pos.focus.toString, t.pos.focus.line) :: generatePrintln(t.expr.pos.focus.toString, t.expr.pos.focus.line) :: printList, t)
                    }
                                      
                    case c @ ClassDef(mods, name, tparams, impl) => { 
                      val ClassDeclPos = (c.pos.focusStart.line to c.pos.focusEnd.line).toList.foldLeft(List[Tree]())((list, i) => 
                        generatePrintln(c.pos.focus.toString, i) :: list)
                        val ModuleValPos = c.impl.body.foldLeft(List[Tree]())((list, i) => {
                            val newPrint = i match {
                            case v @ ValDef(mods, name, tpt, rhs) => {
                              (v.pos.focusStart.line to v.pos.focusEnd.line).toList.foldLeft(List[Tree]())((list, i) => 
                                generatePrintln(v.pos.focus.toString, i) :: list)
                            }
                            case t => Nil
                          }
                          newPrint ::: list  
                          })
                        val newBody = c.impl.body.foldLeft(List[Tree]())((list, i) => list ::: List(helpTransform(i, ClassDeclPos ::: printList)))
                      treeCopy.ClassDef(c, c.mods, c.name, c.tparams,
                          treeCopy.Template(c.impl, c.impl.parents, c.impl.self, newBody ::: ModuleValPos))  
                    }
                    
                    case l @ LabelDef(name, params, rhs) => {                                           
                      treeCopy.LabelDef(l, name, params, helpTransform(rhs, generatePrintln(l.pos.focus.toString, l.pos.focus.line) :: printList))
                    }
                    
                    case f @ Function(vparams, body) => {
                      treeCopy.Function(f, vparams, helpTransform(body, generatePrintln(f.pos.focus.toString, f.pos.focus.line) :: printList))
                    }
                    
                    case t => t
                  }    
              }
          }
                  
          class OutputLineFilePhase(prev: Phase) extends StdPhase(prev){
              override def name = OutputLineFile.this.name
              def apply(unit: CompilationUnit){
                    val full:String = unit.body.toString()
                    val parsemap = full.split("\n")
                    val line = Map[Int, List[Int]]()
                    val mytransform = new MyTransformer
                    unit.body = mytransform.transform(unit.body)
                    println("Line Number: " + lineCounter)
              }
  
          }
      }
}

