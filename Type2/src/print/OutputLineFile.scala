/*
 * Source code for compiler plugin of Type 2 dead code detection. The plugin prints out data flow during runtime.
 * By searching the data flows, we can know which of data flows contributes to the final compilation result.
 * Then use small tools in Type2 folder to find out corresponding Type 2 dead codes in compiler source.
 */

package localhost

import scala.reflect.runtime.universe._  

import scala.tools.nsc
import scala.compat.Platform.EOL
import scala.language.postfixOps
import scala.tools.nsc.ast.Printers
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.backend.icode
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
import scala.collection.immutable.Stack
import scala.reflect.internal.util.FreshNameCreator
import scala.reflect.internal.Symbols
import scala.reflect.internal._

class OutputLineFile(val global:Global) extends Plugin {  
      import global._
      val name = "outputlinefile"
      val description = "My OutputLineFile"
      val components = List[PluginComponent](Component) 
      
      private object Component extends PluginComponent with TypingTransformers{
        override def toString = ""
          val global:OutputLineFile.this.global.type = OutputLineFile.this.global                                              
          val runsAfter = List[String]("typer");
          val phaseName = OutputLineFile.this.name
          def newPhase(_prev: Phase) = new OutputLineFilePhase(_prev)
          def newTransformer(unit:CompilationUnit) = new MyTransformer(unit)       
          var lineCounter : Int = 0
          var globalSymbol : Symbol = null
          class MyTransformer(unit : CompilationUnit) extends TypingTransformer(unit) {     
            val global = Component.global
                        
            /*
             * Print out the data flow, given a name list of written AST nodes and a name lis of read AST nodes
             */
            def genDataFlow(write : List[String], read : List[String]) : List[Tree] = {
              val writers = write.foldLeft("")((sum, i) => sum + i + " ")
              val readers = read.foldLeft("")((sum, i) => sum + i + " ")
              if(writers.lastIndexOf("newvalue") == -1 && !(write == Nil && read == Nil)) {
              val liter = Literal(Constant(readers.trim() + " >>> " + writers + "\n"))
              liter.setType(typeOf[String])
              val newPrint : Tree = gen.mkMethodCall(definitions.PredefModule.info.decl(newTermName("print")), List(liter))         
              newPrint.setType(typeOf[Unit])
              lineCounter = lineCounter + 1
              List(newPrint, printFile)
              }
              else Nil     
            }
            
            /*
             * Get name of the source file where "tree" locates. And generate a "println" statement to print that name out.
             */
            def getFileName(tree : Tree) : Tree = {
              val posString = tree.pos.focus.toString
              val fileName = posString.substring(posString.lastIndexOf("/") + 1, posString.lastIndexOf(".scala"))
              val liter = Literal(Constant(fileName + "\n"))
              liter.setType(typeOf[String])
              val newPrint : Tree = gen.mkMethodCall(definitions.PredefModule.info.decl(newTermName("print")), List(liter))         
              newPrint.setType(typeOf[Unit])
            }
            
            /*
             * Get name of AST nodes for the data flow. Use the symbol if the AST has one, otherwise use the Class name 
             * in addition with AST id in the source file.
             */
            def getName(tree : Tree) : String = {
              tree match {
                    case p @ PackageDef(pid, stats) => namePick(p.name.toString)
                    
                    case m @ ModuleDef(mods, name, impl) => namePick(m.name.toString)
                    
                    case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                      namePick(d.name.toString)
                    }
                    
                    case a @ Apply(fun, args) => {
                      fun match {
                        case f @ Function(vparams, body) => "Function" + f.id
                        case t => namePick(getRealFunName(fun))
                      }
                    }
                    
                    case b @ Block(stats, expr) => "Block" + b.id
                                                             
                    case r @ Return(expr) => "Return" + r.id
                          
                    case v @ ValDef(mods, name, tpt, rhs) => namePick(v.name.toString)
                    
                    case ite @ If(cond, thenp, elsep) => "If" + ite.id
                                                          
                    case s @ Select(qualifier, selector) => namePick(s.name.toString)
                  
                    case m @ Match(selector, cases) => "Match" + m.id
                    
                   case n @ New(tpt) => "New" + n.id
                    
                    case a @ Assign(lhs, rhs) => "Assign" + a.id
                                                              
                    case t @ Try(block, catches, finalizer) => "Try" + t.id
               
                    case t @ Throw(expr) => "Throw" + t.id
                                      
                    case c @ ClassDef(mods, name, tparams, impl) => namePick(c.name.toString)
                    
                    case l @ LabelDef(name, params, rhs) => namePick(l.name.toString)
                    
                    case f @ Function(vparams, body) => "Function" + f.id
                    
                    case l @ Literal(value) => ""
                    
                    case i @ Ident(name) => namePick(i.name.toString)
                    
                    case t => "!!!"
              }
            }
            
           /*
            * A list contains name of values/variables defined in the package currently being transformed. 
            * These names are used in the data flows.
            */
              var nameList : Set[String] = Set()
              
              /*
               * printFile is used to store the "println" statement which prints the name of the package currently being transformed.
               * In Type2 we also want to print out package name that each data flow locates, so we store the "println" to avoid
               * calling "getFileName" every time we want to print the package name. (See PackageDef case in helpTransform)
               */
              var printFile : Tree = EmptyTree
              
              /*
               * Transform function calls "helpTransform" to do the transformation. 
               */
              override def transform(tree : Tree) : Tree = helpTransform(tree, Nil, Nil)
              
              /*
               * A function for debug. Print out the symbols and their owners in a tree.
               */
              def showSym(tree : Tree)  {
                tree match {
                    case p @ PackageDef(pid, stats) => {
                      println("Symbol: " + p.symbol + " " + "Owner: " + p.symbol.owner)
                      for(stat <- stats) showSym(stat)
                    }
                    
                    case m @ ModuleDef(mods, name, impl) => {
                      println("Symbol: " + m.symbol + " " + "Owner: " + m.symbol.owner)
                      showSym(impl)
                    }
                    
                    case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                      println("Symbol: " + d.symbol + " " + "Owner: " + d.symbol.owner)
                      for(vparams <- vparamss) 
                        for(vparam <- vparams) 
                          showSym(vparam)
                      showSym(rhs)
                      showSym(tpt)
                    }
                    
                    case a @ Apply(fun, args) => {
                      showSym(fun)
                      for(arg <- args) showSym(arg)
                    }
                    
                    case b @ Block(stats, expr) => {
                      for(stat <- stats) showSym(stat)
                      showSym(expr)
                    }
                                                             
                    case r @ Return(expr) => {
                      showSym(expr)
                    }
                          
                    case v @ ValDef(mods, name, tpt, rhs) => {
                      println("Symbol: " + v.symbol + " " + "Owner: " + v.symbol.owner)
                      showSym(rhs)
                      showSym(tpt)
                    }   
                    
                    case ite @ If(cond, thenp, elsep) => {
                      showSym(cond)
                      showSym(thenp)
                      showSym(elsep)
                    }
                                                          
                    case s @ Select(qualifier, selector) => {
                      println("Symbol: " + s.symbol + " " + "Owner: " + s.symbol.owner)
                    }
                  
                    case m @ Match(selector, cases) => {
                      showSym(selector)
                      for(caser <- cases) showSym(caser)
                    }
                    
                   case n @ New(tpt) => {
                     showSym(tpt)
                   }
                       
                    case a @ Assign(lhs, rhs) => {
                      showSym(lhs)
                      showSym(rhs)
                    }
                                                              
                    case t @ Try(block, catches, finalizer) => {
                      showSym(block)
                      for(caser <- catches) showSym(caser)
                      showSym(finalizer)
                    }
               
                    case t @ Throw(expr) => showSym(expr)
                                      
                    case c @ ClassDef(mods, name, tparams, impl) => {
                      showSym(impl)
                    }
                    
                   case l @ LabelDef(name, params, rhs) => {
                     println("Symbol: " + l.symbol + " " + "Owner: " + l.symbol.owner)
                     for(param <- params) println("Symbol: " + param.symbol + " " + "Owner: " + param.symbol.owner)
                     showSym(rhs)
                   }
                    
                    case f @ Function(vparams, body) => {
                      for(vparam <- vparams)
                        println("Symbol: " + vparam.symbol + " " + "Owner: " + vparam.symbol.owner)
                    }
                    
                    case l @ Literal(value) => 
                    
                    case i @ Ident(name) => println("Symbol: " + i.symbol + " " + "Owner: " + i.symbol.owner)
                      
                    case t @ TypeApply(fun, args) => {
                      showSym(fun)
                      for(arg <- args) 
                        showSym(arg)
                    }
                    
                    case t @ TypeTree() => 
                    
                    case c @ CaseDef(pat, guard, body) => {
                      showSym(pat)
                      showSym(guard)
                      showSym(body)
                    }
                    
                    case t @ Template(parents: List[Tree], self: ValDef, body: List[Tree]) => {
                      for(parent <- parents) 
                        println("Symbol: " + parent.symbol + " " + "Owner: " + parent.symbol.owner)
                      println("Symbol: " + self.symbol + " " + "Owner: " + self.symbol.owner)
                      for(bo <- body) showSym(bo)
                    }     
                      
                    case t => 
                }
              }
            
              /*
               * Function doing the transformation work. Because we still have problem with setting up symbols.
               * For Type 2 detection, we need to insert "println"s in some special position to maintain the order of data flows.
               * However this may cause errors in some source files, so we just skip these files before we find a solution. 
               */
              def helpTransform(tree : Tree, paramList : List[Tree], returnList : List[Tree], returnType : Tree = EmptyTree, parSymbol : Symbol = NoSymbol) : Tree = {     
                val newtree =  
                tree match {
                    case p @ PackageDef(pid, stats) => {
                      printFile = getFileName(p)
                      globalSymbol = p.symbol
                      if(
                          p.pos.focus.toString.lastIndexOf("library/scala/io/AnsiColor.scala") == -1   
                      && p.pos.focus.toString.lastIndexOf("library/scala/Console.scala") == -1
                      && p.pos.focus.toString.lastIndexOf("library/scala/Predef.scala") == -1
                      && p.pos.focus.toString.lastIndexOf("library/scala/util/DynamicVariable.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/AnyVal.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Function.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Boolean.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Byte.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Char.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Double.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Float.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Int.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Unit.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Long.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/AnyValCompanion.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Cloneable.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Short.scala") == -1   
                          && p.pos.focus.toString.lastIndexOf("library/scala/Console.scala") == -1                          
                          && p.pos.focus.toString.lastIndexOf("library/scala/Predef.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/Tuple") == -1
                          && p.pos.focus.toString.lastIndexOf("reflect/scala/reflect/api/Standard") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/collection/immutable/Stream.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("library/scala/sys/process/ProcessImpl.scala") == -1
                          && p.pos.focus.toString.lastIndexOf("scala/reflect/runtime/JavaMirrors") == -1
                          && p.pos.focus.toString.lastIndexOf("scala/reflect/runtime/SymbolLoaders") == -1
                          && p.pos.focus.toString.lastIndexOf("scala/reflect/runtime/TwoWayCaches") == -1 
                          && p.pos.focus.toString.lastIndexOf("scala/reflect/internal/Definitions") == -1  
                          && p.pos.focus.toString.lastIndexOf("scala/reflect/internal/Importers") == -1  
                          && p.pos.focus.toString.lastIndexOf("scala/reflect/internal/pickling/UnPickler") == -1
                          && p.pos.focus.toString.lastIndexOf("scala/reflect/internal/tpe/TypeConstraints") == -1 
                          && p.pos.focus.toString.lastIndexOf("scala/reflect/internal/tpe/TypeMaps") == -1
                          && p.pos.focus.toString.lastIndexOf("scala/reflect/io/VirtualDirectory") == -1
                          && p.pos.focus.toString.lastIndexOf("scala/tools/nsc/ast/DocComments") == -1   
                          && p.pos.focus.toString.lastIndexOf("scala/tools/nsc/ast/DocComments") == -1
                          && p.pos.focus.toString.lastIndexOf("scala/tools/nsc/ast/parser/Parsers") == -1
                          ) {                                                      
                        val newStats = p.stats.foldLeft(List[Tree]())((list, i) => list ::: List(helpTransform(i, Nil, Nil)))
                        treeCopy.PackageDef(p, p.pid, newStats)
                      }   
                     else p
                    }
                    
                    case m @ ModuleDef(mods, name, impl) => {
                      globalSymbol = m.symbol
                      val plugable : Boolean = m.impl.body.foldLeft(false)((plug, i) => {
                        i match {
                            case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                              if(name.toString == "<init>" || name.toString == "$init$") plug || true
                              else plug
                             }
                            case t => plug
                          }
                      })
                        for(i <- m.impl.body) { 
                        i match {
                          case v @ ValDef(mods, name, tpt, rhs) => {
                            if(!nameList.contains(getName(v))) {
                              nameList += getName(v) 
                            }
                          }
                          case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                            for(vparams <- vparamss)
                              for(vparam <- vparams)
                                if(!nameList.contains(getName(vparam))) {
                              nameList += getName(vparam) 
                          }
                          }
                          case t =>
                        }
                      }
                      
                      val inList = m.impl.body.foldLeft(List[String]())((list, i) => {
                        i match {
                          case v @ ValDef(mods, name, tpt, rhs) => getName(v) :: list
                          case t => list
                        }
                      })
                      
                      val params = m.impl.body.foldLeft(List[String]())((list, i) => {
                        i match {
                          case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                            list ::: vparamss.foldLeft(List[String]())((list, i) => {
                              list ::: i.foldLeft(List[String]())((list, i) => {
                                list ::: List(namePick(i.name.toString))
                              })
                            })
                          }
                          case t => list
                        }
                      })
                      
                      val transformChild = m.impl.body.foldLeft(List[Tree]())((list, i) => {
                        i match {
                            case v @ ValDef(mods, name, tpt, rhs) => {
                                list ::: List(helpTransform(v, paramList, Nil, EmptyTree, m.symbol)) ::: genDataFlow(getName(v) :: Nil, getName(rhs) :: Nil) 
                             }
                            case a @ Assign(lhs, rhs) => {
                                list ::: List(helpTransform(a, Nil, Nil, EmptyTree, m.symbol)) ::: genDataFlow(getName(lhs) :: Nil, getName(rhs) :: Nil)
                             }
                            case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                              if(name.toString == "<init>" || name.toString == "$init$") list ::: List(d)
                              else list ::: List(helpTransform(d, Nil, Nil, EmptyTree, m.symbol))
                             }
                            case t => list ::: List(helpTransform(t, Nil, Nil, EmptyTree, m.symbol))
                          }
                      })
                      if(plugable)
                      treeCopy.ModuleDef(m, m.mods, m.name,   
                          treeCopy.Template(m.impl, m.impl.parents, m.impl.self, genDataFlow(params, getName(m) + "$" :: Nil) :::
                              transformChild ::: genDataFlow(getName(m) :: Nil, inList)))
                      else 
                      treeCopy.ModuleDef(m, m.mods, m.name,   
                          treeCopy.Template(m.impl, m.impl.parents, m.impl.self, transformChild))
                    }  
                    
                    case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                      for(vparams <- vparamss)
                        for(vparam <- vparams)
                          if(!nameList.contains(getName(vparam))) {
                              nameList += getName(vparam) 
                          }
                       if(!mods.isLazy && !name.toString.endsWith("_$eq") && !nameList.contains(namePick(d.name.toString))
                           && d.symbol.annotations.toString.lastIndexOf("tailrec") == -1) {
                         
                            val params = vparamss.foldLeft(List[String]())((list, i) => {
                              list ::: i.foldLeft(List[String]())((list, i) => {
                                list ::: List(namePick(i.name.toString))
                              })
                            })
                            
                            treeCopy.DefDef(d, mods, name, tparams, vparamss, tpt,    
                              helpTransform(rhs, genDataFlow(params, (getName(d) + "$") :: Nil), 
                                  genDataFlow(getName(d) :: Nil, getName(rhs) :: Nil), d.tpt, d.symbol))
                            }          
                       else d
                    }
                    
                    case a @ Apply(fun, args) => {
                      val getname = getName(a)
                      val funName = if(getname.lastIndexOf("_$eq") != -1) getname.substring(0, getname.lastIndexOf("_$eq")) else getname + "$"
                      
                      val inList = args.foldLeft(List[String]())((list, arg) => list ::: List(getName(arg))) :::
                                   sparseSelect(getRealFunName(fun)).foldLeft(List[String]())((list, name) =>
                                   if(nameList.contains(name)) list ::: List(name) else list)
                                   
                      val newArgs = args.foldLeft(List[Tree]())((list, arg) => list ::: List(helpTransform(arg, Nil, Nil)))
                      
                      /*
                       * In the if condition are names of functions that we cannot do "let" transformation, due to problems with symbol.
                       * This is a obstacle to overcome, because it causes our data flow to be short and broken.
                       */
                      if(returnType != EmptyTree && parSymbol.name.toString != "toType"
                          && parSymbol.name.toString != "classBTypeFromParsedClassfile"
                          && parSymbol.name.toString != "conformsTo"
                          && parSymbol.name.toString != "specializedFunctionName"
                          && parSymbol.name.toString != "postTransform"
                          && parSymbol.name.toString != "unique"
                          && !(parSymbol.name.toString == "apply" && globalSymbol.name.toString == "ConstantFolder")
                          && !(parSymbol.name.toString == "expand" && globalSymbol.name.toString == "MacroExpander")
                          && parSymbol.name.toString != "isAmbiguousAssignment"
                          && parSymbol.name.toString != "transformSelect"
                          && parSymbol.name.toString != "macroExpanderAttachment"
                          && parSymbol.name.toString != "materializeClassTag"
                          && parSymbol.name.toString != "withCompiledScript"
                          && parSymbol.name.toString != "elementType"
                          && parSymbol.name.toString != "lub0"
                          && !(parSymbol.name.toString == "init" && globalSymbol.name.toString == "MethodTFA")
                          && !(parSymbol.name.toString == "apply" && globalSymbol.name.toString == "isJavaEntryPoint")
                          && parSymbol.name.toString != "programPoint"
                          && parSymbol.name.toString != "jvmWiseLUB"
                          && parSymbol.name.toString != "classBTypeFromSymbol"
                          && parSymbol.name.toString != "mirrorClassClassBType"
                          && parSymbol.name.toString != "isTopLevelModuleClass"
                          && !(parSymbol.name.toString == "isJavaEntryPoint" && globalSymbol.name.toString == "AsmPhase")
                          && !(parSymbol.name.toString == "initialProducers" && globalSymbol.name.toString == "ProdConsAnalyzer")
                          && parSymbol.name.toString != "inline"
                          && !(parSymbol.name.toString == "getTypesAtBlockEntry" && globalSymbol.name.toString == "InlineExceptionHandlersPhase")
                          && parSymbol.name.toString != "ownedName"
                          && parSymbol.name.toString != "loadClassSymbol"
                          && parSymbol.name.toString != "parseParents"
                          && parSymbol.name.toString != "getSymbolStaticField"
                          && parSymbol.name.toString != "createBridgeMethod"
                          && parSymbol.name.toString != "javaSig"
                          && parSymbol.name.toString != "makeBridgeDefDef"
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.transform.Erasure.ErasureTransformer.transform")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.transform.ExtensionMethods.extensionMethod")
                          && !(parSymbol.name.toString == "removeSymbolInCurrentScope")
                          && !(parSymbol.name.toString == "rebindSuper")
                          && !(parSymbol.name.toString == "doesConform")
                          && !(parSymbol.name.toString == "transformTemplate")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.transform.UnCurry.UnCurryTransformer.arrayToSequence")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.transform.UnCurry.UnCurryTransformer.dependentParamTypeErasure.isDependent")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.transform.patmat.MatchTreeMaking.TreeMakers.combineCasesNoSubstOnly")
                          && !(parSymbol.name.toString == "savingUndeterminedTypeParams")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.typechecker.Implicits.ImplicitSearch.typingLog")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.typechecker.Infer.Inferencer.tryInstantiating")
                          && parSymbol.name.toString != "calculateUndetparams"           
                          && parSymbol.name.toString != "eliminateModuleDefs"               
                          && parSymbol.name.toString != "typedTypeDef"
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.util.InterruptReq.execute")
                          && !(parSymbol.fullName.toString == "scala.tools.reflect.FormatInterpolator.badlyInvoked")  
                          && !(parSymbol.fullName.toString == "scala.tools.reflect.ToolBoxFactory.ToolBoxImpl.withCompilerApi.apply")
                          && !(parSymbol.fullName.toString == "scala.tools.reflect.WrappedProperties.systemProperties")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.interpreter.JavapClass.bytesFor")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.interpreter.ILoop.process")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.interpreter.IMain.WrappedRequest.loadAndRunReq")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.interpreter.IMain.withoutWarnings")
                          && !(parSymbol.fullName.toString == "scala.tools.nsc.interpreter.IMain.parse.apply") 
                        && !(parSymbol.name.toString == "typeFromNameTreatedAsTerm")
                        && !(parSymbol.name.toString == "readUntil")
                        && !(parSymbol.fullName.toString == "scala.tools.nsc.doc.model.ModelFactory.makeTemplate")
                        && !(parSymbol.fullName.toString == "scala.tools.nsc.interactive.Global.typeMembers")
                        && !(parSymbol.fullName.toString == "scala.tools.nsc.interactive.Response.get")
                        && !(parSymbol.fullName.toString == "scala.tools.scalap.scalax.rules.scalasig.ConstantPool.apply")  
                        && !(parSymbol.fullName.toString == "scala.actors.remote.TcpService.connect")
                        && !(parSymbol.fullName.toString == "scala.actors.remote.TcpService.send")
                        && !(parSymbol.fullName.toString == "scala.actors.FutureActor.act")
                        && !(parSymbol.fullName.toString == "scala.actors.remote.NetKernel.processMsg")
                        && !(parSymbol.fullName.toString == "scala.concurrent.BatchingExecutor.Batch.run")
                        && !(parSymbol.fullName.toString == "scala.reflect.internal.Trees.traverseMemberDef")) {
                        val term = newTermName("newvalue")
                        val newapply = treeCopy.Apply(a, a.fun, newArgs)       
                        val newVal = (ValDef(Modifiers(0), term, TypeTree(a.tpe), newapply))
                        newVal.setType(a.tpe)
                        val newSymbol = a.symbol.newValue(newTermName("newvalue"), a.pos, 0) 
                        newSymbol.owner = parSymbol                                 
                        newVal.setSymbol(newSymbol)             
                        newVal.symbol.setInfo(a.tpe)
                        val newIdent = Ident(newVal.symbol)
                        newIdent.setType(a.tpe)                                            
                        val newblock = 
                          if(!funName.startsWith("while"))
                            treeCopy.Block(a, paramList ::: genDataFlow(funName :: Nil, inList) ::: List(newVal) ::: returnList, newIdent)
                            else 
                              treeCopy.Block(a, paramList ::: returnList, a)
                        newblock
                      }
                      else 
                        if(!funName.startsWith("while"))
                          treeCopy.Block(a, paramList ::: genDataFlow(funName :: Nil, inList) ::: returnList, (treeCopy.Apply(a, fun, newArgs)))        
                        else 
                          treeCopy.Block(a, paramList ::: returnList, (treeCopy.Apply(a, fun, args)))
                    }
                    
                    case b @ Block(stats, expr) => {
                      for(stat <- stats) {
                        stat match {
                          case v @ ValDef(mods, name, tpt, rhs) => {
                            if(!nameList.contains(getName(v))) {
                              nameList += getName(v) 
                            }
                          }
                          case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                            for(vparams <- vparamss)
                              for(vparam <- vparams)
                                if(!nameList.contains(getName(vparam))) 
                                    nameList += getName(vparam) 
                          }
                          case t =>
                        }
                      }
                      val inList = stats.foldLeft(List[String]())((list, stat) => list ::: List(getName(stat)))
                      val newStats = stats.foldLeft(List[Tree]())((list, stat) => {
                        stat match {
                          case v @ ValDef(mods, name, tpt, rhs) => {
                            list ::: List(helpTransform(stat, Nil, Nil)) ::: genDataFlow(getName(v) :: Nil, getName(rhs) :: Nil)
                          }
                          case a @ Assign(lhs, rhs) => {
                            list ::: List(helpTransform(stat, Nil, Nil)) ::: genDataFlow(getName(lhs) :: Nil, getName(rhs) :: Nil)
                          }
                          case t => list ::: List(helpTransform(stat, Nil, Nil))
                        }
                      })
                      treeCopy.Block(b, paramList ::: newStats, helpTransform(expr, Nil, genDataFlow(getName(b) :: Nil, getName(expr) :: Nil) ::: returnList, returnType, parSymbol))
                    } 
                                                             
                    case r @ Return(expr) => {
                      (treeCopy.Return(r, helpTransform(expr, paramList, 
                          genDataFlow(getName(r) :: Nil, getName(expr) :: Nil) ::: returnList, returnType)))
                    }
                          
                    case v @ ValDef(mods, name, tpt, rhs) => {                              
                      (treeCopy.ValDef(v, mods, name, tpt, helpTransform(rhs, paramList, returnList)))
                    }
                    
                    case ite @ If(cond, thenp, elsep) => {         
                      val newCond = helpTransform(cond, Nil, Nil)
                      val newThenp = helpTransform(thenp, Nil, genDataFlow(getName(ite) :: Nil, getName(cond) :: getName(thenp) :: Nil) ::: returnList, returnType, parSymbol)
                      val newElsep = helpTransform(elsep, Nil, genDataFlow(getName(ite) :: Nil, getName(cond) :: getName(elsep) :: Nil) ::: returnList, returnType, parSymbol)
                      (treeCopy.Block(ite, paramList, (treeCopy.If(ite, newCond, newThenp, newElsep))))
                    }
                                                          
                    case s @ Select(qualifier, selector) => {
                      val inList = sparseSelect(getRealFunName(qualifier)).foldLeft(List[String]())((list, name) => 
                        if(nameList.contains(name)) list ::: List(name) else list)  
                      val dataFlow = if(s.symbol.isMethod && !nameList.contains(getName(s))) genDataFlow(getName(s) :: Nil, inList) else Nil
                      treeCopy.Block(s, paramList ::: dataFlow ::: returnList, s)
                    }         
                  
                    case m @ Match(selector, cases) => {   
                      val newCases = cases.foldLeft(List[CaseDef]())((list, caser) => list ::: List(treeCopy.CaseDef(caser, caser.pat, caser.guard,   
                          helpTransform(caser.body, Nil, genDataFlow(getName(m) :: Nil, getName(caser.body) :: Nil) ::: returnList, returnType, parSymbol))))
                      (treeCopy.Block(m, paramList, treeCopy.Match(m, m.selector, newCases)))
                    }
                    
                   case n @ New(tpt) => {
                      (treeCopy.Block(n, paramList, 
                          helpTransform(tpt, Nil, genDataFlow(getName(n) :: Nil, getName(tpt) :: Nil) ::: returnList, returnType, parSymbol)))
                    }
                       
                    case a @ Assign(lhs, rhs) => {
                      (treeCopy.Assign(a, lhs, helpTransform(rhs, paramList, genDataFlow(getName(lhs) :: Nil, getName(rhs) :: Nil) ::: returnList, returnType, parSymbol)))
                    }
                                                              
                    case t @ Try(block, catches, finalizer) => {
                      val newBlock = helpTransform(block, paramList, genDataFlow(getName(t) :: Nil, getName(block) :: Nil) ::: returnList, returnType, parSymbol)
                      val newCatches = catches.foldLeft(List[CaseDef]())((list, catcher) => list ::: 
                          (treeCopy.CaseDef(catcher, catcher.pat, catcher.guard, helpTransform(catcher.body, Nil, Nil)) :: Nil))
                      (treeCopy.Try(t, newBlock, newCatches, finalizer))
                    }
               
                    case t @ Throw(expr) => {
                       (treeCopy.Throw(t, helpTransform(expr, paramList, genDataFlow(getName(t) :: Nil, getName(expr) :: Nil) ::: returnList, returnType, parSymbol))) 
                    }
                                      
                    case c @ ClassDef(mods, name, tparams, impl) => {
                      if(c.symbol.name.toString != "$anon")
                      globalSymbol = c.symbol
                      for(i <- c.impl.body) {
                        i match {
                          case v @ ValDef(mods, name, tpt, rhs) => {
                            if(!nameList.contains(namePick(name.toString).trim)) {   
                              nameList += namePick(name.toString).trim  
                            }
                          }
                          case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                            for(vparams <- vparamss)
                              for(vparam <- vparams)
                                if(!nameList.contains(getName(vparam))) {
                              nameList += getName(vparam) 
                          }
                          }
                          case t =>
                        }
                      }
                      val inList = c.impl.body.foldLeft(List[String]())((list, i) => {
                        i match {
                            case v @ ValDef(mods, name, tpt, rhs) => {
                               if(v.symbol.isParamAccessor) getName(v) :: list      
                               else list
                             }
                            case t => list
                            }
                        })
                      val initList = inList ::: c.impl.parents.foldLeft(List[String]())((list, par) => namePick(par.toString) :: list)
                      val params = c.impl.body.foldLeft(List[String]())((list, i) => {
                        i match {
                            case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                              if(name.toString == "<init>" || name.toString == "$init$") {
                                list ::: vparamss.foldLeft(List[String]())((list, parlist) => {
                                  parlist.foldLeft(List[String]())((list, param) => list ::: List(namePick(param.name.toString)))
                                })
                              }
                              else list
                            }
                            case t => list
                          }
                      })
                      val plugable = c.impl.body.foldLeft(false)((plug, i) => {
                        i match {
                          case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => 
                              if(name.toString == "<init>" || name.toString == "$init$") plug || true
                              else plug
                          case t => plug
                        }
                      })
                      val transformChild = c.impl.body.foldLeft(List[Tree]())((list, i) => {
                        i match {
                            case v @ ValDef(mods, name, tpt, rhs) => {
                               val dataFlow = if(!v.symbol.isParamAccessor) genDataFlow(getName(v) :: Nil, getName(rhs) :: Nil) else Nil
                                list ::: List(helpTransform(v, Nil, Nil, returnType)) ::: dataFlow   
                             }
                            
                          case a @ Assign(lhs, rhs) => {
                            list ::: List(helpTransform(a, Nil, Nil)) ::: genDataFlow(getName(lhs) :: Nil, getName(rhs) :: Nil)
                          }
                            case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => {
                              if(name.toString == "<init>" || name.toString == "$init$") {
                                 list ::: List(d)      
                              }
                              else list ::: List(helpTransform(d, Nil, Nil, returnType))
                            }
                            case t => list ::: List(helpTransform(t, Nil, Nil, returnType))
                          }
                      })
                        if(plugable)
                      (treeCopy.ClassDef(c, c.mods, c.name, c.tparams,    
                          treeCopy.Template(c.impl, c.impl.parents, c.impl.self, genDataFlow(params, getName(c) + "$" :: Nil) ::: 
                              transformChild ::: genDataFlow(getName(c) :: Nil, initList))))     
                              else
                                (treeCopy.ClassDef(c, c.mods, c.name, c.tparams,    
                          treeCopy.Template(c.impl, c.impl.parents, c.impl.self,  
                              transformChild)))
                    }
                    
                   case l @ LabelDef(name, params, rhs) => {
                        val paramList = params.foldLeft(List[String]())((list, param) => list ::: List(namePick(param.name.toString)))
                          (treeCopy.LabelDef(l, name, params, 
                              helpTransform(rhs, Nil, 
                                  genDataFlow(getName(l) :: Nil, getName(rhs) :: Nil) ::: returnList, returnType, l.symbol)))
                   }
                    
                    case f @ Function(vparams, body) => {    
                        val paramList = vparams.foldLeft(List[String]())((list, param) => list ::: List(namePick(param.name.toString)))
                           (treeCopy.Function(f, vparams, 
                              helpTransform(body, genDataFlow(paramList, (getName(f) + "$") :: Nil), 
                                  genDataFlow(getName(f) :: Nil, getName(body) :: Nil) ::: returnList, returnType, f.symbol)))
                    }
                    
                    case l @ Literal(value) => {
                      (treeCopy.Block(l, paramList ::: returnList, l))
                    }
                    
                    case i @ Ident(name) => {
                      if(returnList != Nil) treeCopy.Block(i, paramList ::: returnList, i)
                      else i
                    }
                    
                    case t => t
                  }   
                newtree
              }  
              
              /*
               * To get the real name of a function call. For example the name of new A() should be package.module.A.init.
               * Print out the name of such function call only end up with new A, so we write a function to get the name of
               * such function call in the form of Select.
               */
              def getRealFunName(name : Tree) : String = {
                name match {
                  case s @ Select(qualifier, selector) => {
                    getRealFunName(qualifier) + "." + selector.toString
                  }
                  case n @ New(tpt) => {
                    getRealFunName(tpt)
                  }
                  case t @ This(qual) => {
                    qual.toString
                  }
                  case t @ TypeApply(fun, args) => getRealFunName(fun)
                  case t @ TypeTree() => namePick(if(t.original != null && t.original.children.length > 1) t.original.children(0).toString else t.toString)
                  case t => {
                    t.toString
                  }
                }
              }
            
            /*
             * Split the names separated by "." in a Select.
             * Store the results into a list.
             */
            def sparseSelect(select : String) : List[String] = {
              var result : List[String] = Nil
              var s = select
              while(s.indexOf(".") != -1) {
                result = result ::: List(s.substring(0, s.indexOf(".")).trim)
                s = s.substring(s.indexOf(".") + 1)
              }
              result = result ::: List(s)
              result
            }
            
            /*
             * Pick the last section of a name. For example, namePick("package.module.classA") == "classA"
             */
            def namePick(name : String) : String = {
              if(name.lastIndexOf(".") != -1)
                if(name.substring(name.lastIndexOf(".") + 1) == "<init>" || name.substring(name.lastIndexOf(".") + 1) == "$init$")
                  namePick(name.substring(0, name.lastIndexOf(".")))
                else
                  name.substring(name.lastIndexOf(".") + 1).trim
              else name.trim
            } 
          }
          
                  
          class OutputLineFilePhase(prev: Phase) extends StdPhase(prev){
              override def name = OutputLineFile.this.name
              def apply(unit: CompilationUnit){
                    val full:String = unit.body.toString()
                    val parsemap = full.split("\n")
                    val line = Map[Int, List[Int]]()
                    val mytransform = new MyTransformer(unit)
                    unit.body = mytransform.transform(unit.body)
                    println("Line Number: " + lineCounter)
              }
  
          }
      }
}

