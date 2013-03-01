package org.perl8.test

/** Classes to handle running test instances and providing output. */
package object harness {
  import scala.reflect.{ClassTag,classTag}

  /** Loads `className`, returning the
    * [[http://docs.oracle.com/javase/7/docs/api/java/lang/Class.html Class]]
    * instance.
    */
  def loadClass[T: ClassTag] (className: String): Class[_] =
    classTag[T].runtimeClass.getClassLoader.loadClass(className)

  /** Loads `className` and creates a new instance of it, using the
    * no-argument constructor.
    */
  def newInstance[T: ClassTag] (className: String): T =
    loadClass[T](className).newInstance.asInstanceOf[T]

  /** Loads `className` and creates a new instance of it, using a
    * one-argument constructor. Passes `arg` as the argument to the
    * constructor.
    */
  def newInstance[T: ClassTag, U <: AnyRef: ClassTag] (
    className: String,
    arg: U
  ): T = {
    val classObj = loadClass[T](className)
    val argClassObj = classTag[U].runtimeClass
    val constructor = classObj.getConstructor(argClassObj)
    constructor.newInstance(arg).asInstanceOf[T]
  }
}
