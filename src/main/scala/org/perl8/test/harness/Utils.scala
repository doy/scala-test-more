package org.perl8.test.harness

import scala.reflect.{ClassTag,classTag}

object Utils {
  def loadClass[T: ClassTag] (className: String): Class[_] =
    classTag[T].runtimeClass.getClassLoader.loadClass(className)

  def newInstance[T: ClassTag] (className: String): T =
    loadClass[T](className).newInstance.asInstanceOf[T]

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
