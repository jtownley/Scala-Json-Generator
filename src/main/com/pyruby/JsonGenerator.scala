package com.pyruby

import java.text.SimpleDateFormat
import java.util.Date

/*
Copyright 2010 - Chris Tarttelin & James Townley
Release under Apache-BSD style License

Version: 0.2
*/

object JsonGenerator {
  private val self: ThreadLocal[Gen] = new ThreadLocal[Gen]

  def jsonObject(name: String)(f: => Unit) = json(Some(name), "{", "}", f)
  def jsonObject()(f: => Unit) = json(None, "{", "}", f)
  def jsonArray(name: String)(f: => Unit) = json(Some(name), "[", "]", f)
  def jsonArray()(f: => Unit) = json(None, "[", "]", f)
  def field(name: String, value: Option[Any]) = content(Some(name), value)

  def value(value: Option[Any]) = content(None, value)

  private def content(label: Option[String], value: Option[Any]) = {
    if (value.isEmpty) {
      // do nothing
    } else if (self.get != null) {
      val content = if (label.isDefined) "\"" + label.get + "\":" else ""
      val container = self.get
      val data = value.get match {
        case v : String => "\"" + v.replaceAll("\\\\", "\\\\\\\\").replaceAll("\\\"","\\\\\"") + "\""
        case v : Int => v.toString
        case v : Double => v.toString
        case v : Long => v.toString
        case v : Boolean => v.toString
        case v : Date => "\"" + new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").format(v)+ "\""
        case v : Any => "\"" + v.toString.replaceAll("\\\\", "\\\\\\\\").replaceAll("\\\"","\\\\\"") + "\""
      }
      container.details = container.details ::: List(content + data)
    } else {
      throw new RuntimeException("Cannot have a value outside of a containing object")
    }
    this
  }

  private def json(name: Option[String], prefix: String, suffix: String, f: => Unit) = {
    val prior = self.get
    self.set(Gen(name, prefix, suffix))
    if (prior != null) {
      prior.gens = prior.gens ::: List(self.get)
    }
    f
    val reply = self.get
    self.set(prior)
    reply
  }
}

case class Gen(name: Option[String], prefix: String, suffix: String) {
  var details = List[String]()
  var gens = List[Gen]()

  def asString: String = {
    details = details ::: (gens.map(_.asString).filter(_ != null))
    if (details.isEmpty) {
      null
    } else if (name.isDefined) {
      String.format(""""%s":%s%s%s""", name.get, prefix, details.mkString(","), suffix)
    } else {
      String.format("""%s%s%s""", prefix, details.mkString(","), suffix)
    }
  }
}
