package com.pyruby

import java.text.SimpleDateFormat
import java.util.Date

/*
Copyright 2010-2012 - Chris Tarttelin & James Townley
Release under Apache-BSD style License

Version: 0.7.0
*/

object JsonGenerator {

  class Options {
    var OMIT_NONE_FIELDS = true
  }

  // json(None,"{","}",
  //  json(Some(Cheeses), "[", "]",
  //    json(None, "{", "}",
  //      content(Some(name), value))))

  private val self: ThreadLocal[Gen] = new ThreadLocal[Gen]
  val options = new Options

  def jsonObject(name: String)(f: => Unit) = json(Some(name), "{", "}", f)

  def jsonObject()(f: => Unit) = json(None, "{", "}", f)

  def jsonArray(name: String)(f: => Unit) = json(Some(name), "[", "]", f)

  def jsonArray()(f: => Unit) = json(None, "[", "]", f)

  def field(name: String, value: Option[Any]) = content(Some(name), value)

  def value(value: Option[Any]) = content(None, value)

  def rawJson(value: Option[String]) = {
    val container = self.get
    value match {
      case None => container.details = container.details
      case Some(str) => container.details = container.details ::: List(str)
    }
  }

  private def content(label: Option[String], value: Option[Any]) = {
    if (value.isEmpty && options.OMIT_NONE_FIELDS) {
      // do nothing
    } else if (self.get != null) {
      val content = if (label.isDefined) "\"" + label.get + "\":" else ""
      val container = self.get

      var data: String = null
      data = value match {
        case None => null
        case Some(null) => null
        case Some(v: Int) => v.toString
        case Some(v: Double) => v.toString
        case Some(v: Long) => v.toString
        case Some(v: Boolean) => v.toString
        case Some(v: Date) => "\"" + new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").format(v) + "\""
        case Some(v: Any) => {
          val buff = new StringBuilder("\"")
          for (c <- v.toString.toCharArray) {
            buff.append(c match {
              case c: Char if c == '\\' => "\\\\"
              case c: Char if c == '"' => "\\\""
              case c: Char if (c.toInt > 31) => c.toString
              case c: Char if (c == '\n') => "\\\\n"
              case c: Char if (c == '\t') => "\\\\t"
              case c: Char if (c == '\r') => "\\\\r"
              case c: Char if (c == '\b') => "\\\\b"
              case c: Char if (c == '\f') => "\\\\f"
            })

          }
          buff.append("\"")
          buff.toString
        }
      }
      (content, container.details, data) match {
        case ("", List(), null) => container.details = null
        case (_, null, _) => container.details = List(content + data)
        case ("", _, null) => {
          /*Do nothing*/
        }
        case _ => container.details = container.details ::: List(content + data)
      }
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
    (prior, containsNullDetailValues(reply))match {
      case (Gen(Some(_),"[","]"),true) => reply.details = null
      case _ => {}
    }
    reply
  }
  
  private def containsNullDetailValues(reply: Gen) = {
    if (reply.details == null) false
    else reply.details.foldLeft(true)((arg,value)=>{arg && value.endsWith(":null")})
  }
}


case class Gen(name: Option[String], prefix: String, suffix: String) {
  var details = List[String]()
  var gens = List[Gen]()

  def asString: String = {
    details = details ::: (gens.map(_.asString).filter(_ != null))
    (details,name) match {
      case (null,None) => ""
      case (null,Some(name)) => String.format(""""%s":%s""", name, "null")
      case (List(),_) => null
      case (l,Some(name)) if l.mkString.isEmpty => String.format(""""%s":%s""", name, "null")
      case (d ,Some(name)) => String.format(""""%s":%s%s%s""", name, prefix, d.filter(!_.isEmpty).mkString(","), suffix)
      case (d,None) => String.format("""%s%s%s""", prefix, d.mkString(","), suffix)
    }
  }
}