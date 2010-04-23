package com.point2
import collection.jcl.ArrayList
import java.util.Date
import java.text.SimpleDateFormat

/*
Copyright 2010 - Chris Tarttelin & James Townley
Release under Apache-BSD style License

Version: 0.1
*/

object JsonGenerator {
  private var self: Option[Gen] = None

  def jsonObject(name: String)(f: => Unit) = json(Some(name), "{", "}", f)
  def jsonObject()(f: => Unit) = json(None, "{", "}", f)
  def jsonArray(name: String)(f: => Unit) = json(Some(name), "[", "]", f)
  def jsonArray()(f: => Unit) = json(None, "[", "]", f)
  def field(name: String, value: Option[Any]) = content(Some(name), value)

  def value(value: Option[Any]) = content(None, value)

  private def content(label: Option[String], value: Option[Any]) = {
    if (value.isEmpty) {
      // do nothing
    } else if (self.isDefined) {
      val content = if (label.isDefined) "\"" + label.get + "\":" else ""
      val container = self.get
      val data = value.get match {
        case v : String => "\"" + v.replaceAll("\"","\\\\\"") + "\""
        case v : Int => v.toString
        case v : Double => v.toString
        case v : Boolean => v.toString
        case v : Date => "\"" + new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").format(v)+ "\""
        case v : Any => "\"" + v.toString.replaceAll("\"","\\\\\"") + "\""
      }
      container.details.add(content + data)
    } else {
      throw new RuntimeException("Cannot have a value outside of a containing object")
    }
    this
  }

  private def json(name: Option[String], prefix: String, suffix: String, f: => Unit) = {
    val prior = self
    self = Some(Gen(name, prefix, suffix))
    if (prior.isDefined) {
      prior.get.gens.add(self.get)
    }
    f
    val reply = self
    self = prior
    reply.get
  }
}

case class Gen(name: Option[String], prefix: String, suffix: String) {
  val details = new ArrayList[String]()
  val gens = new ArrayList[Gen]()

  def asString: String = {
    details.addAll(gens.map(_.asString).filter(_ != null))
    if (details.isEmpty) {
      null
    } else if (name.isDefined) {
      String.format(""""%s":%s%s%s""", name.get, prefix, details.mkString(","), suffix)
    } else {
      String.format("""%s%s%s""", prefix, details.mkString(","), suffix)
    }
  }
}


//object Listing {
//  import JsonGenerator._
//
//  def asJson = {
//    jsonObject {
//      field("id", Some("123"))
//      jsonObject("Address") {
//        field("street", Some("Eastview"))
//        field("town", Some("Saskatoon"))
//        field("country", Some("Canada"))
//        jsonObject("Style") {
//          field("cheese", Some("Lovely"))
//        }
//      }
//      jsonArray("numbers") {
//        value(Some("1"))
//        if ("so What" == "no way") {
//          value(Some("2"))
//        }
//        value(None)
//        value(Some("3"))
//      }
//      jsonObject("names") {
//        field("first", None)
//        //field("last", Some("Tarttie"))
//        field("last", None)
//      }
//    }
//    jsonString
//  }
//
//  def main(args: Array[String]) = {
//    println(asJson)
//  }
//}
