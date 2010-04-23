package com.point2

import com.point2.JsonGenerator._
import org.junit.Assert._
import org.junit.Test
import java.util.Calendar

/*
Copyright 2010 - Chris Tarttelin & James Townley
Release under Apache-BSD style License

Version: 0.1
*/

class JsonGeneratorTest {
  @Test
  def jsonObject_AnObjectWithASingleFieldOutPutsJson {
    val obj = jsonObject() {
      field("label", Some("value"))
    }
    assertEquals("""{"label":"value"}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldHandleANestedObject {
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonObject("llabel") {
        field("nested", Some("nestedvalue"))
      }
    }
    assertEquals("""{"label":"value","llabel":{"nested":"nestedvalue"}}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldHandleAnSimpleArray {
    val obj = jsonObject() {
      jsonArray("Cheeses") {
        value(Some("Gorgonzola"))
        value(Some("Danish Blue"))
        value(Some("Stilton"))
      }
    }
    assertEquals("""{"Cheeses":["Gorgonzola","Danish Blue","Stilton"]}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldHandleAnObjectArray {
    val obj = jsonObject() {
      jsonArray("Cheeses") {
        jsonObject("Gorgonzola") {
          field("Firmness", Some("Medium"))
          field("Aroma", Some("Odiferous"))
          field("Texture", Some("Smooth with Lumps"))
        }
        jsonObject("Munster") {
          field("Firmness", Some("Soft"))
          field("Aroma", Some("That smell you get after removing a gumboot from a bog"))
          field("Texture", Some("Creamy Spreadable"))
        }
      }
    }
    assertEquals("""{"Cheeses":["Gorgonzola":{"Firmness":"Medium","Aroma":"Odiferous","Texture":"Smooth with Lumps"},"Munster":{"Firmness":"Soft","Aroma":"That smell you get after removing a gumboot from a bog","Texture":"Creamy Spreadable"}]}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldHandleAnEnclosingArray {
    val obj = jsonArray() {
      value(Some("Gorgonzola"))
      value(Some("Danish Blue"))
      value(Some("Stilton"))
    }
    assertEquals("""["Gorgonzola","Danish Blue","Stilton"]""", obj.asString)
  }

  @Test //Note Rare use case could use future improvement if necessary
  def jsonObject_ShouldHandleANestedArray {
    val obj = jsonArray() {
      jsonArray() {
        value(Some("Mozza"))
        value(Some("Mozzeralla"))
        value(Some("String"))
      }
     jsonArray() {
        value(Some("Cheddar"))
        value(Some("Orange"))
        value(Some("Cheese"))
      }
    }
    assertEquals("""[["Mozza","Mozzeralla","String"],["Cheddar","Orange","Cheese"]]""", obj.asString)
  }

  @Test
  def jsonObject_ShouldHandleAnObjectInANestedArray {
    val obj = jsonObject() {
      jsonArray("Cheese") {
        jsonObject() {
          field("name:", Some("Munster"))
          field("age", Some(5))
        }
        jsonObject() {
          field("name:", Some("Brie"))
          field("age", Some(3))
        }
      }
    }
    assertEquals("""{"Cheese":[{"name:":"Munster","age":5},{"name:":"Brie","age":3}]}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldCollapseEmptyObjects {
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonObject("llabel") {
        field("nested", None)
      }
    }
    assertEquals("""{"label":"value"}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldCollapseEmptyObjectsInArray {
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonArray("Cheeses") {
        value(None)
      }
    }
    assertEquals("""{"label":"value"}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldReturnNullIfAllEmpty {
    val obj = jsonObject() {
      field("label", None)
      jsonObject("llabel") {
        field("nested", None)
      }
    }
    assertEquals(null, obj.asString)
  }

  @Test
  def jsonObject_ShouldHandleBooleans {
    val obj = jsonObject() {
      field("label", Some(true))
    }
    assertEquals("""{"label":true}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldHandleNumbers {
    val obj = jsonObject() {
      field("label", Some(31.468765))
      field("label2", Some(42))
    }
    assertEquals("""{"label":31.468765,"label2":42}""", obj.asString)
  }
 
  @Test
  def jsonObject_ShouldHandleDates {
    val c = Calendar.getInstance()
    c.set(2010, Calendar.DECEMBER, 11, 11, 11, 11)
    c.set(Calendar.MILLISECOND, 111)
    val obj = jsonObject() {
      field("date", Some(c.getTime))
    }
    assertEquals("""{"date":"2010-12-11T11:11:11.111-0600"}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldDefaultToToStringForUnknownTypes {
    class Foo {
      override def toString = "Cambazola \" is \" lovely"
    }
    val obj = jsonObject() {
      field("label", Some(new Foo))
    }
    assertEquals("""{"label":"Cambazola \" is \" lovely"}""", obj.asString)
  }

}