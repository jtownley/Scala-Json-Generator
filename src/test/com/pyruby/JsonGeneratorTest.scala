package com.pyruby

import com.pyruby.JsonGenerator._
import org.junit.Assert._
import java.util.Calendar
import org.junit.{Before, Test}

/*
Copyright 2010-2012 - Chris Tarttelin & James Townley
Release under Apache-BSD style License

Version: 0.7.0
*/

class JsonGeneratorTest {
  @Before
  def setup {
    JsonGenerator.options.OMIT_NONE_FIELDS = true
  }

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
  def jsonObject_ShouldNotCollapseNonEmptyObjectsInArray {
    val obj = jsonObject() {
      jsonArray("Cheeses") {
        jsonObject() {
          jsonObject("cheese") {
            field("name", Some("Munster"))
            field("age", Some(5))
          }
        }
      }
    }
    assertEquals("""{"Cheeses":[{"cheese":{"name":"Munster","age":5}}]}""", obj.asString)
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
  def jsonObject_ShouldReturnFieldWithNullValue {
    val obj = jsonObject() {
      field("label", Some(null))
    }
    assertEquals("""{"label":null}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldMakeArrayNull_WhenValuesNull {
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonArray("Cheeses") {
        value(Some(null))
      }
    }
    assertEquals("""{"label":"value","Cheeses":null}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldMaintainArray_WhenSomeOfTheValuesNull {
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonArray("Cheeses") {
        value(Some(null))
        value(Some("Gouda"))
        value(Some(null))
      }
    }
    assertEquals("""{"label":"value","Cheeses":["Gouda"]}""", obj.asString)
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
      override def toString = """Cambazola " is " lovely"""
    }
    val obj = jsonObject() {
      field("label", Some(new Foo))
    }
    assertEquals("""{"label":"Cambazola \" is \" lovely"}""", obj.asString)
  }

  @Test
   def rawJson_ShouldNotChangeExistingJsonObject_whenGivenEmpty {
    val obj = jsonObject() {
          field("label1",Some("content1"))
          rawJson(None)
        }
    assertEquals("""{"label1":"content1"}""", obj.asString)
  }


  @Test
  def rawJson_ShouldInsertRawJsonInsideAnEmptyJsonObject {
    val testJson = Some(""""testLabel":"testContent"""")
    val obj = jsonObject() {
      rawJson(testJson)
    }
    assertEquals("""{"testLabel":"testContent"}""", obj.asString)
  }

  @Test
  def rawJson_ShouldInsertRawJsonInsideExistingJsonOnject{
     class Foo {
      override def toString = """Cambazola " is " lovely"""
    }
    val testJson = Some(""""testObject":{"testLabel":"testContent"}""")
    val obj = jsonObject() {
      field("label", Some(new Foo))
      rawJson(testJson)
      field("label2", Some(new Foo))
    }
   assertEquals("""{"label":"Cambazola \" is \" lovely","testObject":{"testLabel":"testContent"},"label2":"Cambazola \" is \" lovely"}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldEscapeBackslash {
    val obj = jsonObject() {
      field("label", Some("""Grueyer \ Funky"""))
    }
    assertEquals("""{"label":"Grueyer \\ Funky"}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldEscapeLineFeeds {
    val obj = jsonObject() {
      field("label", Some("Edam \r Bouncy"))
    }
    assertEquals("""{"label":"Edam \\r Bouncy"}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldEscapeTabs {
    val obj = jsonObject() {
      field("label", Some("Munster \t Stinky"))
    }
    assertEquals("""{"label":"Munster \\t Stinky"}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldEscapeNewLines {
    val obj = jsonObject() {
      field("label", Some("Cheddar \n Melty"))
    }
    assertEquals("""{"label":"Cheddar \\n Melty"}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldReturnFieldWithNullValue_whenOmitNoneFieldsFalse {
    JsonGenerator.options.OMIT_NONE_FIELDS = false
    val obj = jsonObject() {
      field("label", None)
    }
    assertEquals("""{"label":null}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldMakeArrayNull_WhenValuesNullAndOmitNoneFieldsFalse {
    JsonGenerator.options.OMIT_NONE_FIELDS = false
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonArray("Cheeses") {
        value(None)
      }
    }
    assertEquals("""{"label":"value","Cheeses":null}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldMaintainArray_WhenSomeOfTheValuesNullAndOmitNoneFieldsFalse {
    JsonGenerator.options.OMIT_NONE_FIELDS = false
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonArray("Cheeses") {
        value(None)
        value(Some("Gouda"))
        value(None)
      }
    }
    assertEquals("""{"label":"value","Cheeses":["Gouda"]}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldNotMaintainArray_WhenAllOfTheObjectsValuesNullInListAndOmitNoneFieldsFalse {
    JsonGenerator.options.OMIT_NONE_FIELDS = false
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonArray("Cheeses") {
        jsonObject() {
          field("smell", None)
          field("texture", None)
        }
      }
    }
    assertEquals("""{"label":"value","Cheeses":null}""", obj.asString)
  }
  
  @Test
  def jsonObject_ShouldNotMaintainArray_WhenSomeOfTheObjectsValuesNullInListAndOmitNoneFieldsFalse {
    JsonGenerator.options.OMIT_NONE_FIELDS = false
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonArray("Cheeses") {
        jsonObject() {
          field("smell", None)
          field("texture", Some("Firm(in parts)"))
        }
        jsonObject() {
          field("smell", None)
          field("texture", None)
        }
      }
    }
    assertEquals("""{"label":"value","Cheeses":[{"smell":null,"texture":"Firm(in parts)"}]}""", obj.asString)
  }

  @Test
  def jsonObject_ShouldNotMaintainArray_WhenGroupsOfTheObjectsValuesNullInListAndOmitNoneFieldsFalse {
    JsonGenerator.options.OMIT_NONE_FIELDS = false
    val obj = jsonObject() {
      field("label", Some("value"))
      jsonArray("Cheeses") {
        jsonObject() {
          field("smell", None)
          field("texture", None)
        }
        jsonObject() {
          field("smell", None)
          field("texture", None)
        }
      }
    }
    assertEquals("""{"label":"value","Cheeses":null}""", obj.asString)
  }
}