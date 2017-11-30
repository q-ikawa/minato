package minato.testApp

import minato.app.config.MinatoConfig.ObjectsMap
import minato.app.config._
import minato.app.infrastructure.JsonService
import minato.core.cargo._
import org.json4s.{JNothing, JNull, JValue}
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods.parse

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */
object TestApp extends App{
  override def main(args: Array[String]): Unit = {
    val conf = getConfig(
      """
        | {
        |   "Departures":
        |     {
        |       "name" : "stdin",
        |       "source":{
        |         "type" : "stdin-json"
        |       },
        |       "destinations" : ["port1","port2"]
        |     }
        |   ,
        |   "Ports" :
        |     {
        |       "name" : "port1",
        |       "mustache" : "{
        |         \"user-name\" : \"{{name}}\",
        |         \"age-of-user\" : \"{{age}}\"
        |       } ",
        |       "destinations" : ["terminal1"]
        |     }
        |   ,
        |   "Ports" :
        |     {
        |       "name" : "port2",
        |       "mustache" : "set {{name}}:favorite \"{{favorite}}\"",
        |       "destinations" : ["terminal2"]
        |     }
        |   ,
        |   "Terminals" :
        |     {
        |       "name" : "terminal1",
        |       "anchor" : {
        |         "type" : "stdout"
        |       }
        |     }
        |   ,
        |   "Terminals" :
        |     {
        |       "name" : "terminal2",
        |       "anchor" : {
        |         "type" : "stderr"
        |       }
        |     }
        |
        | }
      """.stripMargin)

    val x = MinatoConfigLoader.load(conf.get)


    x match {
      case Left(MinatoConfigParseException.MissingRefId(k,p)) => println(x+" "+ k + " " + p)
      case Left(MinatoConfigParseException.MissingKey(k,p)) => println(x+" " + k + " " + p)
      case Left(e) => e.printStackTrace()
      case Right(i) => i.cruise()
    }

  }

  def getConfig(string: String): Option[ConfigNode] = {
    val jsonObject = parse(string)
    Some(jsonToConfig("/",jsonObject))
  }

  def jsonToConfig(path:String,jvalue: JValue):ConfigNode = {
    jvalue match {
      case JBool(true)   => ConfigValue(path,"true")
      case JBool(false)  => ConfigValue(path,"false")
      case JDouble(n)    => ConfigValue(path,n.toString)
      case JDecimal(n)   => ConfigValue(path,n.toString)
      case JLong(n)      => ConfigValue(path,n.toString)
      case JInt(n)       => ConfigValue(path,n.toString)
      case JNull         => ConfigValue(path,"null")
      case JString(null) => ConfigValue(path,"null")
      case JString(s)    => ConfigValue(path,s)
      case JArray(arr)   => ConfigArray( path,arr.zipWithIndex.map(ji => jsonToConfig(path+ji._2+"/",ji._1)) )
      case JObject(obj)  => ConfigObjectImpl( path,obj.map( kv => kv._1 -> jsonToConfig(path + kv._1 +"/",kv._2) ) )
      case JSet(set)     => ???
      case JNothing      => ???
    }
  }

  case class ConfigObjectImpl(path:String,obj:Seq[(String,ConfigNode)]) extends ConfigObject {
    def get(key: String) = obj.filter(_._1 == key).map(_._2)
    def entrySet = obj
    def contains(key: String) = obj.exists(_._1 == key)

    val key: String = path

  }
}

