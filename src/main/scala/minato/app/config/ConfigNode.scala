package minato.app.config

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */
sealed trait ConfigNode {
  val path:String
}

case class ConfigValue(path:String,s:String) extends ConfigNode

trait ConfigObject extends ConfigNode{
  val key:String
  val path:String

  def getString(key:String) = get(key) match {
    case Seq(ConfigValue(_,s)) => s
  }

  def get(key:String) : Seq[ConfigNode]
  def contains(key:String) : Boolean
  def entrySet:Seq[(String,ConfigNode)]
}

case class ConfigArray(path:String,values :Seq[ConfigNode]) extends ConfigNode