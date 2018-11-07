package io.flow.stream

import io.apibuilder.spec.v0.models.Field

object EventUnionTypeMatcher {

  def matchFieldToPayloadType(field: Field, typeName: String): Boolean = {
    matchFileName(typeName, field.name) && matchFieldType(typeName, field.`type`)
  }

  private def matchFileName(typeName: String, fieldName: String): Boolean = {
    val typeNameList = typeName.split("_").filter(_.nonEmpty).toList
    val fieldNameList = fieldName.split("_").filter(_.nonEmpty).toList
    matchLists(fieldNameList, typeNameList)
  }

  private def matchFieldType(typeName: String, fieldType: String): Boolean = {
    val simpleType = fieldType.reverse.takeWhile(_ != '.').reverse
    val fieldTypeList = simpleType.split("_").filter(_.nonEmpty).toList
    val typeNameList = typeName.split("_").filter(_.nonEmpty).toList
    matchLists(typeNameList, fieldTypeList) || matchLists(fieldTypeList, typeNameList)
  }

  private def matchLists(required: List[String], withExtras: List[String]): Boolean = (required, withExtras) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (head :: tail, _) =>
      val shortened = withExtras.dropWhile(_ != head)
      if (shortened.isEmpty) {
        false
      } else {
        matchLists(tail, shortened.drop(1))
      }
  }
}
