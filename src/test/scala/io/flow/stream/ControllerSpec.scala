package io.flow.stream

import io.apibuilder.spec.v0.models.{Field, Model}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ControllerSpec extends AnyFunSpec with Matchers {

  private def makeModel(name: String, fields: Seq[Field]): Model = {
    Model(
      name = name,
      plural = name + "s",
      fields = fields,
    )
  }

  private def makeIdField(name: String = "id"): Field = {
    Field(name = name, `type` = "string", required = true)
  }

  describe("CapturedType JSON serialization") {
    val controller = Controller()
    import controller.capturedTypeWrites

    it("serializes with single upsertedDiscriminator for backward compatibility") {
      val capturedType = CapturedType(
        fieldName = "order",
        typeName = "order",
        modelType = makeModel("order", Seq(makeIdField())),
        upsertedDiscriminators = Seq("order_upserted"),
        deletedDiscriminator = "order_deleted",
        deletedHasModel = true,
      )

      val json = Json.toJson(capturedType)

      (json \ "upsertedDiscriminator").as[String] shouldBe "order_upserted"
      (json \ "upsertedDiscriminators").as[Seq[String]] shouldBe Seq("order_upserted")
      (json \ "fieldName").as[String] shouldBe "order"
      (json \ "typeName").as[String] shouldBe "order"
      (json \ "deletedDiscriminator").as[String] shouldBe "order_deleted"
      (json \ "deletedHasModel").as[Boolean] shouldBe true
    }

    it("serializes multiple upsertedDiscriminators (inserted + updated)") {
      val capturedType = CapturedType(
        fieldName = "item",
        typeName = "item",
        modelType = makeModel("item", Seq(makeIdField())),
        upsertedDiscriminators = Seq("item_inserted", "item_updated"),
        deletedDiscriminator = "item_deleted",
        deletedHasModel = false,
      )

      val json = Json.toJson(capturedType)

      // Backward compatibility: first discriminator is used
      (json \ "upsertedDiscriminator").as[String] shouldBe "item_inserted"
      // New format: all discriminators included
      (json \ "upsertedDiscriminators").as[Seq[String]] shouldBe Seq("item_inserted", "item_updated")
    }
  }

  describe("UnionMemberRx pattern matching") {
    val controller = Controller()
    import controller.UnionMemberRx

    it("matches upserted events") {
      val UnionMemberRx(typeName, eventType, suffix) = "order_upserted"
      typeName shouldBe "order"
      eventType shouldBe "upserted"
      suffix shouldBe ""
    }

    it("matches inserted events") {
      val UnionMemberRx(typeName, eventType, suffix) = "order_inserted"
      typeName shouldBe "order"
      eventType shouldBe "inserted"
      suffix shouldBe ""
    }

    it("matches updated events") {
      val UnionMemberRx(typeName, eventType, suffix) = "order_updated"
      typeName shouldBe "order"
      eventType shouldBe "updated"
      suffix shouldBe ""
    }

    it("matches deleted events") {
      val UnionMemberRx(typeName, eventType, suffix) = "order_deleted"
      typeName shouldBe "order"
      eventType shouldBe "deleted"
      suffix shouldBe ""
    }

    it("matches events with suffix") {
      val UnionMemberRx(typeName, eventType, suffix) = "order_upserted_v2"
      typeName shouldBe "order"
      eventType shouldBe "upserted"
      suffix shouldBe "v2"
    }

    it("matches compound type names") {
      val UnionMemberRx(typeName, eventType, suffix) = "shopping_cart_item_inserted"
      typeName shouldBe "shopping_cart_item"
      eventType shouldBe "inserted"
      suffix shouldBe ""
    }

    it("does not match invalid event types") {
      "order_created" match {
        case UnionMemberRx(_, _, _) => fail("Should not match 'created' event type")
        case _ => succeed
      }
    }
  }

  describe("pairUpEvents") {
    val controller = Controller()

    def makeInserted(
      typeName: String,
      discriminator: String,
      payloadName: Option[String] = None,
    ): EventType.Inserted = {
      val model = makeModel(payloadName.getOrElse(typeName), Seq(makeIdField()))
      EventType.Inserted(
        eventName = s"${typeName}_inserted",
        typeName = typeName,
        fieldName = typeName,
        payloadType = model,
        idField = makeIdField(),
        discriminator = discriminator,
      )
    }

    def makeUpdated(
      typeName: String,
      discriminator: String,
      payloadName: Option[String] = None,
    ): EventType.Updated = {
      val model = makeModel(payloadName.getOrElse(typeName), Seq(makeIdField()))
      EventType.Updated(
        eventName = s"${typeName}_updated",
        typeName = typeName,
        fieldName = typeName,
        payloadType = model,
        idField = makeIdField(),
        discriminator = discriminator,
      )
    }

    def makeUpserted(typeName: String, discriminator: String): EventType.Upserted = {
      val model = makeModel(typeName, Seq(makeIdField()))
      EventType.Upserted(
        eventName = s"${typeName}_upserted",
        typeName = typeName,
        fieldName = typeName,
        payloadType = model,
        idField = makeIdField(),
        discriminator = discriminator,
      )
    }

    def makeDeleted(
      typeName: String,
      discriminator: String,
      hasPayload: Boolean = true,
      payloadName: Option[String] = None,
    ): EventType.Deleted = {
      val model = makeModel(payloadName.getOrElse(typeName), Seq(makeIdField()))
      EventType.Deleted(
        eventName = s"${typeName}_deleted",
        typeName = typeName,
        payloadType = if (hasPayload) Some(model) else None,
        idField = makeIdField(),
        discriminator = discriminator,
      )
    }

    it("pairs upserted events with deleted events") {
      val upserted = List(makeUpserted("order", "order_upserted"))
      val deleted = List(makeDeleted("order", "order_deleted"))

      val result = controller.pairUpEvents(Nil, Nil, upserted, deleted)

      result should have size 1
      result.head.typeName shouldBe "order"
      result.head.upsertedDiscriminators shouldBe Seq("order_upserted")
      result.head.deletedDiscriminator shouldBe "order_deleted"
      result.head.deletedHasModel shouldBe true
    }

    it("merges inserted and updated events for the same type and payload") {
      val inserted = List(makeInserted("item", "item_inserted"))
      val updated = List(makeUpdated("item", "item_updated"))
      val deleted = List(makeDeleted("item", "item_deleted"))

      val result = controller.pairUpEvents(inserted, updated, Nil, deleted)

      result should have size 1
      result.head.typeName shouldBe "item"
      result.head.upsertedDiscriminators should contain allOf ("item_inserted", "item_updated")
      result.head.deletedDiscriminator shouldBe "item_deleted"
    }

    it("does not merge inserted and updated events with different payloads") {
      val inserted = List(makeInserted("item", "item_inserted", payloadName = Some("item_summary")))
      val updated = List(makeUpdated("item", "item_updated", payloadName = Some("item_full")))
      val deleted = List(
        makeDeleted("item", "item_deleted_1"),
        makeDeleted("item", "item_deleted_2"),
      )

      val result = controller.pairUpEvents(inserted, updated, Nil, deleted)

      // Should get two separate CapturedTypes since payloads differ
      result should have size 2
      result.foreach { ct =>
        ct.upsertedDiscriminators should have size 1
      }
    }

    it("keeps upserted events separate from inserted/updated") {
      // Given: an inserted event and an upserted event for the same type "item"
      val inserted = List(makeInserted("item", "item_inserted"))
      val upserted = List(makeUpserted("item", "item_upserted"))
      // And: two deleted events to pair with them
      val deleted = List(
        makeDeleted("item", "item_deleted_1"),
        makeDeleted("item", "item_deleted_2"),
      )

      val result = controller.pairUpEvents(inserted, Nil, upserted, deleted)

      // Then: we get two separate CapturedTypes (not merged into one)
      result should have size 2

      // And: each CapturedType has exactly one discriminator (not merged)
      result.foreach { ct =>
        ct.upsertedDiscriminators should have size 1
      }

      // And: both discriminators are present across the results
      val allDiscriminators = result.flatMap(_.upsertedDiscriminators).toSet
      allDiscriminators shouldBe Set("item_inserted", "item_upserted")

      // And: each is paired with a different deleted event
      val deletedDiscriminators = result.map(_.deletedDiscriminator).toSet
      deletedDiscriminators should have size 2
    }

    it("handles multiple different types") {
      val inserted = List(
        makeInserted("order", "order_inserted"),
        makeInserted("item", "item_inserted"),
      )
      val deleted = List(
        makeDeleted("order", "order_deleted"),
        makeDeleted("item", "item_deleted"),
      )

      val result = controller.pairUpEvents(inserted, Nil, Nil, deleted)

      result should have size 2
      result.map(_.typeName).toSet shouldBe Set("order", "item")
    }

    it("returns empty list when no events to pair") {
      val result = controller.pairUpEvents(Nil, Nil, Nil, Nil)
      result shouldBe empty
    }

    it("skips upsert events without matching deleted event") {
      val upserted = List(makeUpserted("order", "order_upserted"))
      val deleted = List(makeDeleted("item", "item_deleted")) // different type

      val result = controller.pairUpEvents(Nil, Nil, upserted, deleted)

      result shouldBe empty
    }

    it("prefers deleted events with payload type") {
      val upserted = List(makeUpserted("order", "order_upserted"))
      val deleted = List(
        makeDeleted("order", "order_deleted_no_payload", hasPayload = false),
        makeDeleted("order", "order_deleted_with_payload", hasPayload = true),
      )

      val result = controller.pairUpEvents(Nil, Nil, upserted, deleted)

      result should have size 1
      result.head.deletedDiscriminator shouldBe "order_deleted_with_payload"
      result.head.deletedHasModel shouldBe true
    }

    it("prefers deleted events with matching payload name") {
      val inserted = List(makeInserted("item", "item_inserted", payloadName = Some("item_summary")))
      val deleted = List(
        makeDeleted("item", "item_deleted_full", payloadName = Some("item_full")),
        makeDeleted("item", "item_deleted_summary", payloadName = Some("item_summary")),
      )

      val result = controller.pairUpEvents(inserted, Nil, Nil, deleted)

      result should have size 1
      result.head.deletedDiscriminator shouldBe "item_deleted_summary"
    }
  }

}
