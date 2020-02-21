package io.flow.lint.linters

import io.apibuilder.spec.v0.models.{Model, Service}
import io.flow.lint.Linter
import io.flow.stream.EventUnionTypeMatcher

/**
  * Match naming convention required to get events into s3
  */
case object UpsertedDeletedEventModels extends Linter with Helpers {

  override def validate(service: Service): Seq[String] = {
    service.models.
      filterNot { m => LegacyModels.contains(m.name) }.
      flatMap { m =>
      expectedFieldName(m.name).toSeq.flatMap { name =>
        validateModel(m, name)
      }
    }
  }

  def expectedFieldName(name: String): Option[String] = {
    val i = name.indexOf("_upserted")
    if (i > 0) {
      Some(name.take(i))
    } else {
      val j = name.indexOf("_deleted")
      if (j > 0) {
        Some(name.take(j))
      } else {
        None
      }
    }
  }

  def validateModel(model: Model, expected: String): Seq[String] = {
    if (model.fields.exists { f =>
      EventUnionTypeMatcher.matchFieldToPayloadType(f, expected)
    }) {
      Nil
    } else {
      Seq(
        error(model, s"Event must contain a field whose name and type contain " + expected.split("_").mkString(" or "))
      )
    }
  }

  private[this] val LegacyModels = Set(
    "allocation_deleted_v2", "attribute_deleted", "attribute_deleted_v2", "attribute_upserted", "authorization_deleted_v2", "available_promotions_deleted", "available_promotions_deleted_v2", "available_promotions_upserted", "available_promotions_upserted_v2", "browse_optin_responses_deleted", "card_deleted", "catalog_deleted", "catalog_item_deleted", "catalog_item_upserted", "catalog_upserted", "checkout_optin_responses_deleted", "currency_format_deleted", "currency_format_upserted", "delivery_option_deleted", "delivery_option_upserted", "experience_deleted", "experience_price_book_mapping_deleted", "experience_upserted", "fully_harmonized_item_upserted", "harmonized_item_deleted", "harmonized_item_upserted", "hs10_code_deleted", "hs10_code_upserted", "hs6_code_deleted", "hs6_code_upserted", "item_margin_deleted", "item_margin_upserted", "item_origin_deleted", "item_origin_upserted", "item_sales_margin_deleted", "item_sales_margin_upserted", "label_format_deleted", "label_format_upserted", "label_upserted", "manifested_label_deleted", "manifested_label_upserted", "notification_deleted", "notification_upserted", "order_deleted", "order_identifier_deleted", "order_identifier_deleted_v2", "order_identifier_upserted", "order_identifier_upserted_v2", "order_upserted", "organization_deleted", "organization_upserted", "payment_deleted", "price_book_deleted", "price_book_item_deleted", "pricing_deleted", "pricing_upserted", "rate_deleted", "rate_upserted", "return_deleted", "return_upserted", "rule_deleted", "serial_deleted", "shipping_configuration_deleted", "snapshot_deleted", "snapshot_upserted", "subcatalog_deleted", "subcatalog_item_deleted", "subcatalog_item_upserted", "subcatalog_upserted", "targeting_item_deleted", "targeting_item_deleted_v3", "targeting_item_upserted", "task_item_upserted", "tier_deleted", "tier_upserted", "tracking_label_event_upserted", "virtual_card_capture_deleted", "virtual_card_refund_deleted",
    "localization_upserted", "localized_content_upserted", "spot_rate_deleted", "duty_raw_bulk_upserted", "order_attribute_deleted", "experiment_deleted", "fraud_pending_review_deleted", "fraud_review_decision_deleted", "fraud_review_deleted", "ftp_file_deleted", "ratecard_upserted", "ratecard_lane_deleted", "blacklisted_item_deleted", "item_dimension_estimate_deleted", "item_dimension_estimate_upserted", "label_invoice_deleted", "label_invoice_upserted", "localized_item_deleted", "localized_item_deleted_v2", "optin_prompt_deleted", "order_fulfillment_deleted", "order_fulfillment_upserted", "lane_deleted", "lane_upserted", "ratecard_rate_deleted", "ratecard_rate_upserted", "shopify_shop_deleted", "user_deleted", "user_upserted",
    "tax_remittance_transaction_deleted",
    "item_dimension_estimate_deleted_v2",
    "ratecard_standard_configuration_deleted"
  )
}
