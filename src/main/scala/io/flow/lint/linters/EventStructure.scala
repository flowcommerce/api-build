package io.flow.lint.linters

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.spec.v0.models.{Field, Model, Service}
import io.flow.lint.Linter

/** For event models (models ending with 'upserted', 'deleted'), validate:
  *
  *   a. upserted events have deleted events b. updated event models have an 'id' field c. deleted events have only an
  *      'id' d. no extra fields on the event
  */
case object EventStructure extends Linter with EventHelpers {

  override def validate(service: Service): Seq[String] = {
    findAllEvents(service)
      .map(filterLegacyModels)
      .flatMap(filterVerboseEventModels)
      .map { e => validate(service, e) }
      .sequence match {
      case Invalid(e) => e.toList.distinct
      case Valid(_) => Nil
    }
  }

  private[this] def validate(service: Service, event: EventInstance): ValidatedNec[String, Unit] = {
    (
      validateMatchingDeleteEvents(event),
      validateDeleteEventsHaveId(event.deleted),
      validateNoAdditionalFields(event),
      validateUpsertedModelsHaveId(service, event.upserted),
    ).mapN { case (_, _, _, _) => () }
  }

  private[this] def validateMatchingDeleteEvents(event: EventInstance): ValidatedNec[String, Unit] = {
    event.upserted
      .map { m =>
        validateMatchingDeleteEvent(m, event.deleted)
      }
      .sequence
      .map(_ => ())
  }

  private[this] def validateMatchingDeleteEvent(
    upserted: UpsertedEventModel,
    candidates: Seq[DeletedEventModel],
  ): ValidatedNec[String, Unit] = {
    candidates.find(_.prefix == upserted.prefix) match {
      case None => s"Missing delete event for '${upserted.model.name}'".invalidNec
      case Some(_) => ().validNec
    }
  }

  private[this] def validateDeleteEventsHaveId(models: Seq[DeletedEventModel]): ValidatedNec[String, Unit] = {
    models
      .map { m =>
        m.model.fields.find(_.name == "id") match {
          case None => s"Deleted event '${m.model.name}' is missing a field named 'id'".invalidNec
          case Some(f) => validateIdField(m.model, f)
        }
      }
      .sequence
      .map(_ => ())
  }

  private[this] def validateIdField(model: Model, field: Field): ValidatedNec[String, Unit] = {
    if (field.`type` == "string") {
      ().validNec
    } else {
      s"Model '${model.name}' Field '${field.name}' must have type 'string' and not '${field.`type`}'".invalidNec
    }
  }

  private[this] def validateNoAdditionalFields(event: EventInstance): ValidatedNec[String, Unit] = {
    (
      validateNoAdditionalFieldsUpserted(event.upserted),
      validateNoAdditionalFieldsDeleted(event.deleted),
    ).mapN { case (_, _) => () }
  }

  private[this] def validateNoAdditionalFieldsUpserted(models: Seq[UpsertedEventModel]): ValidatedNec[String, Unit] = {
    models
      .map { m =>
        validateNoAdditionalFields(m, Seq(m.prefix) ++ m.prefix.split("_").toList)
      }
      .sequence
      .map(_ => ())
  }

  private[this] def validateNoAdditionalFieldsDeleted(models: Seq[DeletedEventModel]): ValidatedNec[String, Unit] = {
    models
      .map { m =>
        validateNoAdditionalFields(m, Seq("id"))
      }
      .sequence
      .map(_ => ())
  }

  private[this] def validateNoAdditionalFields(
    m: EventModel,
    acceptableFinalFieldNames: Seq[String],
  ): ValidatedNec[String, Unit] = {
    m.model.fields.zipWithIndex
      .map { case (field, i) =>
        i match {
          case 0 => validateFieldName(m, field, Seq("event_id"))
          case 1 => validateFieldName(m, field, Seq("timestamp"))
          case 2 =>
            validateFieldName(
              m,
              field,
              acceptableFinalFieldNames ++ Seq("organization", "channel", "channel_id", "partner"),
            )
          case 3 => validateFieldName(m, field, acceptableFinalFieldNames)
          case _ => error(m.model, "Cannot have more than 4 fields").invalidNec
        }
      }
      .sequence
      .map(_ => ())
  }

  private[this] def validateFieldName(
    model: EventModel,
    field: Field,
    allowed: Seq[String],
  ): ValidatedNec[String, Unit] = {
    if (allowed.contains(field.name)) {
      ().validNec
    } else {
      invalidFieldError(model, field, allowed).invalidNec
    }
  }
  private[this] def invalidFieldError(model: EventModel, field: Field, allowed: Seq[String]): String = {
    val msg = allowed.distinct.toList match {
      case one :: Nil => s"Must be '$one'"
      case all => "Must be one of: " + all.mkString(", ")
    }
    error(model.model, field, s"Invalid name '${field.name}'. $msg")
  }

  private[this] def validateUpsertedModelsHaveId(
    service: Service,
    models: Seq[UpsertedEventModel],
  ): ValidatedNec[String, Unit] = {
    models
      .flatMap { m =>
        m.model.fields.lastOption
          .map(_.`type`)
          .flatMap { t =>
            service.models.find(_.name == t)
          }
          .map { underlyingModel =>
            underlyingModel.fields.find(_.name == "id") match {
              case None =>
                s"Model '${underlyingModel.name}' is missing a field named 'id' - this is required as part of the upserted event '${m.model.name}'".invalidNec
              case Some(f) => validateIdField(underlyingModel, f)
            }
          }
      }
      .sequence
      .map(_ => ())
  }

  private[this] def filterLegacyModels(event: EventInstance): EventInstance = {
    event.copy(
      models = event.models.flatMap { m =>
        Some(m).filterNot(_ => LegacyInvalidModels.contains(m.model.name))
      },
    )
  }

  private val FilteredVerboseEventModels = Set(
    "catalog_item_event",
  )

  // Verbose event models are those with Inserted/Updated/Deleted events (as opposed to Upserted/Deleted).
  // We can write a validator for those later if satisfied by this approach.
  // In addition, could we add an annotation to the spec to indicate that we want the verbose model?
  private[this] def filterVerboseEventModels(event: EventInstance): Option[EventInstance] = {
    Some(event).filterNot(e => FilteredVerboseEventModels.contains(e.union.name))
  }

  private[this] val LegacyInvalidModels = Set(
    "adyen_authorization_deleted",
    "adyen_authorization_upserted",
    "adyen_cancel_deleted",
    "adyen_cancel_upserted",
    "adyen_capture_deleted",
    "adyen_capture_upserted",
    "adyen_merchant_account_deleted",
    "adyen_merchant_account_upserted",
    "adyen_refund_deleted",
    "adyen_refund_upserted",
    "afterpay_authorization_deleted",
    "afterpay_authorization_upserted",
    "afterpay_capture_deleted",
    "afterpay_capture_upserted",
    "afterpay_refund_deleted",
    "afterpay_refund_upserted",
    "attribute_deleted",
    "attribute_upserted",
    "available_promotions_deleted",
    "available_promotions_deleted_v2",
    "available_promotions_upserted",
    "available_promotions_upserted_v2",
    "b2b_credit_memo_deleted",
    "b2b_credit_memo_upserted",
    "b2b_invoice_deleted",
    "b2b_invoice_upserted",
    "bank_payment_deleted",
    "bank_payment_upserted",
    "capture_deleted",
    "capture_deleted_v2",
    "capture_identifier_deleted",
    "capture_identifier_upserted",
    "capture_upserted",
    "capture_upserted_v2",
    "card_authorization_deleted_v2",
    "card_authorization_upserted_v2",
    "card_deleted_v2",
    "card_upserted_v2",
    "catalog_deleted",
    "catalog_item_deleted",
    "catalog_item_deleted_v2",
    "catalog_item_upserted",
    "catalog_item_upserted_v2",
    "catalog_upserted",
    "center_deleted",
    "center_upserted",
    "channel_order_acceptance_deleted",
    "channel_order_acceptance_upserted",
    "channel_statement_deleted",
    "channel_statement_upserted",
    "channel_transaction_deleted",
    "channel_transaction_deleted_v2",
    "channel_transaction_upserted",
    "channel_transaction_upserted_v2",
    "chargeback_deleted",
    "chargeback_upserted",
    "checkout_configuration_deleted",
    "checkout_configuration_upserted",
    "consumer_invoice_deleted",
    "consumer_invoice_upserted",
    "country_status_deleted",
    "country_status_upserted",
    "credit_memo_deleted",
    "credit_memo_upserted",
    "crossdock_shipment_deleted",
    "crossdock_shipment_upserted",
    "currency_format_deleted",
    "currency_format_upserted",
    "customer_address_book_contact_deleted",
    "customer_address_book_contact_upserted",
    "customer_deleted",
    "customer_purge_deleted",
    "customer_purge_upserted",
    "customer_upserted",
    "daily_experiment_results_deleted",
    "daily_experiment_results_upserted",
    "ecommerce_platform_deleted",
    "ecommerce_platform_upserted",
    "email_notification_deleted",
    "email_notification_upserted",
    "exclusion_rule_deleted",
    "exclusion_rule_upserted",
    "experience_deleted",
    "experience_deleted_v2",
    "experience_logistics_settings_deleted",
    "experience_logistics_settings_upserted",
    "experience_upserted",
    "experience_upserted_v2",
    "experiment_deleted",
    "experiment_results_deleted",
    "experiment_results_upserted",
    "experiment_upserted",
    "feature_deleted",
    "feature_upserted",
    "feed_deleted",
    "feed_upserted",
    "fraud_pending_review_deleted",
    "fraud_pending_review_upserted",
    "fraud_provider_configuration_deleted",
    "fraud_provider_configuration_upserted",
    "fraud_review_decision_deleted",
    "fraud_review_decision_upserted",
    "fully_harmonized_item_deleted",
    "fully_harmonized_item_upserted",
    "harmonization_item_classification_deleted",
    "harmonization_item_classification_upserted",
    "harmonized_landed_cost_deleted",
    "harmonized_landed_cost_upserted",
    "hs10_code_deleted",
    "hs10_code_upserted",
    "hs6_code_deleted",
    "hs6_code_upserted",
    "internal_authorization_deleted",
    "internal_authorization_upserted",
    "issuer_deleted",
    "issuer_upserted",
    "item_harmonization_deleted",
    "item_harmonization_upserted",
    "item_margin_deleted_v2",
    "item_margin_upserted_v2",
    "item_origin_deleted",
    "item_origin_upserted",
    "item_sales_margin_deleted",
    "item_sales_margin_upserted",
    "label_deleted",
    "label_deleted_v2",
    "label_format_deleted",
    "label_format_upserted",
    "label_upserted",
    "label_upserted_v2",
    "levy_rate_summary_deleted",
    "levy_rate_summary_upserted",
    "liability_remittance_plan_deleted",
    "liability_remittance_plan_upserted",
    "local_item_deleted",
    "local_item_upserted",
    "localization_deleted",
    "localization_upserted",
    "localized_content_deleted",
    "localized_content_upserted",
    "localized_item_deleted",
    "localized_item_deleted_v2",
    "localized_item_upserted",
    "localized_item_upserted_v2",
    "main_transaction_deleted",
    "main_transaction_upserted",
    "manifested_label_deleted",
    "manifested_label_upserted",
    "membership_deleted_v2",
    "membership_upserted_v2",
    "merchant_application_deleted",
    "merchant_application_upserted",
    "notification_deleted_v2",
    "notification_upserted_v2",
    "online_authorization_deleted_v2",
    "online_authorization_upserted_v2",
    "optin_prompt_deleted",
    "optin_prompt_upserted",
    "order_attribute_deleted",
    "order_attribute_upserted",
    "order_deleted",
    "order_deleted_v2",
    "order_fulfillment_deleted",
    "order_fulfillment_upserted",
    "order_identifier_deleted",
    "order_identifier_deleted_v2",
    "order_identifier_upserted",
    "order_identifier_upserted_v2",
    "order_identifier_upserted_v3",
    "order_replacement_deleted",
    "order_replacement_upserted",
    "order_upserted",
    "order_upserted_v2",
    "organization_boolean_value_deleted",
    "organization_boolean_value_upserted",
    "organization_business_entity_deleted",
    "organization_business_entity_upserted",
    "organization_default_configurations_deleted",
    "organization_default_configurations_upserted",
    "organization_deleted",
    "organization_deleted_v2",
    "organization_onboarding_state_deleted",
    "organization_onboarding_state_upserted",
    "organization_restriction_approval_deleted",
    "organization_restriction_approval_upserted",
    "organization_restriction_snapshot_deleted",
    "organization_restriction_snapshot_upserted",
    "organization_upserted",
    "organization_upserted_v2",
    "payment_deleted",
    "payment_processor_account_deleted",
    "payment_processor_account_upserted",
    "payment_processor_merchant_deleted",
    "payment_processor_merchant_upserted",
    "payment_request_deleted",
    "payment_request_upserted",
    "payment_upserted",
    "paypal_execution_deleted",
    "paypal_execution_upserted",
    "paypal_payment_deleted",
    "paypal_payment_upserted",
    "paypal_refund_deleted",
    "paypal_refund_upserted",
    "pricing_deleted",
    "pricing_upserted",
    "processing_transaction_deleted",
    "processing_transaction_upserted",
    "product_restriction_result_deleted",
    "product_restriction_result_upserted",
    "rate_deleted",
    "rate_deleted_v2",
    "rate_deleted_v3",
    "rate_freshness_summary_deleted",
    "rate_freshness_summary_upserted",
    "rate_source_summary_deleted",
    "rate_source_summary_upserted",
    "rate_upserted",
    "rate_upserted_v2",
    "rate_upserted_v3",
    "ratecard_deleted",
    "ratecard_dimension_estimate_deleted",
    "ratecard_dimension_estimate_upserted",
    "ratecard_lane_aggregate_deleted",
    "ratecard_lane_aggregate_upserted",
    "ratecard_rate_level_deleted",
    "ratecard_rate_level_organization_deleted",
    "ratecard_rate_level_organization_upserted",
    "ratecard_rate_level_ratecard_deleted",
    "ratecard_rate_level_ratecard_upserted",
    "ratecard_rate_level_upserted",
    "ratecard_service_fee_deleted",
    "ratecard_service_fee_upserted",
    "ratecard_upserted",
    "refund_capture_deleted_v2",
    "refund_capture_upserted_v2",
    "refund_deleted_v2",
    "refund_identifier_deleted",
    "refund_identifier_upserted",
    "refund_upserted_v2",
    "restriction_organization_status_deleted",
    "restriction_organization_status_upserted",
    "return_deleted",
    "return_deleted_v2",
    "return_policy_deleted",
    "return_policy_item_result_deleted",
    "return_policy_item_result_upserted",
    "return_policy_upserted",
    "return_upserted",
    "return_upserted_v2",
    "reversal_deleted",
    "reversal_upserted",
    "rule_deleted",
    "rule_upserted",
    "shipping_configuration_item_availability_deleted",
    "shipping_configuration_item_availability_upserted",
    "shipping_configuration_item_shipping_pricing_deleted",
    "shipping_configuration_item_shipping_pricing_upserted",
    "shipping_lane_deleted",
    "shipping_lane_upserted",
    "shopify_experience_short_id_deleted",
    "shopify_experience_short_id_upserted",
    "shopify_localization_setting_deleted",
    "shopify_localization_setting_upserted",
    "shopify_markets_order_deleted",
    "shopify_markets_order_upserted",
    "shopify_monitoring_order_monitor_event_deleted",
    "shopify_monitoring_order_monitor_event_upserted",
    "snapshot_deleted",
    "snapshot_upserted",
    "statement_deleted",
    "statement_upserted",
    "stripe_authorization_deleted",
    "stripe_authorization_upserted",
    "stripe_capture_deleted",
    "stripe_capture_upserted",
    "stripe_refund_deleted",
    "stripe_refund_upserted",
    "stripe_reversal_deleted",
    "stripe_reversal_upserted",
    "subcatalog_deleted",
    "subcatalog_item_deleted",
    "subcatalog_item_upserted",
    "subcatalog_upserted",
    "submitted_order_deleted",
    "submitted_order_upserted",
    "svb_virtual_card_clearing_deleted",
    "svb_virtual_card_clearing_upserted",
    "test_deleted",
    "test_upserted",
    "tier_deleted_v2",
    "tier_upserted_v2",
    "time_to_classify_aggregated_deleted",
    "time_to_classify_aggregated_upserted",
    "time_to_classify_deleted",
    "time_to_classify_upserted",
    "tracking_deleted",
    "tracking_label_deleted",
    "tracking_label_event_deleted",
    "tracking_label_event_deleted_v2",
    "tracking_label_event_upserted",
    "tracking_label_event_upserted_v2",
    "tracking_label_upserted",
    "tracking_request_deleted",
    "tracking_request_upserted",
    "tracking_response_deleted",
    "tracking_response_upserted",
    "tracking_upserted",
    "transaction_deleted",
    "transaction_upserted",
    "transfer_transaction_deleted",
    "transfer_transaction_upserted",
    "user_deleted_v2",
    "user_upserted_v2",
    "virtual_card_capture_deleted",
    "virtual_card_capture_upserted",
    "virtual_card_provider_deleted",
    "virtual_card_provider_upserted",
    "virtual_card_refund_deleted",
    "virtual_card_refund_upserted",
  )
}
