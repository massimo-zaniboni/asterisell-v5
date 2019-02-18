
SELECT
	  `calldate`,
	  MOD(`id`, 1000000),
	  NOT(ISNULL('to_calldate')),
	  `to_calldate`,
	  `count_of_calls`,
	  `destination_type`,
	  `is_redirect`,
	  `duration`,
	  `billsec`,
	  `ar_organization_unit_id`,
	  `cached_parent_id_hierarchy`,
	  `billable_ar_organization_unit_id`,
	  `bundle_ar_organization_unit_id`,
	  `income`,
	  `cost_saving`,
	  `ar_vendor_id`,
	  `ar_communication_channel_type_id`,
	  `cost`,
	  `expected_cost`,
	  `ar_telephone_prefix_id`,
	  `cached_external_telephone_number`,
	  `external_telephone_number_with_applied_portability`,
	  `cached_masked_external_telephone_number`,
	  `error_destination_type`,
	  `ar_problem_duplication_key`,
	  `debug_cost_rate`,
	  `debug_income_rate`

  INTO OUTFILE '/var/tmp/ar_cdr.csv'
       CHARACTER SET 'utf8mb4'
       FIELDS TERMINATED BY '\t'
       OPTIONALLY ENCLOSED BY '"'
       LINES TERMINATED BY '\n'
  FROM ar_cdr
  ORDER BY calldate;


SELECT
	`telephone_number`,
	`from_date`,
	`ported_telephone_number`
INTO OUTFILE '/var/tmp/ar_number_portability.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_number_portability;

SELECT
	`id`,
	`name`,
	`compact_name`,
        '',
	`external_crm_code`,
        '',
	`vat`,
        '',
	`is_billable`,
	`legal_address`,
	`legal_city`,
	`legal_zipcode`,
	`legal_state_province`,
	`legal_country`,
	`email`,
        '',
	`phone`,
	`phone2`,
	`fax`,
        '',
	`max_limit_30`,
	`last_email_advise_for_max_limit_30`,
	`is_active`,
	`ar_reseller_id`,
	`migration_field_for_telephone`,
	`migration_field_for_adsl`,
	`payment_iban`,
	`payment_bic`,
	`payment_sepa`,
	`payment_info`
INTO OUTFILE '/var/tmp/ar_party.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_party;

SELECT
	`id`,
	`internal_name`,
	`note_for_admin`,
	`name_for_customer`,
	`note_for_customer`
INTO OUTFILE '/var/tmp/ar_tag.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_tag;

SELECT
	`ar_party_id`,
	`ar_tag_id`
INTO OUTFILE '/var/tmp/ar_party_has_tag.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_party_has_tag;


SELECT
	`id`,
	`name`,
	`is_default`,
	`service_name`,
	`service_provider_website`,
	`service_provider_email`,
	`vat_tax_perc`,
	`logo_image`,
	`slogan`,
	`logo_image_in_invoices`,
	`footer`,
	`user_message`,
	`legal_name`,
	`external_crm_code`,
	`vat`,
	`legal_address`,
	`legal_website`,
	`legal_city`,
	`legal_zipcode`,
	`legal_state_province`,
	`legal_country`,
	`legal_email`,
	`legal_phone`,
	`phone2`,
	`legal_fax`,
	`invoice_notes`,
	`invoice_payment_terms`,
	`invoice_payment_due_in_xx_days`,
	`sender_name_on_invoicing_emails`,
	`invoicing_email_address`,
	`logo_html_color`,
	`html_notes_on_the_login_form`,
	`official_calldate`,
	`scheduled_rerate_from_official_calldate`,
	`new_imported_cdrs_to_calldate`,
	`current_count_of_rerating_failed_attempts`,
	`current_rerating_event_is_running`,
	`should_reschedule_rerate_from_official_calldate`,
	`wait_for_scheduled_rerate`,
	`clean_error_table`
INTO OUTFILE '/var/tmp/ar_params.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_params;


SELECT
	`id`,
	`internal_name`,
	`name`,
	`note`
INTO OUTFILE '/var/tmp/ar_reseller.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_reseller;


SELECT
	`id`,
	`internal_name`,
	`short_description`
INTO OUTFILE '/var/tmp/ar_rate_category.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_rate_category;


SELECT
	`id`,
	`short_description`,
	`detailed_description`,
	`internal_name`,
	`order_name`
INTO OUTFILE '/var/tmp/ar_rate_format.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_rate_format;


SELECT
	`id`,
	`ar_vendor_id`,
	`ar_rate_format_id`,
	`from_time`,
	`internal_name`,
	`ar_rate_id`,
	`short_description`,
	`note`,
	`is_exported_to_resellers`,
	`was_compiled`,
	`source_data_file`,
	`backup_source_data_file`,
	`html_description`
INTO OUTFILE '/var/tmp/ar_rate.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_rate;


SELECT
	`ar_rate_id`,
	`ar_reseller_id`,
	`is_exported`,
	`id`
INTO OUTFILE '/var/tmp/ar_rate_shared_with_reseller.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_rate_shared_with_reseller;


SELECT
	`id`,
	`prefix`,
	`match_only_numbers_with_n_digits`,
	`name`,
	`geographic_location`,
	`operator_type`,
	`display_priority_level`
INTO OUTFILE '/var/tmp/ar_telephone_prefix.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_telephone_prefix;


SELECT
	`id`,
	`internal_name`,
	`internal_name2`,
        NULL,
        NULL,
        NULL,
        NULL,
	`export_code`,
	`automatically_managed_from`
INTO OUTFILE '/var/tmp/ar_organization_unit.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_organization_unit;


SELECT
	`id`,
	`name`,
	`short_code`,
	`internal_name`
INTO OUTFILE '/var/tmp/ar_organization_unit_type.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_organization_unit_type;


SELECT
	`id`,
	`ar_organization_unit_id`,
	`ar_organization_unit_type_id`,
	`ar_parent_organization_unit_id`,
	`from`,
	`exists`,
	`ar_rate_category_id`,
	`ar_party_id`,
	`extension_codes`,
	`extension_name`,
	`extension_user_code`
INTO OUTFILE '/var/tmp/ar_organization_unit_has_structure.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_organization_unit_has_structure;


SELECT
	`id`,
	`internal_name`,
	`ar_party_id`,
	`is_internal`
INTO OUTFILE '/var/tmp/ar_vendor.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_vendor;


SELECT
	`id`,
	`name`,
	`description`,
	`internal_name`
INTO OUTFILE '/var/tmp/ar_communication_channel_type.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_communication_channel_type;


SELECT
	`id`,
	`internal_name`,
	`ar_vendor_id`,
	`ar_communication_channel_type_id`,
	`domain`,
	`is_prefix`,
	`is_suffix`,
	`from`,
	`to`
INTO OUTFILE '/var/tmp/ar_vendor_domain.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_vendor_domain;


SELECT
	`id`,
	`ar_party_id`,
	`ar_organization_unit_id`,
	`login`,
	`password`,
  NULL,
	`is_enabled`,
	`is_root_admin`
INTO OUTFILE '/var/tmp/ar_user.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_user;


SELECT
	`id`,
	`ar_user_id`,
	`at_date`,
	`old_password`,
	`new_password`,
	`is_processed`
INTO OUTFILE '/var/tmp/ar_user_change_password_request.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_user_change_password_request;


SELECT
	`ar_user_id`,
	`ar_role_id`,
	`id`
INTO OUTFILE '/var/tmp/ar_user_has_role.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_user_has_role;


SELECT
	`ar_user_id`,
	`ar_permission_id`,
	`id`
INTO OUTFILE '/var/tmp/ar_user_has_permission.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_user_has_permission;


SELECT
	`id`,
	`name`,
	`power`,
	`description`,
	`internal_name`
INTO OUTFILE '/var/tmp/ar_role.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_role;


SELECT
	`ar_permission_id`,
	`ar_role_id`,
	`id`
INTO OUTFILE '/var/tmp/ar_role_has_permission.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_role_has_permission;


SELECT
	`id`,
	`show_call_cost`,
	`show_call_income`,
	`show_outgoing_calls`,
	`show_incoming_calls`,
	`show_internal_calls`,
	`show_voip_provider`,
	`show_communication_channel`,
	`show_cost_saving`
INTO OUTFILE '/var/tmp/ar_global_permissions.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_global_permissions;


SELECT
	`id`,
	`name`,
	`description`,
	`power`
INTO OUTFILE '/var/tmp/ar_permission.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_permission;


SELECT
	`id`,
	`name`,
	`description`
INTO OUTFILE '/var/tmp/ar_report_order_of_children.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_report_order_of_children;


SELECT
	`id`,
	`is_template`,
	`ar_report_set_id`,
	`about_ar_report_set_id`,
	`ar_organization_unit_id`,
	`ar_user_id`,
	`ar_vendor_id`,
	`ar_tag_id`,
	`from_date`,
	`to_date`,
	`param_show_masked_telephone_numbers`,
	`param_show_call_cost`,
	`param_show_call_income`,
	`param_show_also_outgoing_calls`,
	`param_show_also_system_calls`,
	`param_show_also_incoming_calls`,
	`param_show_also_internal_calls`,
	`param_show_call_details`,
	`param_show_voip_provider`,
	`param_show_communication_channel`,
	`param_show_geographic_location`,
	`param_show_connection_type`,
	`param_show_cost_saving`,
	`param_is_legal`,
	`param_expand_to_level`,
	`ar_report_order_of_children_id`,
	`php_class_name`,
	`produced_report_generation_date`,
	`report_name`,
	`produced_report_short_description`,
	`produced_report_additional_description`,
	`produced_report_already_reviewed`,
	`produced_report_is_draft`,
	`produced_report_must_be_regenerated`,
	`produced_report_mime_type`,
	`produced_report_file_type_suffix`,
	`produced_report_document`,
	`produced_report_document_checksum`,
	`report_mail_subject`,
	`report_mail_body`,
	`report_attachment_file_name`,
	`report_attachment_file_name_add_report_date`,
	`internal_name`,
	`cached_parent_id_hierarchy`,
	`legal_nr_prefix`,
	`legal_consecutive_nr`,
	`legal_date`,
	`legal_sender_name`,
	`legal_sender_vat`,
	`legal_sender_address`,
	`legal_receiver_name`,
	`legal_receiver_vat`,
	`legal_receiver_address`,
	IFNULL(total_without_tax, 0),
	IFNULL(tax, 0),
	IFNULL(applied_vat, 0),
	IFNULL(total_with_tax, 0)
INTO OUTFILE '/var/tmp/ar_report.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_report;


SELECT
	`ar_report_id`,
	`ar_role_id`
INTO OUTFILE '/var/tmp/ar_report_also_for.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_report_also_for;


SELECT
	`id`,
	`is_active`,
	`last_execution_date`,
	`last_from_date`,
	`last_to_date`,
	`ar_report_id`,
	`ar_organization_unit_id`,
	`short_description`,
	`additional_description`,
	`note`,
	`produced_report_must_be_reviewed`,
	`ar_report_generation_id`,
	`schedule_every_x_days`,
	`schedule_every_x_months`,
	`start_generation_after_x_hours`,
	`internal_name`,
	`ar_legal_date_generation_method_id`,
	`days_to_add_to_legal_date_generation_method`,
	`is_yearly_legal_numeration`,
	`generate_only_if_there_is_cost`,
	`minimum_cost`,
	`send_compact_report_list_to_accountant`
INTO OUTFILE '/var/tmp/ar_report_scheduler.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_report_scheduler;


SELECT
	`ar_report_set_id`,
	`ar_organization_unit_id`
INTO OUTFILE '/var/tmp/ar_postponed_report.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_postponed_report;


SELECT
	`ar_organization_unit_id`,
	`from_date`,
	`is_billed`,
	`is_processed`
INTO OUTFILE '/var/tmp/ar_postponed_report_tmp.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_postponed_report_tmp;


SELECT
	`id`,
	`name`,
	`description`
INTO OUTFILE '/var/tmp/ar_legal_date_generation_method.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_legal_date_generation_method;


SELECT
	`id`,
	`ar_report_scheduler_id`,
	`from_date`,
	`to_date`,
	`must_be_reviewed`,
	`postponed_fields_are_updated`,
	`postponed_reports`,
	`postponed_amount`,
	`reports`,
	`amount`
INTO OUTFILE '/var/tmp/ar_report_set.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_report_set;


SELECT
	`id`,
	`name`,
	`description`
INTO OUTFILE '/var/tmp/ar_report_generation.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_report_generation;


SELECT
	`id`,
	`ar_report_to_read_id`,
	`ar_report_id`,
	`ar_user_id`,
	`seen_or_received_from_user`,
	`ar_organization_unit_id`,
	`from_date`,
	`to_date`,
	`produced_report_generation_date`,
	`produced_report_short_description`,
	`produced_report_additional_description`,
	`produced_report_already_reviewed`,
	`produced_report_is_draft`
INTO OUTFILE '/var/tmp/ar_report_to_read_user_view.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_report_to_read_user_view;


SELECT
	`id`,
	`ar_report_id`,
	`ar_user_id`,
	`seen_or_received_from_user`,
	`must_be_sent_to_email`,
	`sent_to_email_at_date`
INTO OUTFILE '/var/tmp/ar_report_to_read.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_report_to_read;


 SELECT 
	  `ar_user_id`,
	  `ar_report_id`
 INTO OUTFILE '/var/tmp/ar_user_can_view_report.csv'
      CHARACTER SET 'utf8mb4'
      FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_user_can_view_report;


SELECT
	`id`,
	`name`,
	`description`
INTO OUTFILE '/var/tmp/ar_logical_source.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_logical_source;


SELECT
	`id`,
	`internal_name`,
	`description`,
        NULL,
        NULL
INTO OUTFILE '/var/tmp/ar_cdr_provider.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_cdr_provider;


SELECT
	`id`,
	`ar_logical_source_id`,
	`name`,
	`description`
INTO OUTFILE '/var/tmp/ar_physical_format.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_physical_format;


SELECT
	`id`,
	`ar_cdr_provider_id`,
	`ar_physical_format_id`,
	`retrieved_from_server`,
	`is_status`,
	`is_calldate_processed`,
	`is_imported`,
	`is_active_info`,
	`min_calldate`,
	`max_calldate`,
	`name`,
	`archive_directory`,
	`checksum`,
	`receiving_date`,
	`serious_processing_errors`,
	`tot_lines`,
	`lines_with_errors`
INTO OUTFILE '/var/tmp/ar_source_csv_file.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_source_csv_file;

SELECT
	`calldate`,
        MOD(`id`, 1000000),
	`ar_cdr_provider_id`,
	`ar_physical_format_id`,
	`content`
INTO OUTFILE '/var/tmp/ar_source_cdr.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_source_cdr
ORDER BY calldate;

SELECT
	`ar_cdr_provider_id`,
	`ar_physical_format_id`
INTO OUTFILE '/var/tmp/ar_type_of_source_cdr.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_type_of_source_cdr;


SELECT
	`id`,
	`ar_cdr_provider_id`,
	`name`,
	`checksum`,
	`receiving_date`
INTO OUTFILE '/var/tmp/ar_remote_file.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_remote_file;

SELECT
	`id`,
	`extension`,
	`pincode`,
	`from_date`
INTO OUTFILE '/var/tmp/ar_extension_to_pincode.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_extension_to_pincode;


SELECT
	`id`,
	`internal_name`,
	`customer_name`,
	`customer_description`,
	`vendor_name`,
	`vendor_description`,
	`external_crm_code`,
	`customer_price_depend_from_activation_date`,
	`customer_price_change_with_price_list`,
	`is_enabled`,
	`is_applied_only_one_time`,
	`schedule_timeframe`,
	`was_compiled`,
	`schedule_from`,
        '00:00:00'
INTO OUTFILE '/var/tmp/ar_service.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_service;


SELECT
	`id`,
	`internal_name`,
	`ar_service_id`,
	`from_date`,
	`price`
INTO OUTFILE '/var/tmp/ar_service_price.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_service_price;


SELECT
	`id`,
	`internal_name`,
	`external_crm_code`,
	`from_date`,
	`ar_service_id`,
	`ar_organization_unit_id`,
	`nr_of_items`,
	`discount`,
	`note`
INTO OUTFILE '/var/tmp/ar_assigned_service.csv'
     CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM ar_assigned_service;

