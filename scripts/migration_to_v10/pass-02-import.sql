DROP TRIGGER IF EXISTS invalidate_cached_ar_assigned_service_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_assigned_service_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_assigned_service_DELETE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_communication_channel_type_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_communication_channel_type_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_communication_channel_type_DELETE;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_daily_status_job;
DROP TRIGGER IF EXISTS invalidate_cached_ar_holiday_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_holiday_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_holiday_DELETE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_number_portability_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_number_portability_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_number_portability_DELETE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_organization_unit_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_organization_unit_UPDATE;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_organization_unit;
DROP TRIGGER IF EXISTS invalidate_cached_ar_organization_unit_DELETE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_organization_unit_has_structure_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_organization_unit_has_structure_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_organization_unit_has_structure_DELETE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_organization_unit_type_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_organization_unit_type_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_organization_unit_type_DELETE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_party_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_party_UPDATE;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_party;
DROP TRIGGER IF EXISTS invalidate_cached_ar_party_DELETE;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_permission;
DROP TRIGGER IF EXISTS invalidate_cached_ar_rate_INSERT;
DROP TRIGGER IF EXISTS compile_rate_trigger;
DROP TRIGGER IF EXISTS invalidate_cached_ar_rate_UPDATE;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_rate;
DROP TRIGGER IF EXISTS invalidate_cached_ar_rate_DELETE;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_1;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_2;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_report;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_2d;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_report_set;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_reseller;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_role;
DROP TRIGGER IF EXISTS invalidate_cached_ar_service_INSERT;
DROP TRIGGER IF EXISTS send_service_trigger;
DROP TRIGGER IF EXISTS invalidate_cached_ar_service_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_service_DELETE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_service_price_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_service_price_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_service_price_DELETE;
DROP TRIGGER IF EXISTS type_of_source_cdr;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_tag;
DROP TRIGGER IF EXISTS invalidate_cached_ar_telephone_prefix_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_telephone_prefix_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_telephone_prefix_DELETE;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_3;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_4;
DROP TRIGGER IF EXISTS trigger_cascade_on_ar_user;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_4b;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_6;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_5;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_7;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_9;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_8;
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_10;
DROP TRIGGER IF EXISTS invalidate_cached_ar_vendor_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_vendor_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_vendor_DELETE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_vendor_domain_INSERT;
DROP TRIGGER IF EXISTS invalidate_cached_ar_vendor_domain_UPDATE;
DROP TRIGGER IF EXISTS invalidate_cached_ar_vendor_domain_DELETE;


TRUNCATE TABLE ar_cdr;

LOAD DATA INFILE '/var/tmp/ar_cdr.csv'
REPLACE INTO TABLE ar_cdr
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(	  `calldate`,
	  `id`,
	   is_service_cdr,
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
);
TRUNCATE TABLE ar_number_portability;

LOAD DATA INFILE '/var/tmp/ar_number_portability.csv'
REPLACE INTO TABLE ar_number_portability
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`telephone_number`,
	`from_date`,
	`ported_telephone_number`
);
TRUNCATE TABLE ar_party;

LOAD DATA INFILE '/var/tmp/ar_party.csv'
REPLACE INTO TABLE ar_party
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`name`,
	`compact_name`,
        `note`,
	`external_crm_code`,
        `contract_number`,
	`vat`,
        `legal_registration_number`,
	`is_billable`,
	`legal_address`,
	`legal_city`,
	`legal_zipcode`,
	`legal_state_province`,
	`legal_country`,
	`email`,
        `contact_name`,
	`phone`,
	`phone2`,
	`fax`,
        web_site,
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
);
TRUNCATE TABLE ar_tag;

LOAD DATA INFILE '/var/tmp/ar_tag.csv'
REPLACE INTO TABLE ar_tag
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`internal_name`,
	`note_for_admin`,
	`name_for_customer`,
	`note_for_customer`
);
TRUNCATE TABLE ar_party_has_tag;

LOAD DATA INFILE '/var/tmp/ar_party_has_tag.csv'
REPLACE INTO TABLE ar_party_has_tag
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`ar_party_id`,
	`ar_tag_id`
);

TRUNCATE TABLE ar_params;

LOAD DATA INFILE '/var/tmp/ar_params.csv'
REPLACE INTO TABLE ar_params
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
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
	`new_imported_cdrs_from_calldate`,
	`current_count_of_rerating_failed_attempts`,
	`current_rerating_event_is_running`,
	`should_reschedule_rerate_from_official_calldate`,
	`wait_for_scheduled_rerate`,
	`clean_error_table`
);

TRUNCATE TABLE ar_reseller;

LOAD DATA INFILE '/var/tmp/ar_reseller.csv'
REPLACE INTO TABLE ar_reseller
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`internal_name`,
	`name`,
	`note`
);

TRUNCATE TABLE ar_rate_category;

LOAD DATA INFILE '/var/tmp/ar_rate_category.csv'
REPLACE INTO TABLE ar_rate_category
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`internal_name`,
	`short_description`
);

TRUNCATE TABLE ar_rate_format;

LOAD DATA INFILE '/var/tmp/ar_rate_format.csv'
REPLACE INTO TABLE ar_rate_format
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`short_description`,
	`detailed_description`,
	`internal_name`,
	`order_name`
);

TRUNCATE TABLE ar_rate;

LOAD DATA INFILE '/var/tmp/ar_rate.csv'
REPLACE INTO TABLE ar_rate
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
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
);

TRUNCATE TABLE ar_rate_shared_with_reseller;

LOAD DATA INFILE '/var/tmp/ar_rate_shared_with_reseller.csv'
REPLACE INTO TABLE ar_rate_shared_with_reseller
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`ar_rate_id`,
	`ar_reseller_id`,
	`is_exported`,
	`id`
);

TRUNCATE TABLE ar_telephone_prefix;

LOAD DATA INFILE '/var/tmp/ar_telephone_prefix.csv'
REPLACE INTO TABLE ar_telephone_prefix
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`prefix`,
	`match_only_numbers_with_n_digits`,
	`name`,
	`geographic_location`,
	`operator_type`,
	`display_priority_level`
);

TRUNCATE TABLE ar_organization_unit;

LOAD DATA INFILE '/var/tmp/ar_organization_unit.csv'
REPLACE INTO TABLE ar_organization_unit
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`internal_name`,
	`internal_name2`,
        internal_checksum1,
        internal_checksum2,
        internal_checksum3,
        internal_checksum4,
	`export_code`,
	`automatically_managed_from`
);

TRUNCATE TABLE ar_organization_unit_type;

LOAD DATA INFILE '/var/tmp/ar_organization_unit_type.csv'
REPLACE INTO TABLE ar_organization_unit_type
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`name`,
	`short_code`,
	`internal_name`
);

TRUNCATE TABLE ar_organization_unit_has_structure;

LOAD DATA INFILE '/var/tmp/ar_organization_unit_has_structure.csv'
REPLACE INTO TABLE ar_organization_unit_has_structure
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
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
);

TRUNCATE TABLE ar_vendor;

LOAD DATA INFILE '/var/tmp/ar_vendor.csv'
REPLACE INTO TABLE ar_vendor
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`internal_name`,
	`ar_party_id`,
	`is_internal`
);

TRUNCATE TABLE ar_communication_channel_type;

LOAD DATA INFILE '/var/tmp/ar_communication_channel_type.csv'
REPLACE INTO TABLE ar_communication_channel_type
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`name`,
	`description`,
	`internal_name`
);

TRUNCATE TABLE ar_vendor_domain;

LOAD DATA INFILE '/var/tmp/ar_vendor_domain.csv'
REPLACE INTO TABLE ar_vendor_domain
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`internal_name`,
	`ar_vendor_id`,
	`ar_communication_channel_type_id`,
	`domain`,
	`is_prefix`,
	`is_suffix`,
	`from`,
	`to`
);

TRUNCATE TABLE ar_user;

LOAD DATA INFILE '/var/tmp/ar_user.csv'
REPLACE INTO TABLE ar_user
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`ar_party_id`,
	`ar_organization_unit_id`,
	`login`,
	`password`,
        clear_password_to_import,
	`is_enabled`,
	`is_root_admin`
);

TRUNCATE TABLE ar_user_change_password_request;

LOAD DATA INFILE '/var/tmp/ar_user_change_password_request.csv'
REPLACE INTO TABLE ar_user_change_password_request
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`ar_user_id`,
	`at_date`,
	`old_password`,
	`new_password`,
	`is_processed`
);

TRUNCATE TABLE ar_user_has_role;

LOAD DATA INFILE '/var/tmp/ar_user_has_role.csv'
REPLACE INTO TABLE ar_user_has_role
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`ar_user_id`,
	`ar_role_id`,
	`id`
);

TRUNCATE TABLE ar_user_has_permission;

LOAD DATA INFILE '/var/tmp/ar_user_has_permission.csv'
REPLACE INTO TABLE ar_user_has_permission
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`ar_user_id`,
	`ar_permission_id`,
	`id`
);

TRUNCATE TABLE ar_role;

LOAD DATA INFILE '/var/tmp/ar_role.csv'
REPLACE INTO TABLE ar_role
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`name`,
	`power`,
	`description`,
	`internal_name`
);

TRUNCATE TABLE ar_role_has_permission;

LOAD DATA INFILE '/var/tmp/ar_role_has_permission.csv'
REPLACE INTO TABLE ar_role_has_permission
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`ar_permission_id`,
	`ar_role_id`,
	`id`
);

TRUNCATE TABLE ar_global_permissions;

LOAD DATA INFILE '/var/tmp/ar_global_permissions.csv'
REPLACE INTO TABLE ar_global_permissions
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`show_call_cost`,
	`show_call_income`,
	`show_outgoing_calls`,
	`show_incoming_calls`,
	`show_internal_calls`,
	`show_voip_provider`,
	`show_communication_channel`,
	`show_cost_saving`
);

TRUNCATE TABLE ar_permission;

LOAD DATA INFILE '/var/tmp/ar_permission.csv'
REPLACE INTO TABLE ar_permission
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`name`,
	`description`,
	`power`
);

TRUNCATE TABLE ar_report_order_of_children;

LOAD DATA INFILE '/var/tmp/ar_report_order_of_children.csv'
REPLACE INTO TABLE ar_report_order_of_children
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`name`,
	`description`
);

TRUNCATE TABLE ar_report;

LOAD DATA INFILE '/var/tmp/ar_report.csv'
REPLACE INTO TABLE ar_report
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
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
	`total_without_tax`,
	`tax`,
	`applied_vat`,
	`total_with_tax`
);

TRUNCATE TABLE ar_report_also_for;

LOAD DATA INFILE '/var/tmp/ar_report_also_for.csv'
REPLACE INTO TABLE ar_report_also_for
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`ar_report_id`,
	`ar_role_id`
);

TRUNCATE TABLE ar_report_scheduler;

LOAD DATA INFILE '/var/tmp/ar_report_scheduler.csv'
REPLACE INTO TABLE ar_report_scheduler
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
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
);

TRUNCATE TABLE ar_postponed_report;

LOAD DATA INFILE '/var/tmp/ar_postponed_report.csv'
REPLACE INTO TABLE ar_postponed_report
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`ar_report_set_id`,
	`ar_organization_unit_id`
);

TRUNCATE TABLE ar_postponed_report_tmp;

LOAD DATA INFILE '/var/tmp/ar_postponed_report_tmp.csv'
REPLACE INTO TABLE ar_postponed_report_tmp
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`ar_organization_unit_id`,
	`from_date`,
	`is_billed`,
	`is_processed`
);

TRUNCATE TABLE ar_legal_date_generation_method;

LOAD DATA INFILE '/var/tmp/ar_legal_date_generation_method.csv'
REPLACE INTO TABLE ar_legal_date_generation_method
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`name`,
	`description`
);

TRUNCATE TABLE ar_report_set;

LOAD DATA INFILE '/var/tmp/ar_report_set.csv'
REPLACE INTO TABLE ar_report_set
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
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
);

TRUNCATE TABLE ar_report_generation;

LOAD DATA INFILE '/var/tmp/ar_report_generation.csv'
REPLACE INTO TABLE ar_report_generation
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`name`,
	`description`
);

TRUNCATE TABLE ar_report_to_read;

LOAD DATA INFILE '/var/tmp/ar_report_to_read.csv'
REPLACE INTO TABLE ar_report_to_read
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`ar_report_id`,
	`ar_user_id`,
	`seen_or_received_from_user`,
	`must_be_sent_to_email`,
	`sent_to_email_at_date`
);

TRUNCATE TABLE ar_user_can_view_report;

LOAD DATA INFILE '/var/tmp/ar_user_can_view_report.csv'
REPLACE INTO TABLE ar_user_can_view_report
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	  `ar_user_id`,
	  `ar_report_id`
);

TRUNCATE TABLE ar_logical_source;

LOAD DATA INFILE '/var/tmp/ar_logical_source.csv'
REPLACE INTO TABLE ar_logical_source
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`name`,
	`description`
);

TRUNCATE TABLE ar_cdr_provider;

LOAD DATA INFILE '/var/tmp/ar_cdr_provider.csv'
REPLACE INTO TABLE ar_cdr_provider
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`internal_name`,
	`description`,
        last_imported_id,
        last_imported_data
);

TRUNCATE TABLE ar_physical_format;

LOAD DATA INFILE '/var/tmp/ar_physical_format.csv'
REPLACE INTO TABLE ar_physical_format
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`ar_logical_source_id`,
	`name`,
	`description`
);

TRUNCATE TABLE ar_source_csv_file;

LOAD DATA INFILE '/var/tmp/ar_source_csv_file.csv'
REPLACE INTO TABLE ar_source_csv_file
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
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
);

TRUNCATE TABLE ar_source_cdr;

LOAD DATA INFILE '/var/tmp/ar_source_cdr.csv'
  REPLACE INTO TABLE ar_source_cdr
  CHARACTER SET 'utf8mb4'
  FIELDS TERMINATED BY '\t'
  OPTIONALLY ENCLOSED BY '"'
  LINES TERMINATED BY '\n'
(	`calldate`,
  `id`,
	`ar_cdr_provider_id`,
	`ar_physical_format_id`,
	`content`
);

TRUNCATE TABLE ar_type_of_source_cdr;

LOAD DATA INFILE '/var/tmp/ar_type_of_source_cdr.csv'
REPLACE INTO TABLE ar_type_of_source_cdr
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(	`ar_cdr_provider_id`,
`ar_physical_format_id`
);

TRUNCATE TABLE ar_remote_file;

LOAD DATA INFILE '/var/tmp/ar_remote_file.csv'
REPLACE INTO TABLE ar_remote_file
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(	`id`,
	`ar_cdr_provider_id`,
	`name`,
	`checksum`,
	`receiving_date`
);

TRUNCATE TABLE ar_extension_to_pincode;

LOAD DATA INFILE '/var/tmp/ar_extension_to_pincode.csv'
REPLACE INTO TABLE ar_extension_to_pincode
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(	`id`,
	`extension`,
	`pincode`,
	`from_date`
);

TRUNCATE TABLE ar_service;

LOAD DATA INFILE '/var/tmp/ar_service.csv'
REPLACE INTO TABLE ar_service
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
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
        `schedule_at`
);

TRUNCATE TABLE ar_service_price;

LOAD DATA INFILE '/var/tmp/ar_service_price.csv'
REPLACE INTO TABLE ar_service_price
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`internal_name`,
	`ar_service_id`,
	`from_date`,
	`price`
);

TRUNCATE TABLE ar_assigned_service;

LOAD DATA INFILE '/var/tmp/ar_assigned_service.csv'
REPLACE INTO TABLE ar_assigned_service
CHARACTER SET 'utf8mb4'
FIELDS TERMINATED BY '\t'
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(
	`id`,
	`internal_name`,
	`external_crm_code`,
	`from_date`,
	`ar_service_id`,
	`ar_organization_unit_id`,
	`nr_of_items`,
	`discount`,
	`note`
);
