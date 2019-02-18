
# This is a fix for InnoDB in MySQL >= 4.1.x
# It "suspends judgement" for fkey relationships until are tables are set.
SET FOREIGN_KEY_CHECKS = 0;

#-----------------------------------------------------------------------------
#-- ar_cdr
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_cdr`;


CREATE TABLE `ar_cdr`
(
	`calldate` DATETIME  NOT NULL,
	`id` INTEGER default 0 NOT NULL,
	`is_service_cdr` TINYINT default 0 NOT NULL,
	`to_calldate` DATETIME,
	`count_of_calls` INTEGER default 1 NOT NULL,
	`destination_type` SMALLINT default 0 NOT NULL,
	`is_redirect` TINYINT default 0 NOT NULL,
	`duration` INTEGER default 0 NOT NULL,
	`billsec` INTEGER default 0 NOT NULL,
	`ar_organization_unit_id` INTEGER,
	`cached_parent_id_hierarchy` VARBINARY(850),
	`billable_ar_organization_unit_id` INTEGER,
	`bundle_ar_organization_unit_id` INTEGER,
	`income` BIGINT,
	`cost_saving` BIGINT,
	`ar_vendor_id` INTEGER,
	`ar_communication_channel_type_id` INTEGER,
	`cost` BIGINT,
	`expected_cost` BIGINT,
	`ar_telephone_prefix_id` INTEGER,
	`cached_external_telephone_number` VARCHAR(1024),
	`external_telephone_number_with_applied_portability` VARCHAR(1024),
	`cached_masked_external_telephone_number` VARCHAR(1024),
	`error_destination_type` SMALLINT default 0 NOT NULL,
	`ar_problem_duplication_key` VARCHAR(255),
	`debug_cost_rate` VARCHAR(512),
	`debug_income_rate` VARCHAR(512),
	PRIMARY KEY (`calldate`,`id`,`is_service_cdr`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_destination_type
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_destination_type`;


CREATE TABLE `ar_destination_type`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_id` SMALLINT,
	`name` VARCHAR(1024),
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_number_portability
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_number_portability`;


CREATE TABLE `ar_number_portability`
(
	`telephone_number` VARCHAR(255)  NOT NULL,
	`from_date` DATETIME  NOT NULL,
	`ported_telephone_number` VARCHAR(255)  NOT NULL,
	PRIMARY KEY (`telephone_number`,`from_date`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_rating_engine_export_status
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_rating_engine_export_status`;


CREATE TABLE `ar_rating_engine_export_status`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255),
	`check_value` VARCHAR(1024),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_rating_engine_export_status_U_1` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_party
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_party`;


CREATE TABLE `ar_party`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`name` VARCHAR(255),
	`compact_name` VARCHAR(255),
	`note` VARCHAR(1024),
	`external_crm_code` VARCHAR(255),
	`contract_number` VARCHAR(255),
	`vat` VARCHAR(255),
	`legal_registration_number` VARCHAR(255),
	`is_billable` TINYINT default 0 NOT NULL,
	`legal_address` VARCHAR(255),
	`legal_city` VARCHAR(255),
	`legal_zipcode` VARCHAR(255),
	`legal_state_province` VARCHAR(255),
	`legal_country` VARCHAR(255),
	`email` VARCHAR(255),
	`contact_name` VARCHAR(255),
	`phone` VARCHAR(255),
	`phone2` VARCHAR(255),
	`fax` VARCHAR(255),
	`web_site` VARCHAR(120),
	`max_limit_30` BIGINT,
	`last_email_advise_for_max_limit_30` DATETIME,
	`is_active` TINYINT default 1 NOT NULL,
	`ar_reseller_id` INTEGER,
	`migration_field_for_telephone` VARCHAR(255),
	`migration_field_for_adsl` VARCHAR(255),
	`payment_iban` VARCHAR(255),
	`payment_bic` VARCHAR(255),
	`payment_sepa` VARCHAR(255),
	`payment_info` VARCHAR(255),
	PRIMARY KEY (`id`),
	KEY `ar_party_I_1`(`is_billable`),
	KEY `ar_party_I_2`(`is_active`),
	INDEX `ar_party_FI_1` (`ar_reseller_id`),
	CONSTRAINT `ar_party_FK_1`
		FOREIGN KEY (`ar_reseller_id`)
		REFERENCES `ar_reseller` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_tag
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_tag`;


CREATE TABLE `ar_tag`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255)  NOT NULL,
	`note_for_admin` VARCHAR(1024),
	`name_for_customer` VARCHAR(512) default '' NOT NULL,
	`note_for_customer` VARCHAR(1204) default '' NOT NULL,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_tag_U_1` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_party_has_tag
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_party_has_tag`;


CREATE TABLE `ar_party_has_tag`
(
	`ar_party_id` INTEGER  NOT NULL,
	`ar_tag_id` INTEGER  NOT NULL,
	PRIMARY KEY (`ar_party_id`,`ar_tag_id`),
	CONSTRAINT `ar_party_has_tag_FK_1`
		FOREIGN KEY (`ar_party_id`)
		REFERENCES `ar_party` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_party_has_tag_FI_2` (`ar_tag_id`),
	CONSTRAINT `ar_party_has_tag_FK_2`
		FOREIGN KEY (`ar_tag_id`)
		REFERENCES `ar_tag` (`id`)
		ON DELETE CASCADE
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_params
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_params`;


CREATE TABLE `ar_params`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`name` VARCHAR(255),
	`is_default` TINYINT,
	`service_name` VARCHAR(255),
	`service_provider_website` VARCHAR(255),
	`service_provider_email` VARCHAR(255),
	`vat_tax_perc` INTEGER default 0 NOT NULL,
	`logo_image` VARCHAR(255),
	`slogan` VARCHAR(1024),
	`logo_image_in_invoices` VARCHAR(255),
	`footer` VARCHAR(255),
	`user_message` VARCHAR(255),
	`legal_name` VARCHAR(255),
	`external_crm_code` VARCHAR(255),
	`vat` VARCHAR(255),
	`legal_address` VARCHAR(255),
	`legal_website` VARCHAR(255),
	`legal_city` VARCHAR(255),
	`legal_zipcode` VARCHAR(255),
	`legal_state_province` VARCHAR(255),
	`legal_country` VARCHAR(255),
	`legal_email` VARCHAR(255),
	`legal_phone` VARCHAR(255),
	`phone2` VARCHAR(255),
	`legal_fax` VARCHAR(255),
	`invoice_notes` VARCHAR(255),
	`invoice_payment_terms` VARCHAR(2048),
	`invoice_payment_due_in_xx_days` INTEGER(4),
	`sender_name_on_invoicing_emails` VARCHAR(255),
	`invoicing_email_address` VARCHAR(255),
	`logo_html_color` VARCHAR(12),
	`html_notes_on_the_login_form` VARCHAR(2048),
	`official_calldate` DATETIME,
	`scheduled_rerate_from_official_calldate` TINYINT default 0 NOT NULL,
	`new_imported_cdrs_from_calldate` DATETIME,
	`scheduled_rerate_from_specific_calldate` DATETIME,
	`current_count_of_rerating_failed_attempts` INTEGER default 0 NOT NULL,
	`current_rerating_event_is_running` TINYINT default 0 NOT NULL,
	`should_reschedule_rerate_from_official_calldate` TINYINT default 0 NOT NULL,
	`wait_for_scheduled_rerate` TINYINT default 1 NOT NULL,
	`clean_error_table` INTEGER default 0 NOT NULL,
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_reseller
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_reseller`;


CREATE TABLE `ar_reseller`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255),
	`name` VARCHAR(1204)  NOT NULL,
	`note` VARCHAR(2048),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_reseller_U_1` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_rate_category
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_rate_category`;


CREATE TABLE `ar_rate_category`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255)  NOT NULL,
	`short_description` TEXT,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_rate_category_U_1` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_rate_format
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_rate_format`;


CREATE TABLE `ar_rate_format`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`short_description` TEXT,
	`detailed_description` TEXT,
	`internal_name` VARCHAR(255)  NOT NULL,
	`order_name` VARCHAR(255),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_rate_format_U_1` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_rate
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_rate`;


CREATE TABLE `ar_rate`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_vendor_id` INTEGER,
	`ar_rate_format_id` INTEGER,
	`from_time` DATETIME  NOT NULL,
	`internal_name` VARCHAR(512),
	`ar_rate_id` INTEGER,
	`short_description` VARCHAR(1024),
	`note` TEXT,
	`is_exported_to_resellers` TINYINT default 0 NOT NULL,
	`was_compiled` TINYINT default 0 NOT NULL,
	`source_data_file` LONGBLOB,
	`backup_source_data_file` LONGBLOB,
	`html_description` LONGBLOB,
	PRIMARY KEY (`id`),
	KEY `ar_rate_I_1`(`from_time`),
	KEY `ar_rate_I_2`(`internal_name`),
	KEY `ar_rate_I_3`(`is_exported_to_resellers`),
	KEY `ar_rate_I_4`(`was_compiled`),
	INDEX `ar_rate_FI_1` (`ar_vendor_id`),
	CONSTRAINT `ar_rate_FK_1`
		FOREIGN KEY (`ar_vendor_id`)
		REFERENCES `ar_vendor` (`id`),
	INDEX `ar_rate_FI_2` (`ar_rate_format_id`),
	CONSTRAINT `ar_rate_FK_2`
		FOREIGN KEY (`ar_rate_format_id`)
		REFERENCES `ar_rate_format` (`id`),
	INDEX `ar_rate_FI_3` (`ar_rate_id`),
	CONSTRAINT `ar_rate_FK_3`
		FOREIGN KEY (`ar_rate_id`)
		REFERENCES `ar_rate` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_rate_shared_with_reseller
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_rate_shared_with_reseller`;


CREATE TABLE `ar_rate_shared_with_reseller`
(
	`ar_rate_id` INTEGER,
	`ar_reseller_id` INTEGER,
	`is_exported` TINYINT default 0 NOT NULL,
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (`id`),
	KEY `ar_rate_shared_with_reseller_I_1`(`is_exported`),
	INDEX `ar_rate_shared_with_reseller_FI_1` (`ar_rate_id`),
	CONSTRAINT `ar_rate_shared_with_reseller_FK_1`
		FOREIGN KEY (`ar_rate_id`)
		REFERENCES `ar_rate` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_rate_shared_with_reseller_FI_2` (`ar_reseller_id`),
	CONSTRAINT `ar_rate_shared_with_reseller_FK_2`
		FOREIGN KEY (`ar_reseller_id`)
		REFERENCES `ar_reseller` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_telephone_prefix
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_telephone_prefix`;


CREATE TABLE `ar_telephone_prefix`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`prefix` VARCHAR(255)  NOT NULL,
	`match_only_numbers_with_n_digits` INTEGER,
	`name` VARCHAR(1024),
	`geographic_location` VARCHAR(255),
	`operator_type` VARCHAR(255),
	`display_priority_level` INTEGER default 0 NOT NULL,
	`rating_code` VARCHAR(255) default '' NOT NULL,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_telephone_prefix_U_1` (`prefix`),
	KEY `ar_telephone_prefix_I_1`(`operator_type`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_organization_unit
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_organization_unit`;


CREATE TABLE `ar_organization_unit`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(200),
	`internal_name2` VARCHAR(200),
	`internal_checksum1` VARCHAR(200),
	`internal_checksum2` VARCHAR(200),
	`internal_checksum3` VARCHAR(200),
	`internal_checksum4` VARCHAR(200),
	`export_code` VARCHAR(200),
	`automatically_managed_from` INTEGER default 0 NOT NULL,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_organization_unit_U_1` (`internal_name`),
	UNIQUE KEY `ar_organization_unit_U_2` (`internal_name2`),
	UNIQUE KEY `ar_organization_unit_U_3` (`export_code`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_organization_unit_type
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_organization_unit_type`;


CREATE TABLE `ar_organization_unit_type`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`name` VARCHAR(200)  NOT NULL,
	`short_code` VARCHAR(255),
	`internal_name` VARCHAR(200),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_organization_unit_type_U_1` (`name`),
	UNIQUE KEY `ar_organization_unit_type_U_2` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_organization_unit_has_structure
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_organization_unit_has_structure`;


CREATE TABLE `ar_organization_unit_has_structure`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_organization_unit_id` INTEGER,
	`ar_organization_unit_type_id` INTEGER,
	`ar_parent_organization_unit_id` INTEGER,
	`from` DATETIME  NOT NULL,
	`exists` TINYINT default 1 NOT NULL,
	`ar_rate_category_id` INTEGER,
	`ar_party_id` INTEGER,
	`extension_codes` VARCHAR(5024),
	`extension_name` VARCHAR(1024),
	`extension_user_code` VARCHAR(1024),
	PRIMARY KEY (`id`),
	UNIQUE KEY `avoid_conflicting_changes_on_the_same_date` (`ar_organization_unit_id`, `from`),
	KEY `ar_organization_unit_has_structure_I_1`(`from`),
	CONSTRAINT `ar_organization_unit_has_structure_FK_1`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`),
	INDEX `ar_organization_unit_has_structure_FI_2` (`ar_organization_unit_type_id`),
	CONSTRAINT `ar_organization_unit_has_structure_FK_2`
		FOREIGN KEY (`ar_organization_unit_type_id`)
		REFERENCES `ar_organization_unit_type` (`id`),
	INDEX `ar_organization_unit_has_structure_FI_3` (`ar_parent_organization_unit_id`),
	CONSTRAINT `ar_organization_unit_has_structure_FK_3`
		FOREIGN KEY (`ar_parent_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`),
	INDEX `ar_organization_unit_has_structure_FI_4` (`ar_rate_category_id`),
	CONSTRAINT `ar_organization_unit_has_structure_FK_4`
		FOREIGN KEY (`ar_rate_category_id`)
		REFERENCES `ar_rate_category` (`id`),
	INDEX `ar_organization_unit_has_structure_FI_5` (`ar_party_id`),
	CONSTRAINT `ar_organization_unit_has_structure_FK_5`
		FOREIGN KEY (`ar_party_id`)
		REFERENCES `ar_party` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_organization_backup_of_changes
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_organization_backup_of_changes`;


CREATE TABLE `ar_organization_backup_of_changes`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`backup_at_date` DATETIME  NOT NULL,
	`application_version` VARCHAR(255),
	`md5_sum` VARCHAR(2048),
	`yaml_export_at_date` LONGBLOB,
	`sql_tables` LONGBLOB,
	PRIMARY KEY (`id`),
	KEY `ar_organization_backup_of_changes_I_1`(`backup_at_date`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_expanded_extensions
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_expanded_extensions`;


CREATE TABLE `ar_expanded_extensions`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_organization_unit_id` INTEGER,
	`extension_code` VARCHAR(5024)  NOT NULL,
	PRIMARY KEY (`id`),
	INDEX `ar_expanded_extensions_FI_1` (`ar_organization_unit_id`),
	CONSTRAINT `ar_expanded_extensions_FK_1`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_vendor
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_vendor`;


CREATE TABLE `ar_vendor`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(200),
	`ar_party_id` INTEGER,
	`is_internal` TINYINT default 0 NOT NULL,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_vendor_U_1` (`internal_name`),
	INDEX `ar_vendor_FI_1` (`ar_party_id`),
	CONSTRAINT `ar_vendor_FK_1`
		FOREIGN KEY (`ar_party_id`)
		REFERENCES `ar_party` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_communication_channel_type
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_communication_channel_type`;


CREATE TABLE `ar_communication_channel_type`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`name` VARCHAR(255)  NOT NULL,
	`description` VARCHAR(2048),
	`internal_name` VARCHAR(200),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_communication_channel_type_U_1` (`name`),
	UNIQUE KEY `ar_communication_channel_type_U_2` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_vendor_domain
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_vendor_domain`;


CREATE TABLE `ar_vendor_domain`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(200),
	`ar_vendor_id` INTEGER,
	`ar_communication_channel_type_id` INTEGER,
	`domain` VARCHAR(255)  NOT NULL,
	`is_prefix` TINYINT default 0 NOT NULL,
	`is_suffix` TINYINT default 0 NOT NULL,
	`from` DATETIME  NOT NULL,
	`to` DATETIME,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_vendor_domain_U_1` (`internal_name`),
	UNIQUE KEY `ar_vendor_domain_U_2` (`domain`),
	INDEX `ar_vendor_domain_FI_1` (`ar_vendor_id`),
	CONSTRAINT `ar_vendor_domain_FK_1`
		FOREIGN KEY (`ar_vendor_id`)
		REFERENCES `ar_vendor` (`id`),
	INDEX `ar_vendor_domain_FI_2` (`ar_communication_channel_type_id`),
	CONSTRAINT `ar_vendor_domain_FK_2`
		FOREIGN KEY (`ar_communication_channel_type_id`)
		REFERENCES `ar_communication_channel_type` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_user
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_user`;


CREATE TABLE `ar_user`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_party_id` INTEGER,
	`ar_organization_unit_id` INTEGER,
	`login` VARCHAR(255)  NOT NULL,
	`password` VARCHAR(1024),
	`clear_password_to_import` VARCHAR(255),
	`is_enabled` TINYINT default 1 NOT NULL,
	`is_root_admin` TINYINT default 0 NOT NULL,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_user_U_1` (`login`),
	KEY `ar_user_I_1`(`clear_password_to_import`),
	INDEX `ar_user_FI_1` (`ar_party_id`),
	CONSTRAINT `ar_user_FK_1`
		FOREIGN KEY (`ar_party_id`)
		REFERENCES `ar_party` (`id`),
	INDEX `ar_user_FI_2` (`ar_organization_unit_id`),
	CONSTRAINT `ar_user_FK_2`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_user_change_password_request
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_user_change_password_request`;


CREATE TABLE `ar_user_change_password_request`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_user_id` INTEGER,
	`at_date` DATETIME  NOT NULL,
	`old_password` VARCHAR(1024),
	`new_password` VARCHAR(1024),
	`is_processed` TINYINT default 0 NOT NULL,
	PRIMARY KEY (`id`),
	KEY `ar_user_change_password_request_I_1`(`is_processed`),
	INDEX `ar_user_change_password_request_FI_1` (`ar_user_id`),
	CONSTRAINT `ar_user_change_password_request_FK_1`
		FOREIGN KEY (`ar_user_id`)
		REFERENCES `ar_user` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_user_has_role
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_user_has_role`;


CREATE TABLE `ar_user_has_role`
(
	`ar_user_id` INTEGER,
	`ar_role_id` INTEGER,
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (`id`),
	INDEX `ar_user_has_role_FI_1` (`ar_user_id`),
	CONSTRAINT `ar_user_has_role_FK_1`
		FOREIGN KEY (`ar_user_id`)
		REFERENCES `ar_user` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_user_has_role_FI_2` (`ar_role_id`),
	CONSTRAINT `ar_user_has_role_FK_2`
		FOREIGN KEY (`ar_role_id`)
		REFERENCES `ar_role` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_user_has_permission
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_user_has_permission`;


CREATE TABLE `ar_user_has_permission`
(
	`ar_user_id` INTEGER,
	`ar_permission_id` INTEGER,
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (`id`),
	INDEX `ar_user_has_permission_FI_1` (`ar_user_id`),
	CONSTRAINT `ar_user_has_permission_FK_1`
		FOREIGN KEY (`ar_user_id`)
		REFERENCES `ar_user` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_user_has_permission_FI_2` (`ar_permission_id`),
	CONSTRAINT `ar_user_has_permission_FK_2`
		FOREIGN KEY (`ar_permission_id`)
		REFERENCES `ar_permission` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_view_all_user_permissions
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_view_all_user_permissions`;


CREATE TABLE `ar_view_all_user_permissions`
(
	`ar_user_id` VARCHAR(255)  NOT NULL,
	`ar_permission_id` VARCHAR(255)  NOT NULL,
	PRIMARY KEY (`ar_user_id`,`ar_permission_id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_role
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_role`;


CREATE TABLE `ar_role`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`name` VARCHAR(1024),
	`power` INTEGER(5)  NOT NULL,
	`description` TEXT,
	`internal_name` VARCHAR(200),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_role_U_1` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_role_has_permission
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_role_has_permission`;


CREATE TABLE `ar_role_has_permission`
(
	`ar_permission_id` INTEGER,
	`ar_role_id` INTEGER,
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (`id`),
	INDEX `ar_role_has_permission_FI_1` (`ar_permission_id`),
	CONSTRAINT `ar_role_has_permission_FK_1`
		FOREIGN KEY (`ar_permission_id`)
		REFERENCES `ar_permission` (`id`),
	INDEX `ar_role_has_permission_FI_2` (`ar_role_id`),
	CONSTRAINT `ar_role_has_permission_FK_2`
		FOREIGN KEY (`ar_role_id`)
		REFERENCES `ar_role` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_global_permissions
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_global_permissions`;


CREATE TABLE `ar_global_permissions`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`show_call_cost` TINYINT,
	`show_call_income` TINYINT,
	`show_outgoing_calls` TINYINT,
	`show_incoming_calls` TINYINT,
	`show_internal_calls` TINYINT,
	`show_voip_provider` TINYINT,
	`show_communication_channel` TINYINT,
	`show_cost_saving` TINYINT,
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_permission
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_permission`;


CREATE TABLE `ar_permission`
(
	`id` INTEGER  NOT NULL,
	`name` VARCHAR(1024),
	`description` TEXT,
	`power` INTEGER  NOT NULL,
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_report_order_of_children
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_report_order_of_children`;


CREATE TABLE `ar_report_order_of_children`
(
	`id` INTEGER  NOT NULL,
	`name` VARCHAR(255),
	`description` VARCHAR(1024),
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_report
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_report`;


CREATE TABLE `ar_report`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`is_template` TINYINT default 0 NOT NULL,
	`ar_report_set_id` INTEGER,
	`about_ar_report_set_id` INTEGER,
	`ar_organization_unit_id` INTEGER,
	`ar_user_id` INTEGER,
	`ar_vendor_id` INTEGER,
	`ar_tag_id` INTEGER,
	`from_date` DATETIME,
	`to_date` DATETIME,
	`param_show_masked_telephone_numbers` TINYINT default 1 NOT NULL,
	`param_show_call_cost` TINYINT default 0 NOT NULL,
	`param_show_call_income` TINYINT default 0 NOT NULL,
	`param_show_also_outgoing_calls` TINYINT default 0 NOT NULL,
	`param_show_also_system_calls` TINYINT default 0 NOT NULL,
	`param_show_also_incoming_calls` TINYINT default 0 NOT NULL,
	`param_show_also_internal_calls` TINYINT default 0 NOT NULL,
	`param_show_call_details` TINYINT default 0 NOT NULL,
	`param_show_voip_provider` TINYINT default 0 NOT NULL,
	`param_show_communication_channel` TINYINT default 0 NOT NULL,
	`param_show_geographic_location` TINYINT default 0 NOT NULL,
	`param_show_connection_type` TINYINT default 0 NOT NULL,
	`param_show_cost_saving` TINYINT default 0 NOT NULL,
	`param_is_legal` TINYINT default 0 NOT NULL,
	`param_expand_to_level` INTEGER(4) default 0 NOT NULL,
	`ar_report_order_of_children_id` INTEGER,
	`php_class_name` VARCHAR(1024),
	`produced_report_generation_date` DATETIME,
	`report_name` VARCHAR(1024),
	`produced_report_short_description` VARCHAR(512),
	`produced_report_additional_description` VARCHAR(1024),
	`produced_report_already_reviewed` TINYINT default 0 NOT NULL,
	`produced_report_is_draft` TINYINT default 0 NOT NULL,
	`produced_report_must_be_regenerated` TINYINT default 0 NOT NULL,
	`produced_report_mime_type` VARCHAR(128) default 'application/pdf' NOT NULL,
	`produced_report_file_type_suffix` VARCHAR(25) default 'pdf' NOT NULL,
	`produced_report_document` LONGBLOB,
	`produced_report_document_checksum` VARCHAR(1024),
	`report_mail_subject` VARCHAR(512),
	`report_mail_body` VARCHAR(2048),
	`report_attachment_file_name` VARCHAR(255),
	`report_attachment_file_name_add_report_date` TINYINT default 0 NOT NULL,
	`internal_name` VARCHAR(512),
	`cached_parent_id_hierarchy` VARBINARY(850),
	`legal_nr_prefix` VARCHAR(80) default '' NOT NULL,
	`legal_consecutive_nr` INTEGER,
	`legal_date` DATE,
	`legal_sender_name` VARCHAR(255),
	`legal_sender_vat` VARCHAR(255),
	`legal_sender_address` VARCHAR(1024),
	`legal_receiver_name` VARCHAR(255),
	`legal_receiver_vat` VARCHAR(255),
	`legal_receiver_address` VARCHAR(1024),
	`total_without_tax` BIGINT default 0 NOT NULL,
	`tax` BIGINT default 0 NOT NULL,
	`applied_vat` BIGINT default 0 NOT NULL,
	`total_with_tax` BIGINT default 0 NOT NULL,
	PRIMARY KEY (`id`),
	KEY `ar_report_I_1`(`is_template`),
	KEY `ar_report_I_2`(`from_date`),
	KEY `ar_report_I_3`(`to_date`),
	KEY `ar_report_I_4`(`produced_report_generation_date`),
	KEY `ar_report_I_5`(`produced_report_already_reviewed`),
	KEY `ar_report_I_6`(`produced_report_is_draft`),
	KEY `ar_report_I_7`(`internal_name`),
	KEY `ar_report_I_8`(`cached_parent_id_hierarchy`),
	KEY `ar_report_I_9`(`legal_nr_prefix`),
	KEY `ar_report_I_10`(`legal_date`),
	INDEX `ar_report_FI_1` (`ar_report_set_id`),
	CONSTRAINT `ar_report_FK_1`
		FOREIGN KEY (`ar_report_set_id`)
		REFERENCES `ar_report_set` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_report_FI_2` (`about_ar_report_set_id`),
	CONSTRAINT `ar_report_FK_2`
		FOREIGN KEY (`about_ar_report_set_id`)
		REFERENCES `ar_report_set` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_report_FI_3` (`ar_organization_unit_id`),
	CONSTRAINT `ar_report_FK_3`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`),
	INDEX `ar_report_FI_4` (`ar_user_id`),
	CONSTRAINT `ar_report_FK_4`
		FOREIGN KEY (`ar_user_id`)
		REFERENCES `ar_user` (`id`),
	INDEX `ar_report_FI_5` (`ar_vendor_id`),
	CONSTRAINT `ar_report_FK_5`
		FOREIGN KEY (`ar_vendor_id`)
		REFERENCES `ar_vendor` (`id`),
	INDEX `ar_report_FI_6` (`ar_tag_id`),
	CONSTRAINT `ar_report_FK_6`
		FOREIGN KEY (`ar_tag_id`)
		REFERENCES `ar_tag` (`id`),
	INDEX `ar_report_FI_7` (`ar_report_order_of_children_id`),
	CONSTRAINT `ar_report_FK_7`
		FOREIGN KEY (`ar_report_order_of_children_id`)
		REFERENCES `ar_report_order_of_children` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_report_also_for
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_report_also_for`;


CREATE TABLE `ar_report_also_for`
(
	`ar_report_id` INTEGER  NOT NULL,
	`ar_role_id` INTEGER  NOT NULL,
	PRIMARY KEY (`ar_report_id`,`ar_role_id`),
	CONSTRAINT `ar_report_also_for_FK_1`
		FOREIGN KEY (`ar_report_id`)
		REFERENCES `ar_report` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_report_also_for_FI_2` (`ar_role_id`),
	CONSTRAINT `ar_report_also_for_FK_2`
		FOREIGN KEY (`ar_role_id`)
		REFERENCES `ar_role` (`id`)
		ON DELETE CASCADE
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_report_scheduler
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_report_scheduler`;


CREATE TABLE `ar_report_scheduler`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`is_active` TINYINT,
	`last_execution_date` DATETIME,
	`last_from_date` DATETIME,
	`last_to_date` DATETIME,
	`ar_report_id` INTEGER,
	`ar_organization_unit_id` INTEGER,
	`short_description` VARCHAR(2048),
	`additional_description` VARCHAR(2048),
	`note` VARCHAR(2048),
	`produced_report_must_be_reviewed` TINYINT default 1 NOT NULL,
	`ar_report_generation_id` INTEGER,
	`schedule_every_x_days` INTEGER,
	`schedule_every_x_months` INTEGER,
	`start_generation_after_x_hours` INTEGER default 2 NOT NULL,
	`internal_name` VARCHAR(512),
	`ar_legal_date_generation_method_id` INTEGER,
	`days_to_add_to_legal_date_generation_method` INTEGER,
	`is_yearly_legal_numeration` TINYINT,
	`generate_only_if_there_is_cost` TINYINT default 0 NOT NULL,
	`minimum_cost` BIGINT,
	`send_compact_report_list_to_accountant` TINYINT default 0 NOT NULL,
	PRIMARY KEY (`id`),
	KEY `ar_report_scheduler_I_1`(`internal_name`),
	INDEX `ar_report_scheduler_FI_1` (`ar_report_id`),
	CONSTRAINT `ar_report_scheduler_FK_1`
		FOREIGN KEY (`ar_report_id`)
		REFERENCES `ar_report` (`id`),
	INDEX `ar_report_scheduler_FI_2` (`ar_organization_unit_id`),
	CONSTRAINT `ar_report_scheduler_FK_2`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`),
	INDEX `ar_report_scheduler_FI_3` (`ar_report_generation_id`),
	CONSTRAINT `ar_report_scheduler_FK_3`
		FOREIGN KEY (`ar_report_generation_id`)
		REFERENCES `ar_report_generation` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_postponed_report
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_postponed_report`;


CREATE TABLE `ar_postponed_report`
(
	`ar_report_set_id` INTEGER  NOT NULL,
	`ar_organization_unit_id` INTEGER  NOT NULL,
	PRIMARY KEY (`ar_report_set_id`,`ar_organization_unit_id`),
	CONSTRAINT `ar_postponed_report_FK_1`
		FOREIGN KEY (`ar_report_set_id`)
		REFERENCES `ar_report_set` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_postponed_report_FI_2` (`ar_organization_unit_id`),
	CONSTRAINT `ar_postponed_report_FK_2`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
		ON DELETE CASCADE
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_postponed_report_tmp
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_postponed_report_tmp`;


CREATE TABLE `ar_postponed_report_tmp`
(
	`ar_organization_unit_id` INTEGER  NOT NULL,
	`from_date` DATETIME  NOT NULL,
	`is_billed` TINYINT  NOT NULL,
	`is_processed` TINYINT  NOT NULL,
	PRIMARY KEY (`ar_organization_unit_id`),
	CONSTRAINT `ar_postponed_report_tmp_FK_1`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
		ON DELETE CASCADE
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_legal_date_generation_method
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_legal_date_generation_method`;


CREATE TABLE `ar_legal_date_generation_method`
(
	`id` INTEGER  NOT NULL,
	`name` VARCHAR(1024),
	`description` VARCHAR(2048),
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_report_set
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_report_set`;


CREATE TABLE `ar_report_set`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_report_scheduler_id` INTEGER,
	`from_date` DATETIME  NOT NULL,
	`to_date` DATETIME  NOT NULL,
	`must_be_reviewed` TINYINT default 1 NOT NULL,
	`postponed_fields_are_updated` TINYINT default 1 NOT NULL,
	`postponed_reports` INTEGER default 0 NOT NULL,
	`postponed_amount` BIGINT default 0 NOT NULL,
	`reports` INTEGER default 0 NOT NULL,
	`amount` BIGINT default 0 NOT NULL,
	PRIMARY KEY (`id`),
	KEY `ar_report_set_I_1`(`from_date`),
	KEY `ar_report_set_I_2`(`to_date`),
	KEY `ar_report_set_I_3`(`must_be_reviewed`),
	INDEX `ar_report_set_FI_1` (`ar_report_scheduler_id`),
	CONSTRAINT `ar_report_set_FK_1`
		FOREIGN KEY (`ar_report_scheduler_id`)
		REFERENCES `ar_report_scheduler` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_report_generation
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_report_generation`;


CREATE TABLE `ar_report_generation`
(
	`id` INTEGER  NOT NULL,
	`name` VARCHAR(256),
	`description` VARCHAR(1024),
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_report_to_read
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_report_to_read`;


CREATE TABLE `ar_report_to_read`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_report_id` INTEGER  NOT NULL,
	`ar_user_id` INTEGER,
	`seen_or_received_from_user` TINYINT default 0 NOT NULL,
	`must_be_sent_to_email` TINYINT default 0 NOT NULL,
	`sent_to_email_at_date` DATETIME,
	PRIMARY KEY (`id`),
	KEY `ar_report_to_read_queue_for_online_index`(`seen_or_received_from_user`, `ar_user_id`),
	KEY `ar_report_to_read_queue_for_email_index`(`must_be_sent_to_email`, `ar_user_id`),
	INDEX `ar_report_to_read_FI_1` (`ar_report_id`),
	CONSTRAINT `ar_report_to_read_FK_1`
		FOREIGN KEY (`ar_report_id`)
		REFERENCES `ar_report` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_report_to_read_FI_2` (`ar_user_id`),
	CONSTRAINT `ar_report_to_read_FK_2`
		FOREIGN KEY (`ar_user_id`)
		REFERENCES `ar_user` (`id`)
		ON DELETE CASCADE
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_report_to_read_user_view
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_report_to_read_user_view`;


CREATE TABLE `ar_report_to_read_user_view`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_report_to_read_id` INTEGER,
	`ar_report_id` INTEGER,
	`ar_user_id` INTEGER,
	`seen_or_received_from_user` TINYINT default 0 NOT NULL,
	`ar_organization_unit_id` INTEGER,
	`from_date` DATETIME,
	`to_date` DATETIME,
	`produced_report_generation_date` DATETIME,
	`produced_report_short_description` VARCHAR(2048),
	`produced_report_additional_description` VARCHAR(2048),
	`produced_report_already_reviewed` TINYINT default 0 NOT NULL,
	`produced_report_is_draft` TINYINT default 0 NOT NULL,
	PRIMARY KEY (`id`),
	KEY `ar_report_to_read_user_view_I_1`(`from_date`),
	KEY `ar_report_to_read_user_view_I_2`(`to_date`),
	KEY `ar_report_to_read_user_view_I_3`(`produced_report_generation_date`),
	KEY `ar_report_to_read_user_view_I_4`(`produced_report_already_reviewed`),
	KEY `ar_report_to_read_user_view_I_5`(`produced_report_is_draft`),
	INDEX `ar_report_to_read_user_view_FI_1` (`ar_report_to_read_id`),
	CONSTRAINT `ar_report_to_read_user_view_FK_1`
		FOREIGN KEY (`ar_report_to_read_id`)
		REFERENCES `ar_report_to_read` (`id`),
	INDEX `ar_report_to_read_user_view_FI_2` (`ar_report_id`),
	CONSTRAINT `ar_report_to_read_user_view_FK_2`
		FOREIGN KEY (`ar_report_id`)
		REFERENCES `ar_report` (`id`),
	INDEX `ar_report_to_read_user_view_FI_3` (`ar_user_id`),
	CONSTRAINT `ar_report_to_read_user_view_FK_3`
		FOREIGN KEY (`ar_user_id`)
		REFERENCES `ar_user` (`id`),
	INDEX `ar_report_to_read_user_view_FI_4` (`ar_organization_unit_id`),
	CONSTRAINT `ar_report_to_read_user_view_FK_4`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_user_can_view_report
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_user_can_view_report`;


CREATE TABLE `ar_user_can_view_report`
(
	`ar_user_id` INTEGER  NOT NULL,
	`ar_report_id` INTEGER  NOT NULL,
	PRIMARY KEY (`ar_user_id`,`ar_report_id`),
	CONSTRAINT `ar_user_can_view_report_FK_1`
		FOREIGN KEY (`ar_user_id`)
		REFERENCES `ar_user` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_user_can_view_report_FI_2` (`ar_report_id`),
	CONSTRAINT `ar_user_can_view_report_FK_2`
		FOREIGN KEY (`ar_report_id`)
		REFERENCES `ar_report` (`id`)
		ON DELETE CASCADE
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_problem_type
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_problem_type`;


CREATE TABLE `ar_problem_type`
(
	`id` INTEGER  NOT NULL,
	`name` VARCHAR(512),
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_problem_domain
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_problem_domain`;


CREATE TABLE `ar_problem_domain`
(
	`id` INTEGER  NOT NULL,
	`name` VARCHAR(512),
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_problem_responsible
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_problem_responsible`;


CREATE TABLE `ar_problem_responsible`
(
	`id` INTEGER  NOT NULL,
	`name` VARCHAR(512),
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_problem_default_responsible
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_problem_default_responsible`;


CREATE TABLE `ar_problem_default_responsible`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_problem_domain_id` INTEGER,
	`ar_problem_responsible_id` INTEGER,
	PRIMARY KEY (`id`),
	INDEX `ar_problem_default_responsible_FI_1` (`ar_problem_domain_id`),
	CONSTRAINT `ar_problem_default_responsible_FK_1`
		FOREIGN KEY (`ar_problem_domain_id`)
		REFERENCES `ar_problem_domain` (`id`),
	INDEX `ar_problem_default_responsible_FI_2` (`ar_problem_responsible_id`),
	CONSTRAINT `ar_problem_default_responsible_FK_2`
		FOREIGN KEY (`ar_problem_responsible_id`)
		REFERENCES `ar_problem_responsible` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_current_problem
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_current_problem`;


CREATE TABLE `ar_current_problem`
(
	`duplication_key` VARCHAR(255)  NOT NULL,
	`ar_problem_type_id` INTEGER,
	`ar_problem_domain_id` INTEGER,
	`ar_problem_responsible_id` INTEGER,
	`created_at` DATETIME,
	`garbage_collection_key` VARCHAR(1024),
	`garbage_collection_from` DATETIME,
	`garbage_collection_to` DATETIME,
	`description` TEXT,
	`effect` TEXT,
	`proposed_solution` TEXT,
	`signaled_to_admin` TINYINT default 0 NOT NULL,
	`count_of_cdrs` BIGINT default 0 NOT NULL,
	PRIMARY KEY (`duplication_key`),
	INDEX `ar_current_problem_FI_1` (`ar_problem_type_id`),
	CONSTRAINT `ar_current_problem_FK_1`
		FOREIGN KEY (`ar_problem_type_id`)
		REFERENCES `ar_problem_type` (`id`),
	INDEX `ar_current_problem_FI_2` (`ar_problem_domain_id`),
	CONSTRAINT `ar_current_problem_FK_2`
		FOREIGN KEY (`ar_problem_domain_id`)
		REFERENCES `ar_problem_domain` (`id`),
	INDEX `ar_current_problem_FI_3` (`ar_problem_responsible_id`),
	CONSTRAINT `ar_current_problem_FK_3`
		FOREIGN KEY (`ar_problem_responsible_id`)
		REFERENCES `ar_problem_responsible` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_new_problem
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_new_problem`;


CREATE TABLE `ar_new_problem`
(
	`duplication_key` VARCHAR(255)  NOT NULL,
	`ar_problem_type_id` INTEGER,
	`ar_problem_domain_id` INTEGER,
	`ar_problem_responsible_id` INTEGER,
	`created_at` DATETIME,
	`garbage_collection_key` VARCHAR(1024),
	`garbage_collection_from` DATETIME,
	`garbage_collection_to` DATETIME,
	`description` TEXT,
	`effect` TEXT,
	`proposed_solution` TEXT,
	`signaled_to_admin` TINYINT default 0 NOT NULL,
	`count_of_cdrs` BIGINT default 0 NOT NULL,
	PRIMARY KEY (`duplication_key`),
	INDEX `ar_new_problem_FI_1` (`ar_problem_type_id`),
	CONSTRAINT `ar_new_problem_FK_1`
		FOREIGN KEY (`ar_problem_type_id`)
		REFERENCES `ar_problem_type` (`id`),
	INDEX `ar_new_problem_FI_2` (`ar_problem_domain_id`),
	CONSTRAINT `ar_new_problem_FK_2`
		FOREIGN KEY (`ar_problem_domain_id`)
		REFERENCES `ar_problem_domain` (`id`),
	INDEX `ar_new_problem_FI_3` (`ar_problem_responsible_id`),
	CONSTRAINT `ar_new_problem_FK_3`
		FOREIGN KEY (`ar_problem_responsible_id`)
		REFERENCES `ar_problem_responsible` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_job_queue
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_job_queue`;


CREATE TABLE `ar_job_queue`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`is_part_of` INTEGER,
	`state` INTEGER(1) default 0 NOT NULL,
	`created_at` DATETIME  NOT NULL,
	`start_at` DATETIME,
	`end_at` DATETIME,
	`description` VARCHAR(12000) default '' NOT NULL,
	`php_data_job_serialization` LONGTEXT,
	`internal_name` VARCHAR(512),
	PRIMARY KEY (`id`),
	KEY `ar_job_queue_I_1`(`is_part_of`),
	KEY `ar_job_queue_I_2`(`state`),
	KEY `ar_job_queue_I_3`(`created_at`),
	KEY `ar_job_queue_I_4`(`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_lock
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_lock`;


CREATE TABLE `ar_lock`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`name` VARCHAR(255)  NOT NULL,
	`time` DATETIME,
	`info` VARCHAR(255),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_lock_U_1` (`name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_application_upgrade
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_application_upgrade`;


CREATE TABLE `ar_application_upgrade`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`upg_key` VARCHAR(2048),
	`upg_output` TEXT,
	`installation_date` DATETIME,
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_logical_source
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_logical_source`;


CREATE TABLE `ar_logical_source`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`name` VARCHAR(255)  NOT NULL,
	`description` VARCHAR(2048),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_logical_source_U_1` (`name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_cdr_provider
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_cdr_provider`;


CREATE TABLE `ar_cdr_provider`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255),
	`description` VARCHAR(2048),
	`last_imported_id` BIGINT,
	`last_imported_data` VARCHAR(2048),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_cdr_provider_U_1` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_physical_format
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_physical_format`;


CREATE TABLE `ar_physical_format`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_logical_source_id` INTEGER,
	`name` VARCHAR(200),
	`description` VARCHAR(2048),
	PRIMARY KEY (`id`),
	UNIQUE KEY `avoid_conflicting_physical_formats` (`ar_logical_source_id`, `name`),
	CONSTRAINT `ar_physical_format_FK_1`
		FOREIGN KEY (`ar_logical_source_id`)
		REFERENCES `ar_logical_source` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_source_csv_file
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_source_csv_file`;


CREATE TABLE `ar_source_csv_file`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_cdr_provider_id` INTEGER,
	`ar_physical_format_id` INTEGER,
	`retrieved_from_server` VARCHAR(1024),
	`is_status` TINYINT  NOT NULL,
	`is_calldate_processed` TINYINT default 0 NOT NULL,
	`is_imported` TINYINT default 0 NOT NULL,
	`is_active_info` TINYINT default 1 NOT NULL,
	`min_calldate` DATETIME,
	`max_calldate` DATETIME,
	`name` VARCHAR(255)  NOT NULL,
	`archive_directory` VARCHAR(2048)  NOT NULL,
	`checksum` VARCHAR(2048),
	`receiving_date` DATETIME,
	`serious_processing_errors` TINYINT default 0 NOT NULL,
	`tot_lines` INTEGER,
	`lines_with_errors` INTEGER,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_source_csv_file_U_1` (`name`),
	KEY `ar_source_csv_file_I_1`(`is_calldate_processed`),
	KEY `ar_source_csv_file_I_2`(`is_imported`),
	KEY `ar_source_csv_file_I_3`(`is_active_info`),
	KEY `ar_source_csv_file_I_4`(`min_calldate`),
	KEY `ar_source_csv_file_I_5`(`max_calldate`),
	KEY `ar_source_csv_file_I_6`(`serious_processing_errors`),
	INDEX `ar_source_csv_file_FI_1` (`ar_cdr_provider_id`),
	CONSTRAINT `ar_source_csv_file_FK_1`
		FOREIGN KEY (`ar_cdr_provider_id`)
		REFERENCES `ar_cdr_provider` (`id`),
	INDEX `ar_source_csv_file_FI_2` (`ar_physical_format_id`),
	CONSTRAINT `ar_source_csv_file_FK_2`
		FOREIGN KEY (`ar_physical_format_id`)
		REFERENCES `ar_physical_format` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_source_cdr
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_source_cdr`;


CREATE TABLE `ar_source_cdr`
(
	`calldate` DATETIME  NOT NULL,
	`id` INTEGER  NOT NULL,
	`ar_cdr_provider_id` INTEGER  NOT NULL,
	`ar_physical_format_id` INTEGER  NOT NULL,
	`content` VARCHAR(10000),
	PRIMARY KEY (`calldate`,`id`,`ar_cdr_provider_id`,`ar_physical_format_id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_type_of_source_cdr
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_type_of_source_cdr`;


CREATE TABLE `ar_type_of_source_cdr`
(
	`ar_cdr_provider_id` INTEGER  NOT NULL,
	`ar_physical_format_id` INTEGER  NOT NULL,
	PRIMARY KEY (`ar_cdr_provider_id`,`ar_physical_format_id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_voip_extension_to_move
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_voip_extension_to_move`;


CREATE TABLE `ar_voip_extension_to_move`
(
	`extension` VARCHAR(255)  NOT NULL,
	PRIMARY KEY (`extension`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_remote_file
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_remote_file`;


CREATE TABLE `ar_remote_file`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_cdr_provider_id` INTEGER,
	`name` VARCHAR(255)  NOT NULL,
	`checksum` VARCHAR(2048),
	`receiving_date` DATETIME,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_remote_file_U_1` (`name`),
	INDEX `ar_remote_file_FI_1` (`ar_cdr_provider_id`),
	CONSTRAINT `ar_remote_file_FK_1`
		FOREIGN KEY (`ar_cdr_provider_id`)
		REFERENCES `ar_cdr_provider` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_local_file_to_delete
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_local_file_to_delete`;


CREATE TABLE `ar_local_file_to_delete`
(
	`name` VARCHAR(1024)  NOT NULL,
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_temp_source_cdr
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_temp_source_cdr`;


CREATE TABLE `ar_temp_source_cdr`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_temp_source_cdr_to_dest_cdr
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_temp_source_cdr_to_dest_cdr`;


CREATE TABLE `ar_temp_source_cdr_to_dest_cdr`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_temp_problem
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_temp_problem`;


CREATE TABLE `ar_temp_problem`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_instance_status
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_instance_status`;


CREATE TABLE `ar_instance_status`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255),
	`instance_code` VARCHAR(1024),
	`ar_organization_unit_id` INTEGER,
	`application_version` VARCHAR(1024),
	`nr_of_critical_errors` INTEGER,
	`nr_of_important_errors` INTEGER,
	`nr_of_warning_errors` INTEGER,
	`nr_of_extensions` INTEGER,
	`nr_of_unspecified_extensions` INTEGER,
	`property_nr_errors_outgoing_previous_month` INTEGER,
	`property_nr_errors_incoming_previous_month` INTEGER,
	`property_nr_errors_internal_previous_month` INTEGER,
	`property_nr_errors_outgoing_last_30_days` INTEGER,
	`property_nr_errors_incoming_last_30_days` INTEGER,
	`property_nr_errors_internal_last_30_days` INTEGER,
	`property_nr_outgoing_previous_month` INTEGER,
	`property_nr_incoming_previous_month` INTEGER,
	`property_nr_internal_previous_month` INTEGER,
	`property_nr_outgoing_last_30_days` INTEGER,
	`property_nr_incoming_last_30_days` INTEGER,
	`property_nr_internal_last_30_days` INTEGER,
	`last_processed_cdr_timestamp` DATETIME,
	`info_timestamp` DATETIME,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_instance_status_U_1` (`internal_name`),
	INDEX `ar_instance_status_FI_1` (`ar_organization_unit_id`),
	CONSTRAINT `ar_instance_status_FK_1`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_daily_status_job
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_daily_status_job`;


CREATE TABLE `ar_daily_status_job`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`name` VARCHAR(200)  NOT NULL,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_daily_status_job_U_1` (`name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_daily_status_change
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_daily_status_change`;


CREATE TABLE `ar_daily_status_change`
(
	`day` DATE  NOT NULL,
	`ar_daily_status_job_id` INTEGER  NOT NULL,
	PRIMARY KEY (`day`,`ar_daily_status_job_id`),
	INDEX `ar_daily_status_change_FI_1` (`ar_daily_status_job_id`),
	CONSTRAINT `ar_daily_status_change_FK_1`
		FOREIGN KEY (`ar_daily_status_job_id`)
		REFERENCES `ar_daily_status_job` (`id`)
		ON DELETE CASCADE
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_holiday
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_holiday`;


CREATE TABLE `ar_holiday`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`day_of_month` INTEGER,
	`month` INTEGER,
	`year` INTEGER,
	`day_of_week` INTEGER,
	`from_hour` INTEGER,
	`from_minutes` INTEGER,
	`to_hour` INTEGER,
	`to_minutes` INTEGER,
	`peak_code` VARCHAR(255)  NOT NULL,
	`name` VARCHAR(1024),
	PRIMARY KEY (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_cached_organization_info
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_cached_organization_info`;


CREATE TABLE `ar_cached_organization_info`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255),
	`content` LONGBLOB,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_cached_organization_info_U_1` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_extension_to_pincode
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_extension_to_pincode`;


CREATE TABLE `ar_extension_to_pincode`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`extension` VARCHAR(255)  NOT NULL,
	`pincode` VARCHAR(255)  NOT NULL,
	`from_date` DATETIME  NOT NULL,
	PRIMARY KEY (`id`),
	KEY `ar_extension_to_pincode_I_1`(`extension`),
	KEY `ar_extension_to_pincode_I_2`(`pincode`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_bundle_state
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_bundle_state`;


CREATE TABLE `ar_bundle_state`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`to_time` DATETIME  NOT NULL,
	`data_file` LONGBLOB,
	PRIMARY KEY (`id`),
	KEY `ar_bundle_state_I_1`(`to_time`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_service
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_service`;


CREATE TABLE `ar_service`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255),
	`customer_name` VARCHAR(255),
	`customer_description` VARCHAR(2048),
	`vendor_name` VARCHAR(255),
	`vendor_description` VARCHAR(2048),
	`external_crm_code` VARCHAR(255),
	`customer_price_depend_from_activation_date` TINYINT default 0 NOT NULL,
	`customer_price_change_with_price_list` TINYINT default 0 NOT NULL,
	`is_enabled` TINYINT default 1 NOT NULL,
	`is_applied_only_one_time` TINYINT default 0 NOT NULL,
	`schedule_timeframe` VARCHAR(255),
	`was_compiled` TINYINT default 0 NOT NULL,
	`schedule_from` VARCHAR(255),
	`schedule_at` TIME default '00:00:00',
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_service_U_1` (`internal_name`),
	KEY `ar_service_I_1`(`was_compiled`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_service_price
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_service_price`;


CREATE TABLE `ar_service_price`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255),
	`ar_service_id` INTEGER  NOT NULL,
	`from_date` DATETIME  NOT NULL,
	`price` BIGINT  NOT NULL,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_service_price_U_1` (`internal_name`),
	KEY `ar_service_price_I_1`(`from_date`),
	INDEX `ar_service_price_FI_1` (`ar_service_id`),
	CONSTRAINT `ar_service_price_FK_1`
		FOREIGN KEY (`ar_service_id`)
		REFERENCES `ar_service` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_assigned_service
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_assigned_service`;


CREATE TABLE `ar_assigned_service`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255),
	`external_crm_code` VARCHAR(255),
	`from_date` DATETIME  NOT NULL,
	`ar_service_id` INTEGER  NOT NULL,
	`ar_organization_unit_id` INTEGER  NOT NULL,
	`nr_of_items` BIGINT,
	`discount` BIGINT  NOT NULL,
	`note` VARCHAR(1024),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_assigned_service_U_1` (`internal_name`),
	KEY `ar_assigned_service_I_1`(`from_date`),
	INDEX `ar_assigned_service_FI_1` (`ar_service_id`),
	CONSTRAINT `ar_assigned_service_FK_1`
		FOREIGN KEY (`ar_service_id`)
		REFERENCES `ar_service` (`id`),
	INDEX `ar_assigned_service_FI_2` (`ar_organization_unit_id`),
	CONSTRAINT `ar_assigned_service_FK_2`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_cached_grouped_cdr
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_cached_grouped_cdr`;


CREATE TABLE `ar_cached_grouped_cdr`
(
	`cached_parent_id_hierarchy` VARBINARY(850)  NOT NULL,
	`billable_ar_organization_unit_id` INTEGER  NOT NULL,
	`calldate` DATE  NOT NULL,
	`destination_type` SMALLINT  NOT NULL,
	`ar_communication_channel_type_id` INTEGER  NOT NULL,
	`operator_type` VARCHAR(255)  NOT NULL,
	`ar_vendor_id` INTEGER  NOT NULL,
	`geographic_location` VARCHAR(255)  NOT NULL,
	`count_of_calls` BIGINT  NOT NULL,
	`billsec` BIGINT  NOT NULL,
	`income` BIGINT  NOT NULL,
	`cost_saving` BIGINT  NOT NULL,
	`cost` BIGINT  NOT NULL,
	`count_of_records` BIGINT  NOT NULL,
	`id` SMALLINT default 0 NOT NULL,
	PRIMARY KEY (`cached_parent_id_hierarchy`,`billable_ar_organization_unit_id`,`calldate`,`destination_type`,`ar_communication_channel_type_id`,`operator_type`,`ar_vendor_id`,`geographic_location`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_cached_errors
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_cached_errors`;


CREATE TABLE `ar_cached_errors`
(
	`calldate` DATE  NOT NULL,
	`destination_type` SMALLINT  NOT NULL,
	`error_destination_type` SMALLINT default 0 NOT NULL,
	`count_of_calls` BIGINT  NOT NULL,
	PRIMARY KEY (`calldate`,`destination_type`,`error_destination_type`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_test_cached_grouped_cdr
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_test_cached_grouped_cdr`;


CREATE TABLE `ar_test_cached_grouped_cdr`
(
	`cached_parent_id_hierarchy` VARBINARY(850)  NOT NULL,
	`billable_ar_organization_unit_id` INTEGER  NOT NULL,
	`calldate` DATE  NOT NULL,
	`destination_type` SMALLINT  NOT NULL,
	`ar_communication_channel_type_id` INTEGER  NOT NULL,
	`operator_type` VARCHAR(255)  NOT NULL,
	`ar_vendor_id` INTEGER  NOT NULL,
	`geographic_location` VARCHAR(255)  NOT NULL,
	`count_of_calls` BIGINT  NOT NULL,
	`billsec` BIGINT  NOT NULL,
	`income` BIGINT  NOT NULL,
	`cost_saving` BIGINT  NOT NULL,
	`cost` BIGINT  NOT NULL,
	`id` SMALLINT default 0 NOT NULL,
	PRIMARY KEY (`cached_parent_id_hierarchy`,`billable_ar_organization_unit_id`,`calldate`,`destination_type`,`ar_communication_channel_type_id`,`operator_type`,`ar_vendor_id`,`geographic_location`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

#-----------------------------------------------------------------------------
#-- ar_test_cached_errors
#-----------------------------------------------------------------------------

DROP TABLE IF EXISTS `ar_test_cached_errors`;


CREATE TABLE `ar_test_cached_errors`
(
	`calldate` DATE  NOT NULL,
	`destination_type` SMALLINT  NOT NULL,
	`error_destination_type` SMALLINT default 0 NOT NULL,
	`count_of_calls` BIGINT  NOT NULL,
	PRIMARY KEY (`calldate`,`destination_type`,`error_destination_type`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;

# This restores the fkey checks, after having unset them earlier
SET FOREIGN_KEY_CHECKS = 1;
