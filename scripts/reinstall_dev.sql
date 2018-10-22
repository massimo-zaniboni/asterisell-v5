
# Do not execute initial upgrade jobs.
DELETE
FROM ar_application_upgrade
WHERE NOT
(  upg_key LIKE 'Upgrade%'
OR upg_key LIKE 'EmptyUpgrade%');

TRUNCATE TABLE ar_assigned_service                ;
TRUNCATE TABLE ar_bundle_state                    ;
TRUNCATE TABLE ar_cached_organization_info        ;
TRUNCATE TABLE ar_cdr                             ;

# In case of ITEC they are created dinamically and last-id info is important
# TRUNCATE TABLE ar_cdr_provider                    ;

TRUNCATE TABLE ar_communication_channel_type      ;
TRUNCATE TABLE ar_current_problem                 ;
TRUNCATE TABLE ar_daily_status_change             ;
TRUNCATE TABLE ar_daily_status_job                ;
TRUNCATE TABLE ar_destination_type                ;
TRUNCATE TABLE ar_expanded_extensions             ;
TRUNCATE TABLE ar_extension_to_pincode            ;
TRUNCATE TABLE ar_global_permissions              ;
TRUNCATE TABLE ar_holiday                         ;
TRUNCATE TABLE ar_instance_status                 ;
TRUNCATE TABLE ar_job_queue                       ;
TRUNCATE TABLE ar_legal_date_generation_method    ;
TRUNCATE TABLE ar_lock                            ;

# They are created on-demand at run-time,
# so preserve the id.
# TRUNCATE TABLE ar_logical_source                  ;
# TRUNCATE TABLE ar_physical_format                 ;

TRUNCATE TABLE ar_new_problem                     ;

# Sometime there is a lot of info in this table.
# TRUNCATE TABLE ar_number_portability              ;

TRUNCATE TABLE ar_organization_backup_of_changes  ;
TRUNCATE TABLE ar_organization_unit               ;
TRUNCATE TABLE ar_organization_unit_has_structure ;
TRUNCATE TABLE ar_organization_unit_type          ;
TRUNCATE TABLE ar_params                          ;
TRUNCATE TABLE ar_party                           ;
TRUNCATE TABLE ar_party_has_tag                   ;
TRUNCATE TABLE ar_permission                      ;
TRUNCATE TABLE ar_postponed_report                ;
TRUNCATE TABLE ar_postponed_report_tmp            ;
TRUNCATE TABLE ar_problem_default_responsible     ;
TRUNCATE TABLE ar_problem_domain                  ;
TRUNCATE TABLE ar_problem_responsible             ;
TRUNCATE TABLE ar_problem_type                    ;
TRUNCATE TABLE ar_rate                            ;
TRUNCATE TABLE ar_rate_category                   ;
TRUNCATE TABLE ar_rate_format                     ;
TRUNCATE TABLE ar_rate_shared_with_reseller       ;
TRUNCATE TABLE ar_rating_engine_export_status     ;

# Do not load again remote files
# TRUNCATE TABLE ar_remote_file                     ;

TRUNCATE TABLE ar_report                          ;
TRUNCATE TABLE ar_report_also_for                 ;
TRUNCATE TABLE ar_report_generation               ;
TRUNCATE TABLE ar_report_order_of_children        ;
TRUNCATE TABLE ar_report_scheduler                ;
TRUNCATE TABLE ar_report_set                      ;
TRUNCATE TABLE ar_report_to_read                  ;
TRUNCATE TABLE ar_reseller                        ;
TRUNCATE TABLE ar_role                            ;
TRUNCATE TABLE ar_role_has_permission             ;
TRUNCATE TABLE ar_service                         ;
TRUNCATE TABLE ar_service_price                   ;

# Mantain the loaded cdrs, hoping that ID of providers and formats will not change
# TRUNCATE TABLE ar_source_cdr                      ;
# TRUNCATE TABLE ar_source_csv_file                 ;

TRUNCATE TABLE ar_tag                             ;
TRUNCATE TABLE ar_telephone_prefix                ;
TRUNCATE TABLE ar_temp_problem                    ;
TRUNCATE TABLE ar_temp_source_cdr                 ;
TRUNCATE TABLE ar_temp_source_cdr_to_dest_cdr     ;
TRUNCATE TABLE ar_user                            ;
TRUNCATE TABLE ar_user_can_view_report            ;
TRUNCATE TABLE ar_user_change_password_request    ;
TRUNCATE TABLE ar_user_has_permission             ;
TRUNCATE TABLE ar_user_has_role                   ;
TRUNCATE TABLE ar_vendor                          ;
TRUNCATE TABLE ar_vendor_domain                   ;
TRUNCATE TABLE ar_voip_extension_to_move          ;
