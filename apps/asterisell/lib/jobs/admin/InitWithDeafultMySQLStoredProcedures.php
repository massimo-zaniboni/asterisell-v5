<?php

/* $LICENSE 2012, 2013, 2015:
 *
 * Copyright (C) 2012, 2013, 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Add/update MySQL Stored procedures code to the application,
 * and compile the code of the Rating Engine.
 *
 * DEV-NOTE: at every application upgrade, these constraints are automatically added.
 */
class InitWithDefaultMySQLStoredProcedures extends AdminJobProcessor
{

    const GARBAGE_KEY = 'InitWithDefaultMySQLStoredProcedures';

    public
    function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        ArProblemException::garbageCollect(self::GARBAGE_KEY, null, null);

        // Delete this job because it is not used anymore.
        $jobId = ManageRateEvent::getRateEngineChangedDaysJobId();
        if (!is_null($jobId)) {
            $j = ArDailyStatusJobPeer::retrieveByPK($jobId);
            $j->delete();
        }

        $log = $this->compileMySQLProcedures();

        return $log;
    }

    /**
     * Create a trigger that invalidate cached organization info, and force a rerating.
     *
     * @param string $tableName
     * @return string
     */
    protected function generateTriggerForReratingEvent($tableName)
    {

        $r = '';

        $events = array('INSERT', 'UPDATE', 'DELETE');

        foreach ($events as $event) {

            $triggerName = 'invalidate_cached_' . $tableName . '_' . $event;

            $r .= 'DROP TRIGGER IF EXISTS ' . $triggerName . '$$' . "\n";
            $r .= 'CREATE TRIGGER ' . $triggerName . ' AFTER ' . $event . ' ON ' . $tableName
                . ' FOR EACH ROW BEGIN '
                . ' CALL schedule_rerating(); '
                . ' END$$' . "\n";
        }

        return $r;
    }

    protected
    function compileMySQLProcedures()
    {

        $sqlCode = '';

        $sqlCode .= '

/** Remove old code */
DROP TRIGGER IF EXISTS export_ar_number_portability$$
DROP TRIGGER IF EXISTS clone_ar_number_portability$$
DROP TRIGGER IF EXISTS change_event_trigger_1$$
DROP TRIGGER IF EXISTS change_event_trigger_2$$
DROP TRIGGER IF EXISTS change_event_trigger_3$$
';

        $sqlCode .= '

/**
 * Get the ported telephone number, or NULL if it does not exists, of a source telephone number.
 */
DROP FUNCTION IF EXISTS get_ported_telephone_number$$
CREATE FUNCTION get_ported_telephone_number(source_number VARCHAR(1024), at_date DATETIME)
RETURNS VARCHAR(1024)
READS SQL DATA
BEGIN
  DECLARE dest_number VARCHAR(1024);

  SET dest_number = NULL;

  SELECT ported_telephone_number
  FROM ar_number_portability
  WHERE telephone_number = source_number
  AND from_date <= at_date
  ORDER BY from_date DESC
  LIMIT 1
  INTO dest_number;

  RETURN(dest_number);
END$$
';


        $sqlCode .= <<<SQL
/**
 * Notify each registered ar_daily_status_job that
 * there is a change in the CDRs of the day.
 * This function is called fromm the rating engine.
 */
DROP PROCEDURE IF EXISTS add_daily_status_change_event$$
CREATE PROCEDURE add_daily_status_change_event(d1 DATETIME, is_service TINYINT)
MODIFIES SQL DATA
BEGIN
  INSERT INTO ar_daily_status_change(day, is_service_cdr, ar_daily_status_job_id)
  SELECT DATE(d1), is_service, j.id FROM ar_daily_status_job AS j
  ON DUPLICATE KEY UPDATE day = day, is_service_cdr = is_service_cdr, ar_daily_status_job_id = ar_daily_status_job_id;
END
$$

DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_1$$

DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_2$$

DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_3$$

DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_4$$

DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_5$$

DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_6$$

/**
 * Schedule a rerating and invalidate the various caches.
 */
DROP PROCEDURE IF EXISTS schedule_rerating$$
CREATE PROCEDURE schedule_rerating()
MODIFIES SQL DATA
BEGIN

  DELETE FROM ar_cached_organization_info;

  UPDATE ar_params
  SET wait_for_scheduled_rerate = 1,
      scheduled_rerate_from_official_calldate = 1;

END $$

DROP TRIGGER IF EXISTS compile_rate_trigger$$
CREATE TRIGGER compile_rate_trigger
BEFORE UPDATE ON ar_rate
FOR EACH ROW
BEGIN

  IF NOT (NEW.was_compiled = 1 AND OLD.was_compiled = 0) THEN
    SET NEW.was_compiled = 0;
  END IF;

END $$

DROP TRIGGER IF EXISTS send_service_trigger$$
CREATE TRIGGER send_service_trigger
BEFORE UPDATE ON ar_service
FOR EACH ROW
BEGIN
  IF NOT (NEW.was_compiled = 1 AND OLD.was_compiled = 0) THEN
    SET NEW.was_compiled = 0;
  END IF;
END $$

DROP TRIGGER IF EXISTS type_of_source_cdr$$
CREATE TRIGGER type_of_source_cdr
AFTER INSERT  ON ar_source_cdr
FOR EACH ROW
BEGIN
  INSERT IGNORE INTO ar_type_of_source_cdr(ar_cdr_provider_id, ar_physical_format_id)
  VALUES(NEW.ar_cdr_provider_id, NEW.ar_physical_format_id);
END $$

SQL;

        $tablesWithRerating = array(
            'ar_organization_unit'
        , 'ar_organization_unit_has_structure'
        , 'ar_organization_unit_type'
        , 'ar_party'
        , 'ar_vendor'
        , 'ar_rate'
        , 'ar_telephone_prefix'
        , 'ar_vendor_domain'
        , 'ar_holiday'
        , 'ar_service'
        , 'ar_service_price'
        , 'ar_assigned_service'
        );

        foreach ($tablesWithRerating as $tableName) {
            $sqlCode .= $this->generateTriggerForReratingEvent($tableName);
        }

        $this->sendSQLProceduresToMySQLFromString($sqlCode);
        $this->sendSQLProceduresToMySQLFromString($this->getReportMySQLProcedures());

        return 'Updaded MySQL stored procedures libray.';
    }

    /**
     * The logic for saying if a report can be seen or not from a user.
     *
     * NOTE: if you change this code, you must install again CDR processing rules.
     *
     * @return string the MySQL code separated by $$
     */
    public static function getReportMySQLProcedures()
    {

        $add_new_report = self::generateUserCanViewReportQuery(null, 'NEW.id');
        $add_new_user = self::generateUserCanViewReportQuery('NEW.id', null);
        $add_new_user_permission = self::generateUserCanViewReportQuery('NEW.ar_user_id', null);
        $add_removed_user_permission = self::generateUserCanViewReportQuery('OLD.ar_user_id', null);
        $recalc_all = self::generateUserCanViewReportQuery(null, null);

        return <<<SQL

/**
 * All the permissions of a user, expanding the permission associated to his roles.
 * This view simplify queries.
 */

/* DEV-NOTE: mantain TABLE, instead of VIEW, because it replace  a temporary table created with the database. */
DROP TABLE IF EXISTS ar_view_all_user_permissions $$
CREATE OR REPLACE
VIEW ar_view_all_user_permissions
AS
SELECT rel.ar_user_id AS ar_user_id,
rel.ar_permission_id AS ar_permission_id
FROM ar_user_has_permission AS rel
UNION DISTINCT
SELECT rel.ar_user_id AS ar_user_id,
role.ar_permission_id AS ar_permission_id
FROM   ar_user_has_role AS rel, ar_role_has_permission AS role
WHERE  rel.ar_role_id = role.ar_role_id
$$

/* DEV-NOTE: mantain TABLE, instead of VIEW, because it replace  a temporary table created with the database. */
DROP TABLE IF EXISTS ar_report_to_read_user_view $$
CREATE OR REPLACE
VIEW ar_report_to_read_user_view
AS
SELECT  rr.id AS id,
        rr.id AS ar_report_to_read_id,
        rr.ar_report_id AS ar_report_id,
        rr.ar_user_id AS ar_user_id,
        rr.seen_or_received_from_user AS seen_or_received_from_user,
        rp.ar_organization_unit_id AS ar_organization_unit_id,
        rp.from_date AS from_date,
        rp.to_date AS to_date,
        rp.produced_report_generation_date AS produced_report_generation_date,
        rp.produced_report_short_description AS produced_report_short_description,
        rp.produced_report_additional_description AS produced_report_additional_description,
        rp.produced_report_already_reviewed AS produced_report_already_reviewed,
        rp.produced_report_is_draft AS produced_report_is_draft
FROM
  ar_report_to_read AS rr,
  ar_report AS rp
WHERE
  rr.ar_report_id = rp.id
$$

/**
 * Mantains incrementally the state of the ar_report_to_read_user_view table,
 * intercepting all the changes events that can affect the user permissions on documents.
 *
 * So relationships between users and reports can be maintained incrementally,
 * without reverting to a view that must be calculated every time.
 *
 * NOTE: changes to ar_organization_unit_has_structure are using directly a regeneration of the report,
 * after processing of the events on the recalc table.
 */
DROP TRIGGER IF EXISTS fast_add_user_can_view_report_1 $$
CREATE TRIGGER fast_add_user_can_view_report_1
AFTER INSERT ON ar_report
FOR EACH ROW
BEGIN
  $add_new_report
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_2 $$
CREATE TRIGGER fast_add_user_can_view_report_2
AFTER UPDATE ON ar_report
FOR EACH ROW
BEGIN
  DELETE FROM ar_user_can_view_report WHERE ar_report_id = NEW.id;
  $add_new_report
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_2d $$
CREATE TRIGGER fast_add_user_can_view_report_2d
AFTER DELETE ON ar_report
FOR EACH ROW
BEGIN
  DELETE FROM ar_user_can_view_report WHERE ar_report_id = OLD.id;
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_3 $$
CREATE TRIGGER fast_add_user_can_view_report_3
AFTER INSERT ON ar_user
FOR EACH ROW
BEGIN

  $add_new_user
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_4 $$
CREATE TRIGGER fast_add_user_can_view_report_4
AFTER UPDATE ON ar_user
FOR EACH ROW
BEGIN

  DELETE FROM ar_user_can_view_report WHERE ar_user_id = NEW.id;
  $add_new_user
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_4b $$
CREATE TRIGGER fast_add_user_can_view_report_4b
AFTER DELETE ON ar_user
FOR EACH ROW
BEGIN

  DELETE FROM ar_user_can_view_report WHERE ar_user_id = OLD.id;
END
$$


DROP TRIGGER IF EXISTS fast_add_user_can_view_report_5 $$
CREATE TRIGGER fast_add_user_can_view_report_5
AFTER UPDATE ON ar_user_has_permission
FOR EACH ROW
BEGIN
  DELETE FROM ar_user_can_view_report WHERE ar_user_id = NEW.ar_user_id;
  $add_new_user_permission
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_6 $$
CREATE TRIGGER fast_add_user_can_view_report_6
AFTER INSERT ON ar_user_has_permission
FOR EACH ROW
BEGIN
  DELETE FROM ar_user_can_view_report WHERE ar_user_id = NEW.ar_user_id;
  $add_new_user_permission
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_7 $$
CREATE TRIGGER fast_add_user_can_view_report_7
AFTER DELETE ON ar_user_has_permission
FOR EACH ROW
BEGIN
  DELETE FROM ar_user_can_view_report WHERE ar_user_id = OLD.ar_user_id;
  $add_removed_user_permission
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_8 $$
CREATE TRIGGER fast_add_user_can_view_report_8
AFTER UPDATE ON ar_user_has_role
FOR EACH ROW
BEGIN
  DELETE FROM ar_user_can_view_report WHERE ar_user_id = NEW.ar_user_id;
  $add_new_user_permission
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_9 $$
CREATE TRIGGER fast_add_user_can_view_report_9
AFTER INSERT ON ar_user_has_role
FOR EACH ROW
BEGIN
  DELETE FROM ar_user_can_view_report WHERE ar_user_id = NEW.ar_user_id;
  $add_new_user_permission
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_10 $$
CREATE TRIGGER fast_add_user_can_view_report_10
AFTER DELETE ON ar_user_has_role
FOR EACH ROW
BEGIN
  DELETE FROM ar_user_can_view_report WHERE ar_user_id = OLD.ar_user_id;
  $add_removed_user_permission
END
$$

DROP PROCEDURE IF EXISTS create_reports_from_other_report $$
CREATE PROCEDURE create_reports_from_other_report(var_source_report_id INTEGER, var_dest_report_id INTEGER)
MODIFIES SQL DATA
BEGIN

  INSERT INTO ar_report_also_for(ar_report_id, ar_role_id)
  SELECT var_dest_report_id, s.ar_role_id
  FROM ar_report_also_for AS s
  WHERE s.ar_report_id = var_source_report_id;

END
$$

SQL;

    }

    /**
     * The reports visible from users.
     * Return an INSERT statement, that update incrementally the table with the reports that can be viewed
     * from users.
     *
     * @param string|null $filterOnUserId a condition on useri_id
     * @param string|null $filterOnReportId a condition on report_id
     * @return string
     */
    static
    protected function generateUserCanViewReportQuery($filterOnUserId, $filterOnReportId)
    {

        if (is_null($filterOnUserId)) {
            $sql_filterOnUserId = '';
        } else {
            $sql_filterOnUserId = "AND ar_user.id = $filterOnUserId";
        }

        if (is_null($filterOnReportId)) {
            $sql_filterOnReportId = '';
        } else {
            $sql_filterOnReportId = "AND ar_report.id = $filterOnReportId";
        }

        $can_view_reports = ArPermission::CAN_VIEW_REPORTS;
        $can_view_complete_telephone_numbers = ArPermission::CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS;

        $testGlobalPermissions = <<<SQL
((NOT ar_report.param_show_call_cost) OR ar_global_permissions.show_call_cost)
AND ((NOT ar_report.param_show_call_income) OR ar_global_permissions.show_call_income)
AND ((NOT ar_report.param_show_also_outgoing_calls) OR ar_global_permissions.show_outgoing_calls)
AND ((NOT ar_report.param_show_also_incoming_calls) OR ar_global_permissions.show_incoming_calls)
AND ((NOT ar_report.param_show_also_internal_calls) OR ar_global_permissions.show_internal_calls)
AND ((NOT ar_report.param_show_voip_provider) OR ar_global_permissions.show_voip_provider)
AND ((NOT ar_report.param_show_communication_channel) OR ar_global_permissions.show_communication_channel)
AND ((NOT ar_report.param_show_cost_saving) OR ar_global_permissions.show_cost_saving)
SQL;

        $testUserPermissions = <<<SQL
AND ar_view_all_user_permissions.ar_user_id = ar_user.id
AND (ar_report.param_show_masked_telephone_numbers
     OR EXISTS (SELECT ap.ar_user_id
                FROM ar_view_all_user_permissions AS ap
                WHERE ap.ar_user_id = ar_user.id
                AND ap.ar_permission_id = $can_view_complete_telephone_numbers))
AND ar_user.is_enabled
SQL;

        /**
         * @var string
         *
         * NOTE: also admin can view the same type of reviewed reports.
         * The non reviewed reports are accessible to users, using a different process.
         */
        $reportIsViewable = <<<SQL

ar_report.produced_report_already_reviewed
AND (NOT ar_report.is_template)

SQL;

        return <<<SQL
INSERT INTO ar_user_can_view_report(ar_user_id, ar_report_id)
SELECT DISTINCT ar_user.id AS ar_user_id, ar_report.id AS ar_report_id
FROM ar_report,
ar_user,
ar_view_all_user_permissions,
ar_user_has_role,
ar_global_permissions,
ar_report_also_for
WHERE $reportIsViewable
AND (ar_report.ar_organization_unit_id = ar_user.ar_organization_unit_id
     OR (ar_user.ar_organization_unit_id IS NULL AND ar_report.ar_organization_unit_id IS NULL))
AND ar_view_all_user_permissions.ar_user_id = ar_user.id
AND ar_view_all_user_permissions.ar_permission_id = $can_view_reports
$testUserPermissions
$sql_filterOnReportId
$sql_filterOnUserId
AND (ar_user.is_root_admin
     OR ((NOT ar_user.is_root_admin) AND $testGlobalPermissions))
AND ar_report_also_for.ar_report_id = ar_report.id
AND ar_user_has_role.ar_user_id = ar_user.id
AND ar_user_has_role.ar_role_id = ar_report_also_for.ar_role_id
;

SQL;

    }
}
