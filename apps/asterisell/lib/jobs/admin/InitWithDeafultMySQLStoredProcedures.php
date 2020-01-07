<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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
        $jobId = self::getRateEngineChangedDaysJobId();
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

        $sqlCode = <<<SQL

/** Remove old code */
DROP TRIGGER IF EXISTS export_ar_number_portability$$
DROP TRIGGER IF EXISTS clone_ar_number_portability$$
DROP TRIGGER IF EXISTS change_event_trigger_1$$
DROP TRIGGER IF EXISTS change_event_trigger_2$$
DROP TRIGGER IF EXISTS change_event_trigger_3$$
DROP TRIGGER IF EXISTS recalc_add_user_can_view_report_2$$
DROP TRIGGER IF EXISTS recalc_add_user_can_view_report_1$$
DROP PROCEDURE IF EXISTS add_data_change_event$$

DROP TRIGGER IF EXISTS invalidate_cached_ar_wholesale_number_INSERT$$
DROP TRIGGER IF EXISTS invalidate_cached_ar_wholesale_number_UPDATE$$
DROP TRIGGER IF EXISTS invalidate_cached_ar_wholesale_number_DELETE$$

DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_1$$
DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_2$$
DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_3$$
DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_4$$
DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_5$$
DROP TRIGGER IF EXISTS add_daily_status_change_event_trigger_6$$

/** Force recalcs after upgrade */
DELETE FROM ar_cached_organization_info$$

SQL;

        $sqlCode .=
          $this->generateDeleteOnCascade('ar_party', array('ar_party_has_tag'))
        . $this->generateDeleteOnCascade('ar_tag', array('ar_party_has_tag'))
        . $this->generateDeleteOnCascade('ar_rate', array('ar_rate_shared_with_reseller'))
        . $this->generateDeleteOnCascade('ar_reseller', array('ar_rate_shared_with_reseller'))
        . $this->generateDeleteOnCascade('ar_user'
              , array('ar_user_has_role'
              , 'ar_user_has_permission'
              , 'ar_report_to_read'
              , 'ar_user_can_view_report'
              ))
        . $this->generateDeleteOnCascade('ar_role', array('ar_user_has_role', 'ar_report_also_for'))
        . $this->generateDeleteOnCascade('ar_permission', array('ar_user_has_permission'))
        . $this->generateDeleteOnCascade('ar_report_set'
              , array(
                   'ar_report'
                 , 'ar_postponed_report')
               , array('DELETE FROM ar_report
                        WHERE ar_report.about_ar_report_set_id = OLD.id;')
          )
        . $this->generateDeleteOnCascade('ar_organization_unit'
              , array('ar_postponed_report'
              , 'ar_postponed_report_tmp'
              , 'ar_report'
              ))
        . $this->generateDeleteOnCascade('ar_report'
              , array('ar_report_to_read'
              , 'ar_user_can_view_report'
              , 'ar_report_also_for')
              , array('UPDATE ar_report_set SET postponed_fields_are_updated = 0 WHERE id = OLD.ar_report_set_id;')
          )
        . $this->generateDeleteOnCascade('ar_daily_status_job', array('ar_daily_status_change'))
        ;

        $sqlCode .= <<<SQL

/**
 * Show only last active root customer info
 */
DROP TABLE IF EXISTS ar_root_customer_view $$
CREATE OR REPLACE VIEW ar_root_customer_view
AS SELECT sss.id AS ar_organization_unit_has_structure_id
FROM ar_organization_unit_has_structure  AS sss
INNER JOIN (
    SELECT ar_organization_unit_id, MAX(`from`) AS from_date
    FROM ar_organization_unit_has_structure
    WHERE ar_parent_organization_unit_id IS NULL
    GROUP BY ar_organization_unit_id) AS ss
ON  sss.ar_organization_unit_id = ss.ar_organization_unit_id
AND sss.`from` = ss.from_date
$$
   
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

/**
 * Add a ported telephone number, merging compatible dates together.
 */
DROP PROCEDURE IF EXISTS add_ported_telephone_number$$
CREATE PROCEDURE add_ported_telephone_number(source_number VARCHAR(1024), dest_number VARCHAR(1024), at_date DATETIME)
MODIFIES SQL DATA
BEGIN

  DECLARE empty_result INT DEFAULT 0;
  DECLARE found_dest_number VARCHAR(1024);
  DECLARE found_from_date DATETIME;

  DECLARE scan1 CURSOR FOR
  SELECT ported_telephone_number
  FROM ar_number_portability
  WHERE telephone_number = source_number
  AND   from_date <= at_date
  ORDER BY from_date DESC
  LIMIT 1;

  DECLARE scan2 CURSOR FOR
  SELECT ported_telephone_number, from_date
  FROM ar_number_portability
  WHERE telephone_number = source_number
  AND   from_date >= at_date
  ORDER BY from_date ASC
  LIMIT 1;

  DECLARE CONTINUE HANDLER FOR NOT FOUND SET empty_result = 1;

  SET empty_result = 0;
  OPEN scan1;
  FETCH scan1 INTO found_dest_number;
  CLOSE scan1;
  IF empty_result = 0 AND found_dest_number = dest_number THEN
      SET empty_result = 1;
      # dummy instruction and nothing to do:
      # there is already a porting of the number to the same number, in a date strictly in the past
  ELSE
    SET empty_result = 0;
    OPEN scan2;
    FETCH scan2 INTO found_dest_number, found_from_date;
    CLOSE scan2;
    IF empty_result = 0 AND found_dest_number = dest_number THEN
      # use this new better date in the past, instead of the strictly date in the future

      UPDATE ar_number_portability
      SET from_date = at_date
      WHERE telephone_number = source_number
      AND   from_date = found_from_date
      AND   ported_telephone_number = dest_number;
    ELSE
      # insert a new entry, replacing an old entry if it has the same telephone number and date

      REPLACE INTO ar_number_portability(telephone_number, ported_telephone_number, from_date)
      VALUES(source_number, dest_number, at_date);
    END IF;
  END IF;
END$$
SQL;

        $sqlCode .= <<<SQL
/**
 * Notify each registered ar_daily_status_job that
 * there is a change in the CDRs of the day.
 * This function is called from the rating engine.
 */
DROP PROCEDURE IF EXISTS add_daily_status_change_event$$
CREATE PROCEDURE add_daily_status_change_event(d1 DATETIME)
MODIFIES SQL DATA
BEGIN
  INSERT INTO ar_daily_status_change(day, ar_daily_status_job_id)
  SELECT DATE(d1), j.id FROM ar_daily_status_job AS j
  ON DUPLICATE KEY UPDATE day = day, ar_daily_status_job_id = ar_daily_status_job_id;
END
$$


/**
 * Leave for avoiding upgrading problems, but it is not any more used.
 */
DROP PROCEDURE IF EXISTS add_data_change_event$$
CREATE PROCEDURE add_data_change_event()
MODIFIES SQL DATA
BEGIN
  CALL schedule_rerating();
END $$

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

/**
 * Update ar_repost_set totals fields.
 */
DROP PROCEDURE IF EXISTS proc_update_postponed_reportset_amounts $$
CREATE PROCEDURE proc_update_postponed_reportset_amounts(report_set_id INTEGER)
MODIFIES SQL DATA
BEGIN
  UPDATE ar_report_set AS s
  , (SELECT COUNT(p.id) AS c, SUM(p.total_without_tax) AS t
     FROM ar_report AS p
     WHERE p.ar_report_set_id = report_set_id
  ) AS g
  SET s.reports = g.c
  ,   s.amount = IFNULL(g.t, 0)
  WHERE s.id = report_set_id
  ;

  UPDATE ar_report_set AS s
  , (SELECT COUNT(*) AS c
     FROM ar_postponed_report AS p
     WHERE p.ar_report_set_id = report_set_id
  ) AS g
  SET s.postponed_reports = g.c
  WHERE s.id = report_set_id
  ;

  /* NOTE: this query is rather slow because it involves scanning a lot of CDRs.
     It is probably repeated many times for each added report, so the hope is
     that its result will be cached.*/
  UPDATE ar_report_set AS s
  , (SELECT SUM(ar_cdr.income) AS s
     FROM ar_postponed_report AS p
     JOIN ar_report_set AS ss
     ON p.ar_report_set_id = ss.id
     , ar_cdr 
     WHERE p.ar_report_set_id = report_set_id
     AND ar_cdr.billable_ar_organization_unit_id = p.ar_organization_unit_id
     AND ar_cdr.calldate >= ss.from_date
     AND ar_cdr.calldate < ss.to_date
     ) AS g
  SET s.postponed_amount = IFNULL(g.s, 0)
  WHERE s.id = report_set_id;

  UPDATE ar_report_set
  SET postponed_fields_are_updated = 1
  WHERE id = report_set_id
  AND NOT postponed_fields_are_updated;
END
$$

/* NOTE: the insert, update and delete event of reports into report_set are already managed by other triggers. */

/**
 * Calculate support info that depends from the history.
 * NOTE: This is a service procedure that must be not called directly.
 */
DROP PROCEDURE IF EXISTS proc_update_wholesale_info $$
CREATE PROCEDURE         proc_update_wholesale_info()
MODIFIES SQL DATA
BEGIN

  /* the current read value */
  DECLARE v_id INTEGER;
  DECLARE v_n VARCHAR(255);
  DECLARE v_d DATETIME;
  DECLARE v_e TINYINT;
  DECLARE v_r INTEGER;

  /* the previous read value */
  DECLARE s_id INTEGER;
  DECLARE s_n VARCHAR(255);
  DECLARE s_d DATETIME;
  DECLARE s_e TINYINT DEFAULT 1;
  DECLARE s_r INTEGER;

  /* the n_info value to assign */
  DECLARE n_info VARCHAR(255);
  DECLARE n_is_last_record TINYINT;

  DECLARE end_loop INT;

  DECLARE scan1 CURSOR FOR
  SELECT id, telephone_number, from_date, ar_reseller_id, `exists`
  FROM ar_wholesale_number
  ORDER BY telephone_number, from_date ASC
  ;

  DECLARE CONTINUE HANDLER FOR NOT FOUND SET end_loop = 1;

  DELETE FROM ar_wholesale_update_proc;

  SET s_id = NULL;
  SET s_n = NULL;
  SET s_d = NULL;
  SET s_e = NULL;
  SET s_r = NULL;

  SET end_loop = 0;

  OPEN scan1;
  process_loop: LOOP

    FETCH scan1 INTO v_id, v_n, v_d, v_r, v_e;

    IF (s_id IS NOT NULL) AND (end_loop = 1 OR (NOT v_n = s_n)) THEN
      /* the last record was the most recent */
      UPDATE ar_wholesale_update_proc
      SET csv_is_current = 1
      WHERE foreign_id = s_id;
    END IF;

    IF end_loop = 1 THEN
      LEAVE process_loop;
    END IF;

    IF v_n = s_n THEN
      SET v_n = v_n;
      /* dummy */
    ELSE
     /* start with a new/fresh number */
      SET s_n = NULL;
      SET s_d = NULL;
      SET s_e = NULL;
      SET s_r = NULL;
    END IF;

    IF v_e = 0 THEN
      SET n_info = 'DELETE';
    ELSE
      IF s_n IS NULL THEN
        IF v_r IS NULL THEN
          SET n_info = 'CREATE-FREE';
        ELSE
          SET n_info = 'ASSIGN';
        END IF;
      ELSE
        IF v_r IS NULL THEN
          IF s_r IS NULL THEN
            SET n_info = 'UPDATE';
          ELSE
            SET n_info = 'FREE';
          END IF;
        ELSE
          IF v_r = s_r THEN
            SET n_info = 'UPDATE';
          ELSE
            IF s_r IS NULL THEN
              SET n_info = 'ASSIGN';
            ELSE
              SET n_info = 'MOVE';
            END IF;
          END IF;
        END IF;
      END IF;
    END IF;

    INSERT INTO ar_wholesale_update_proc(foreign_id, csv_comment, csv_last_date, csv_is_current)
    VALUES(v_id, n_info, s_d, 0);

    /* this is the new previous state */
    SET s_id = v_id;
    SET s_n = v_n;
    SET s_d = v_d;
    SET s_e = v_e;
    SET s_r = v_r;

  END LOOP process_loop;
  CLOSE scan1;

  /* Write phisically the data, only now, because there is no lock on the ar_wholesale_number table. */
  UPDATE ar_wholesale_number AS d
  INNER JOIN ar_wholesale_update_proc AS s
  ON d.id = s.foreign_id
  SET d.csv_comment = s.csv_comment
    , d.csv_last_date = s.csv_last_date
    , d.csv_is_current = s.csv_is_current;

  DELETE FROM ar_wholesale_update_proc;
END
$$

/**
 * Update the counting of involved wholesale telephone numbers,
 * only if there is the event, and then deregister the event.
 * NOTE: do not use a transaction because it must be called
 * by a session using LOCK WRITE on TABLES.
 */
DROP PROCEDURE IF EXISTS proc_update_wholesale_transaction $$
CREATE PROCEDURE proc_update_wholesale_transaction()
MODIFIES SQL DATA
BEGIN
  DECLARE count_data_to_update INTEGER;

  SELECT COUNT(*)
  FROM ar_wholesale_number_transaction_to_update
  INTO count_data_to_update;

  IF count_data_to_update > 0 THEN

    REPLACE INTO ar_wholesale_number_transaction(from_date, count_numbers, count_resellers, count_carriers, reseller_codes)
    (SELECT g.from_date AS d
    ,      COUNT(DISTINCT g.telephone_number) AS count1
    ,      COUNT(DISTINCT g.ar_reseller_id) AS count2
    ,      COUNT(DISTINCT g.ar_wholesale_carrier_id) AS count3
    ,      CONCAT('!!BEGIN!!,', GROUP_CONCAT(DISTINCT IFNULL(g.ar_reseller_id, '-1') SEPARATOR ','), ',!!END!!')
    FROM   ar_wholesale_number AS g
    INNER JOIN ar_wholesale_number_transaction_to_update AS u
    ON g.from_date = u.from_date
    GROUP  BY g.from_date);

    DELETE FROM ar_wholesale_number_transaction_to_update;

    CALL proc_update_wholesale_info();

  END IF;
END
$$

/**
 * Add the data only if it is new respect the past.
 */
DROP PROCEDURE IF EXISTS proc_insert_wholesale_number $$
CREATE PROCEDURE proc_insert_wholesale_number(
	`p_telephone_number` VARCHAR(255),
	`p_from_date` DATETIME,
	`p_exists` TINYINT,
	`p_extension_codes` VARCHAR(5024),
	`p_use_default_extension_codes` TINYINT,
	`p_ar_reseller_id` INTEGER,
	`p_ar_wholesale_carrier_id` INTEGER,
	`p_cost_price` BIGINT,
	`p_income_price` BIGINT)
MODIFIES SQL DATA
BEGIN
  DECLARE previous_date DATETIME;
  DECLARE is_old INTEGER;

  SELECT   from_date
  INTO     previous_date
  FROM     ar_wholesale_number
  WHERE    telephone_number = p_telephone_number
  ORDER BY from_date DESC
  LIMIT 1;

  SELECT COUNT(*)
  INTO is_old
  FROM ar_wholesale_number
  WHERE from_date = previous_date
  AND telephone_number = p_telephone_number
  AND `exists` = p_exists
  AND use_default_extension_codes = p_use_default_extension_codes
  AND (use_default_extension_codes = 1 OR extension_codes = p_extension_codes)
  AND ((ar_reseller_id IS NULL AND  p_ar_reseller_id IS NULL) OR
       (ar_reseller_id = p_ar_reseller_id))
  AND ((ar_wholesale_carrier_id IS NULL AND p_ar_wholesale_carrier_id IS NULL) OR
       (ar_wholesale_carrier_id = p_ar_wholesale_carrier_id))
  AND cost_price = p_cost_price
  AND income_price = p_income_price;

  IF previous_date IS NULL OR is_old = 0 THEN
     INSERT INTO ar_wholesale_number(
            telephone_number,
            from_date,
            `exists`,
             extension_codes,
             use_default_extension_codes,
             ar_reseller_id,
             ar_wholesale_carrier_id,
             cost_price,
             income_price,
             csv_comment,
             csv_last_date,
             csv_to_delete,
             csv_is_current)
     VALUES (
   	  `p_telephone_number`,
	  `p_from_date`,
      `p_exists`,
      `p_extension_codes`,
	  `p_use_default_extension_codes`,
	  `p_ar_reseller_id`,
	  `p_ar_wholesale_carrier_id`,
	  `p_cost_price`,
	  `p_income_price`,
	  NULL,NULL,0,0);
  ELSEIF previous_date IS NOT NULL AND is_old > 0 THEN
      UPDATE ar_wholesale_number
      SET csv_to_delete = 0, csv_comment = NULL, csv_last_date = NULL, csv_is_current = 0
      WHERE telephone_number = p_telephone_number
      AND   from_date = previous_date;
  END IF;
END
$$

/**
 * NOTE: force a recalc of the wholesale numbers, for being sure that the upgrade is safe.
 */
DROP PROCEDURE IF EXISTS proc_regenerate_update_wholesale_transaction $$
CREATE PROCEDURE         proc_regenerate_update_wholesale_transaction()
MODIFIES SQL DATA
BEGIN

  /* force all dates to update */
  REPLACE INTO ar_wholesale_number_transaction_to_update(from_date)
  (SELECT DISTINCT g.from_date AS d FROM ar_wholesale_number AS g);

  CALL proc_update_wholesale_transaction();
END
$$

CALL proc_regenerate_update_wholesale_transaction() $$

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
        , 'ar_number_portability'
        , 'ar_communication_channel_type'
        , 'ar_wholesale_carrier'

        // , 'ar_wholesale_number'
        //  NOTE: disabled because the trigger is managed by
        //  generateWholesaleToUpdateTrigger()
        //  (in used version of MySQL there can be only one trigger for table)
        );

        foreach ($tablesWithRerating as $tableName) {
            $sqlCode .= $this->generateTriggerForReratingEvent($tableName);
        }

        $sqlCode .= $this->generateWholesaleToUpdateTrigger("INSERT", false, true);
        $sqlCode .= $this->generateWholesaleToUpdateTrigger("DELETE", true, false);
        $sqlCode .= $this->generateWholesaleToUpdateTrigger("UPDATE", true, true);

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

  UPDATE ar_report_set SET postponed_fields_are_updated = 0 WHERE id = NEW.ar_report_set_id;
END
$$

DROP TRIGGER IF EXISTS fast_add_user_can_view_report_2 $$
CREATE TRIGGER fast_add_user_can_view_report_2
AFTER UPDATE ON ar_report
FOR EACH ROW
BEGIN
  DELETE FROM ar_user_can_view_report WHERE ar_report_id = NEW.id;

  UPDATE ar_report_set
  SET postponed_fields_are_updated = 0
  WHERE id = NEW.ar_report_set_id
  OR id = OLD.ar_report_set_id;

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

     protected function generateWholesaleToUpdateTrigger($triggerEvent, $useOld, $useNew) {

       $triggerName = "wholesale_to_update_" . $triggerEvent;

        $sql = "\nDROP TRIGGER IF EXISTS $triggerName " . '$$'
             . "\nCREATE TRIGGER $triggerName"
             . "\nAFTER $triggerEvent ON ar_wholesale_number"
             . "\nFOR EACH ROW BEGIN
             REPLACE INTO ar_wholesale_number_transaction_to_update VALUES ";

        $useComma = '';
        if ($useOld) {
             $sql .= "(OLD.from_date)";
             $useComma = ',';
        }

        if ($useNew) {
          $sql .= "$useComma (NEW.from_date)";
        }

        $sql .= ";\n";

        $sql .= "\nCALL schedule_rerating();\n";

        $sql .= "\nEND" . '$$';

        return $sql;
    }

    /**
     * TokuDB does not support foreign constraints `ON DELETE CASCADE`
     * so simulate them using an explicit TRIGGER.
     *
     * NOTE: it needs to group by sourceTable because MySQL does not support
     * multiple triggers on the same event.
     *
     * @param string $sourceTable
     * @param array $tablesToDelete list of strings with tables to delete on cascade
     * @param array|null $additionalActions additional actions to insert into the triggr
     * when an element of the $sourceTable is deleted
     * @return string
     */
    protected function generateDeleteOnCascade($sourceTable, $tablesToDelete, $additionalActions = null) {
        $triggerName = "trigger_cascade_on_" . $sourceTable;
        $sourceTableId = "id";
        $destFieldId = $sourceTable . '_id';

        $sql = "\nDROP TRIGGER IF EXISTS $triggerName " . '$$'
             . "\nCREATE TRIGGER $triggerName"
             . "\nBEFORE DELETE ON $sourceTable"
             . "\nFOR EACH ROW BEGIN ";

        if (!is_null($additionalActions)) {
            foreach($additionalActions as $act) {
                $sql .= "\n" . $act;
            }
            $sql .= "\n";
        }

        $sql2 = '';

        foreach($tablesToDelete as $tableToDelete) {
            $sql .= $this->generateDeleteOnCascade_service($sourceTable, $sourceTableId, $tableToDelete, $destFieldId);

            $sql2 .= $this->generateLegacyDeleteOfCorruptedData($sourceTable, $sourceTableId, $tableToDelete, $destFieldId);
        }

        $sql .= "\nEND " . '$$';

        return $sql . "\n" . $sql2;
    }

    /**
     * Generate internal action for the trigger.
     *
     * @param string $sourceTable
     * @param string $sourceFieldId
     * @param string $tableToDelete
     * @param string $destFieldId
     * @return string
     *
     */
    protected function generateDeleteOnCascade_service($sourceTable, $sourceFieldId, $tableToDelete, $destFieldId) {
        return "\n     DELETE FROM $tableToDelete WHERE $tableToDelete . $destFieldId = OLD .$sourceFieldId ; ";
    }

    /**
     * Delete old values simulating the activation of the trigger.
     *
     * @param $sourceTable
     * @param $sourceFieldId
     * @param $tableToDelete
     * @param $destFieldId
     * @return string
     */
    protected function generateLegacyDeleteOfCorruptedData($sourceTable, $sourceFieldId, $tableToDelete, $destFieldId) {
        // Delete old values, simulating the activation of the trigger.
        $sql = "\n DELETE $tableToDelete FROM $tableToDelete LEFT JOIN $sourceTable ON $sourceTable . $sourceFieldId = $tableToDelete . $destFieldId "
             . " WHERE $tableToDelete . $destFieldId IS NOT NULL AND $sourceTable . $sourceFieldId IS NULL" . '$$';

        return $sql;
    }
}
