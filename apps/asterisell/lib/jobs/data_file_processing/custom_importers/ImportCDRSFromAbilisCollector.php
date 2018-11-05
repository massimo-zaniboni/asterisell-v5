<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Read CDRS from the Abilis collector of a remote database.
 * It works assuming that the id is an autoincrement field
 * only increasing, because it is used for recognizing new inserted calls.
 * It works without using transactions during read, because the ID is used, and this approach
 * is lighter on the remote database.
 * In any case there will be write-transactions during importing, and so no data will be lost.
 */
abstract class ImportCDRSFromAbilisCollector extends ImportCDRSFromDatabase
{

    public function getCollectorTableName() {
        return 'collector';
    }

    public function getLogicalType() {
        return 'abilis-collector';
    }

    public function getPhysicalType() {
        return 'v1';
    }

    public function getValueOfCDRToBeExported() {
        return null;
    }

    public function getExportedStatusBooleanField() {
        return null;
    }

    public function getProgressiveField() {
        return 'id';
    }

    public function getAdditionalQueryConditions() {
        return '';
    }

    public function getCallDateField() {
        return 'call_start';
    }

    public function isCallDateFieldATimestamp() {
        return false;
    }

    public function removeExportedCDRSOlderThanDays()
    {
        return 0;
    }

    public function getListOfFields() {
        static $r = null;

        if (is_null($r)) {
            $r = array(
              'id'
            , 'unique_check'
            , 'collector'
            , 'agent_host'
            , 'insert_time'
            , 'orig_port'
            , 'in_called_num_type'
            , 'in_called_num_plan'
            , 'in_called_num'
            , 'in_called_subaddr_type'
            , 'in_called_subaddr_ind'
            , 'in_called_subaddr'
            , 'in_calling_num_type'
            , 'in_calling_num_plan'
            , 'in_calling_num_pres'
            , 'in_calling_num_screen'
            , 'in_calling_num'
            , 'in_calling_subaddr_type'
            , 'in_calling_subaddr_ind'
            , 'in_calling_subaddr'
            , 'orig_cluster_name'
            , 'orig_side'
            , 'orig_port_type'
            , 'in_parent_callid'
            , 'dest_port'
            , 'out_called_num_type'
            , 'out_called_num_plan'
            , 'out_called_num'
            , 'out_called_subaddr_type'
            , 'out_called_subaddr_ind'
            , 'out_called_subaddr'
            , 'out_calling_num_type'
            , 'out_calling_num_plan'
            , 'out_calling_num_pres'
            , 'out_calling_num_screen'
            , 'out_calling_num'
            , 'out_calling_subaddr_type'
            , 'out_calling_subaddr_ind'
            , 'out_calling_subaddr'
            , 'dest_cluster_name'
            , 'dest_side'
            , 'dest_port_type'
            , 'out_parent_callid'
            , 'conn_type'
            , 'disc_coding'
            , 'disc_location'
            , 'disc_recom'
            , 'disc_cause'
            , 'disc_cause_raw'
            , 'disc_diagnostic'
            , 'disc_conn_state'
            , 'disc_direction'
            , 'bearer_codec'
            , 'bearer_bitrate'
            , 'bearer_note'
            , 'call_start'
            , 'call_start_gmt'
            , 'call_end'
            , 'call_end_gmt'
            , 'call_disc'
            , 'call_disc_gmt'
            , 'call_time'
            , 'call_result'
            , 'call_direction'
            , 'callid'
            , 'audio_law'
            , 'audio_ss'
            , 'audio_codec'
            , 'audio_bitrate'
            , 'audio_bandwidth'
            , 'tc_audio_law'
            , 'tc_audio_ss'
            , 'tc_audio_codec'
            , 'tc_audio_bitrate'
            , 'tc_audio_bandwidth'
            , 'fax_relay'
            , 'fax_bypass'
            , 'fax_codec'
            , 'fax_bitrate'
            , 'fax_bandwidth'
            , 'tc_fax_relay'
            , 'tc_fax_bypass'
            , 'tc_fax_codec'
            , 'tc_fax_bitrate'
            , 'tc_fax_bandwidth'
            , 'data_relay'
            , 'data_bypass'
            , 'data_codec'
            , 'data_bitrate'
            , 'data_bandwidth'
            , 'tc_data_relay'
            , 'tc_data_bypass'
            , 'tc_data_codec'
            , 'tc_data_bitrate'
            , 'tc_data_bandwidth'
            , 'reserved_bandwidth'
            , 'tc_reserved_bandwidth'
            , 'lost_records'
            , 'ext_connid'
            , 'ext_in_parent_callid'
            , 'ext_out_parent_callid'
            , 'tc_local_voice_underrun'
            , 'tc_local_voice_overrun'
            , 'tc_local_fax_underrun'
            , 'tc_local_fax_overrun'
            , 'tc_local_voice_def_jitter'
            , 'tc_local_voice_max_jitter'
            , 'tc_local_voice_top_jitter'
            , 'tc_local_voice_avg_jitter'
            , 'tc_local_fax_def_jitter'
            , 'tc_local_fax_max_jitter'
            , 'tc_local_fax_top_jitter'
            , 'tc_local_fax_avg_jitter'
            , 'tc_local_fax_tx_pages'
            , 'local_voice_underrun'
            , 'local_voice_overrun'
            , 'local_fax_underrun'
            , 'local_fax_overrun'
            , 'local_voice_def_jitter'
            , 'local_voice_max_jitter'
            , 'local_voice_top_jitter'
            , 'local_voice_avg_jitter'
            , 'local_fax_def_jitter'
            , 'local_fax_max_jitter'
            , 'local_fax_top_jitter'
            , 'local_fax_avg_jitter'
            , 'local_fax_tx_pages'
            , 'tc_remote_voice_underrun'
            , 'tc_remote_voice_overrun'
            , 'tc_remote_fax_underrun'
            , 'tc_remote_fax_overrun'
            , 'tc_remote_voice_def_jitter'
            , 'tc_remote_voice_max_jitter'
            , 'tc_remote_voice_top_jitter'
            , 'tc_remote_voice_avg_jitter'
            , 'tc_remote_fax_def_jitter'
            , 'tc_remote_fax_max_jitter'
            , 'tc_remote_fax_top_jitter'
            , 'tc_remote_fax_avg_jitter'
            , 'tc_remote_fax_tx_pages'
            , 'remote_voice_underrun'
            , 'remote_voice_overrun'
            , 'remote_fax_underrun'
            , 'remote_fax_overrun'
            , 'remote_voice_def_jitter'
            , 'remote_voice_max_jitter'
            , 'remote_voice_top_jitter'
            , 'remote_voice_avg_jitter'
            , 'remote_fax_def_jitter'
            , 'remote_fax_max_jitter'
            , 'remote_fax_top_jitter'
            , 'remote_fax_avg_jitter'
            , 'remote_fax_tx_pages'
            , 'user_in'
            , 'user_out'
            , 'red_num_in_type'
            , 'red_num_in_plan'
            , 'red_num_in_pres'
            , 'red_num_in_screen'
            , 'red_num_in'
            , 'red_num_out_type'
            , 'red_num_out_plan'
            , 'red_num_out_pres'
            , 'red_num_out_screen'
            , 'red_num_out'
            , 'id'
            , 'id'
            );
        }

        return $r;
    }

}
