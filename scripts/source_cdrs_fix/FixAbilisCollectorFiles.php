<?php

/*
 * Fix files of type "abilis-collector__v1"
 */

$isOk = main($argc, $argv);
if ($isOk) {
    exit(0);
} else {
    exit(1);
}

function help() {
    echo '
Usage:

  php FixAbilisCollectorFiles.php [INPUT_FILENAME] [fix1|fix2|...] [RESULT_FILENAME]

For executing one of the fixes specified in functions fix1 and so on.

For changing the executed code you must explicitly open this file, and customize the PHP code.

WARNING: every time you insert a STATUS FILE, you are deleting previous CDRS of DAY or MONTH,
so make sure to update only not active status.

';

}

/**
 * @param resource $handle
 * @param resource $outHandle
 * @param string $funToCallByLine
 * @return bool
 */
function processFile($handle, $outHandle, $funToCallByLine) {
    $row = 1;

    while (($s = fgetcsv($handle)) !== FALSE) {

        $i = 0;
        $r = array();
        $r['id'] = $s[$i++];
        $r['unique_check'] = $s[$i++];
        $r['collector'] = $s[$i++];
        $r['agent_host'] = $s[$i++];
        $r['insert_time'] = $s[$i++];
        $r['orig_port'] = $s[$i++];
        $r['in_called_num_type'] = $s[$i++];
        $r['in_called_num_plan'] = $s[$i++];
        $r['in_called_num'] = $s[$i++];
        $r['in_called_subaddr_type'] = $s[$i++];
        $r['in_called_subaddr_ind'] = $s[$i++];
        $r['in_called_subaddr'] = $s[$i++];
        $r['in_calling_num_type'] = $s[$i++];
        $r['in_calling_num_plan'] = $s[$i++];
        $r['in_calling_num_pres'] = $s[$i++];
        $r['in_calling_num_screen'] = $s[$i++];
        $r['in_calling_num'] = $s[$i++];
        $r['in_calling_subaddr_type'] = $s[$i++];
        $r['in_calling_subaddr_ind'] = $s[$i++];
        $r['in_calling_subaddr'] = $s[$i++];
        $r['orig_cluster_name'] = $s[$i++];
        $r['orig_side'] = $s[$i++];
        $r['orig_port_type'] = $s[$i++];
        $r['in_parent_callid'] = $s[$i++];
        $r['dest_port'] = $s[$i++];
        $r['out_called_num_type'] = $s[$i++];
        $r['out_called_num_plan'] = $s[$i++];
        $r['out_called_num'] = $s[$i++];
        $r['out_called_subaddr_type'] = $s[$i++];
        $r['out_called_subaddr_ind'] = $s[$i++];
        $r['out_called_subaddr'] = $s[$i++];
        $r['out_calling_num_type'] = $s[$i++];
        $r['out_calling_num_plan'] = $s[$i++];
        $r['out_calling_num_pres'] = $s[$i++];
        $r['out_calling_num_screen'] = $s[$i++];
        $r['out_calling_num'] = $s[$i++];
        $r['out_calling_subaddr_type'] = $s[$i++];
        $r['out_calling_subaddr_ind'] = $s[$i++];
        $r['out_calling_subaddr'] = $s[$i++];
        $r['dest_cluster_name'] = $s[$i++];
        $r['dest_side'] = $s[$i++];
        $r['dest_port_type'] = $s[$i++];
        $r['out_parent_callid'] = $s[$i++];
        $r['conn_type'] = $s[$i++];
        $r['disc_coding'] = $s[$i++];
        $r['disc_location'] = $s[$i++];
        $r['disc_recom'] = $s[$i++];
        $r['disc_cause'] = $s[$i++];
        $r['disc_cause_raw'] = $s[$i++];
        $r['disc_diagnostic'] = $s[$i++];
        $r['disc_conn_state'] = $s[$i++];
        $r['disc_direction'] = $s[$i++];
        $r['bearer_codec'] = $s[$i++];
        $r['bearer_bitrate'] = $s[$i++];
        $r['bearer_note'] = $s[$i++];
        $r['call_start'] = $s[$i++];
        $r['call_start_gmt'] = $s[$i++];
        $r['call_end'] = $s[$i++];
        $r['call_end_gmt'] = $s[$i++];
        $r['call_disc'] = $s[$i++];
        $r['call_disc_gmt'] = $s[$i++];
        $r['call_time'] = $s[$i++];
        $r['call_result'] = $s[$i++];
        $r['call_direction'] = $s[$i++];
        $r['callid'] = $s[$i++];
        $r['audio_law'] = $s[$i++];
        $r['audio_ss'] = $s[$i++];
        $r['audio_codec'] = $s[$i++];
        $r['audio_bitrate'] = $s[$i++];
        $r['audio_bandwidth'] = $s[$i++];
        $r['tc_audio_law'] = $s[$i++];
        $r['tc_audio_ss'] = $s[$i++];
        $r['tc_audio_codec'] = $s[$i++];
        $r['tc_audio_bitrate'] = $s[$i++];
        $r['tc_audio_bandwidth'] = $s[$i++];
        $r['fax_relay'] = $s[$i++];
        $r['fax_bypass'] = $s[$i++];
        $r['fax_codec'] = $s[$i++];
        $r['fax_bitrate'] = $s[$i++];
        $r['fax_bandwidth'] = $s[$i++];
        $r['tc_fax_relay'] = $s[$i++];
        $r['tc_fax_bypass'] = $s[$i++];
        $r['tc_fax_codec'] = $s[$i++];
        $r['tc_fax_bitrate'] = $s[$i++];
        $r['tc_fax_bandwidth'] = $s[$i++];
        $r['data_relay'] = $s[$i++];
        $r['data_bypass'] = $s[$i++];
        $r['data_codec'] = $s[$i++];
        $r['data_bitrate'] = $s[$i++];
        $r['data_bandwidth'] = $s[$i++];
        $r['tc_data_relay'] = $s[$i++];
        $r['tc_data_bypass'] = $s[$i++];
        $r['tc_data_codec'] = $s[$i++];
        $r['tc_data_bitrate'] = $s[$i++];
        $r['tc_data_bandwidth'] = $s[$i++];
        $r['reserved_bandwidth'] = $s[$i++];
        $r['tc_reserved_bandwidth'] = $s[$i++];
        $r['lost_records'] = $s[$i++];
        $r['ext_connid'] = $s[$i++];
        $r['ext_in_parent_callid'] = $s[$i++];
        $r['ext_out_parent_callid'] = $s[$i++];
        $r['tc_local_voice_underrun'] = $s[$i++];
        $r['tc_local_voice_overrun'] = $s[$i++];
        $r['tc_local_fax_underrun'] = $s[$i++];
        $r['tc_local_fax_overrun'] = $s[$i++];
        $r['tc_local_voice_def_jitter'] = $s[$i++];
        $r['tc_local_voice_max_jitter'] = $s[$i++];
        $r['tc_local_voice_top_jitter'] = $s[$i++];
        $r['tc_local_voice_avg_jitter'] = $s[$i++];
        $r['tc_local_fax_def_jitter'] = $s[$i++];
        $r['tc_local_fax_max_jitter'] = $s[$i++];
        $r['tc_local_fax_top_jitter'] = $s[$i++];
        $r['tc_local_fax_avg_jitter'] = $s[$i++];
        $r['tc_local_fax_tx_pages'] = $s[$i++];
        $r['local_voice_underrun'] = $s[$i++];
        $r['local_voice_overrun'] = $s[$i++];
        $r['local_fax_underrun'] = $s[$i++];
        $r['local_fax_overrun'] = $s[$i++];
        $r['local_voice_def_jitter'] = $s[$i++];
        $r['local_voice_max_jitter'] = $s[$i++];
        $r['local_voice_top_jitter'] = $s[$i++];
        $r['local_voice_avg_jitter'] = $s[$i++];
        $r['local_fax_def_jitter'] = $s[$i++];
        $r['local_fax_max_jitter'] = $s[$i++];
        $r['local_fax_top_jitter'] = $s[$i++];
        $r['local_fax_avg_jitter'] = $s[$i++];
        $r['local_fax_tx_pages'] = $s[$i++];
        $r['tc_remote_voice_underrun'] = $s[$i++];
        $r['tc_remote_voice_overrun'] = $s[$i++];
        $r['tc_remote_fax_underrun'] = $s[$i++];
        $r['tc_remote_fax_overrun'] = $s[$i++];
        $r['tc_remote_voice_def_jitter'] = $s[$i++];
        $r['tc_remote_voice_max_jitter'] = $s[$i++];
        $r['tc_remote_voice_top_jitter'] = $s[$i++];
        $r['tc_remote_voice_avg_jitter'] = $s[$i++];
        $r['tc_remote_fax_def_jitter'] = $s[$i++];
        $r['tc_remote_fax_max_jitter'] = $s[$i++];
        $r['tc_remote_fax_top_jitter'] = $s[$i++];
        $r['tc_remote_fax_avg_jitter'] = $s[$i++];
        $r['tc_remote_fax_tx_pages'] = $s[$i++];
        $r['remote_voice_underrun'] = $s[$i++];
        $r['remote_voice_overrun'] = $s[$i++];
        $r['remote_fax_underrun'] = $s[$i++];
        $r['remote_fax_overrun'] = $s[$i++];
        $r['remote_voice_def_jitter'] = $s[$i++];
        $r['remote_voice_max_jitter'] = $s[$i++];
        $r['remote_voice_top_jitter'] = $s[$i++];
        $r['remote_voice_avg_jitter'] = $s[$i++];
        $r['remote_fax_def_jitter'] = $s[$i++];
        $r['remote_fax_max_jitter'] = $s[$i++];
        $r['remote_fax_top_jitter'] = $s[$i++];
        $r['remote_fax_avg_jitter'] = $s[$i++];
        $r['remote_fax_tx_pages'] = $s[$i++];
        $r['user_in'] = $s[$i++];
        $r['user_out'] = $s[$i++];
        $r['red_num_in_type'] = $s[$i++];
        $r['red_num_in_plan'] = $s[$i++];
        $r['red_num_in_pres'] = $s[$i++];
        $r['red_num_in_screen'] = $s[$i++];
        $r['red_num_in'] = $s[$i++];
        $r['red_num_out_type'] = $s[$i++];
        $r['red_num_out_plan'] = $s[$i++];
        $r['red_num_out_pres'] = $s[$i++];
        $r['red_num_out_screen'] = $s[$i++];
        $r['red_num_out'] = $s[$i++];
        $r['is_converted_to_cdr'] = $s[$i++];
        $r['is_sitip'] = $s[$i++];

        $resultLine = call_user_func_array($funToCallByLine, array(&$r));
        if ($resultLine === FALSE) {
            return false;
        } else {
            fputcsv($outHandle, $r);
        }

        $row++;
    }
    fclose($handle);
    fclose($outHandle);

    return true;
}

/**
 * @param array $r an abilis line in the format specified by processFile function.
 * This value will be changed to the new result.
 * It will be set to null, in case the line must be not produced.
 * @return bool FALSE in case of error
 */
function fix1(&$r) {

    if ($r['id'] == '17165935' || $r['id'] == '17333750' || $r['id'] == '17334366') {
        $r['conn_type'] = 2;
        $r['user_in'] = 'arriamantova';
    } else if ($r['id'] == '17222624'
    || $r['id'] == '17223466'
    || $r['id'] == '17334366'
    ) {
        $r = null;
    }
}


/**
 * @param int $argc
 * @param string[] $argv
 * @return bool
 */
function main($argc, $argv)
{

    if ($argc != 4) {
        help();
        return false;
    }

    $fileName = $argv[1];
    $funToCall = $argv[2];
    $outFileName = $argv[3];

    $handle = fopen($fileName, "r");
    if ($handle === FALSE) {
        echo "\nMissing input file \"$fileName\"\n";
        return false;
    }

    $outHandle = fopen($outFileName, "w");
    if ($outHandle=== FALSE) {
        echo "\nProblems openining\"$outFileName\"\n";
        return false;
    }

    $isOk = false;
    if ($funToCall == 'fix1') {
        $isOk = processFile($handle, $outHandle, 'fix1');
    } else {
        echo "\nUnknown specified function \"$funToCall\".\n";
        $isOk = false;
    }

    if (!$isOk) {
        @fclose($outHandle);
        unlink($outFileName);
        echo "\nError processing \"$fileName\"";
        return false;
    } else {
        return true;
    }
}
