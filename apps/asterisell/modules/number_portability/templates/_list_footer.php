<?php
use_helper('Markdown');
// Default culture is en_US
//
echo insertHelp('

## Number Portability

Called external telephone numbers are associated to a telephone vendor, according the telephone prefix. But there can be telephone numbers that were ported to different telephone operators, and the associated rates (expecially in case of mobile lines) can be different.

This table contains a mapping between telephone numbers and ported operators.

Usually this table should be maintained automatically from some job importing this info from some report sent from the VoIP vendor.

The best solution is that the VoIP vendor send directly in the CDR, the info about the real operator of the external telephone number. In this case the application must be customized for importing this info during CDR processing, bypassing this table.

## Multiple Porting

A telephone number can be ported to different operators, more than one time. For each porting, the application store the date of when the porting is valid. For each call, the applicable porting is selected, according the call date.

## Default Telephone Prefix

The source and destination telephone number must contains also the default telephone prefix (see "not_displayed_telephone_prefix" configuration parameter). This in order to have non ambigous telephone numbers.

## Manual Specification of Data

If the form is empty, it seems that there is only the CSV batch importing functionality. In reality the "create" button can be used for adding manually some info.

## CSV File Content

The CSV file has no header line.

The CSV file can contains also telephone numbers mapped to the same telephone numbers. In this case they are not managed as ported telephone number, or the telephone number is ported back to its original operator in case it were previously ported. In this way the CSV file can be used also in case the telephone operator gives a mapping between original number, and maybe ported telephone numbers, without filtering telephone numbers that are not ported.

The CSV file can contain repeated lines. They will not generate new entries in the table.

The CSV file can contain an entry for each call made. The history is updated only if the original telephone number is ported to a new telephone number, otherwise remains in the table only the older record, in order to use fewer records.

The insertion of records works using some heuristics for compacting informations, in case of repeated entries with different dates, for avoiding repetition. These heuristics works correctly if the records follows the order of their importing history, as usually is the case.

');

?>