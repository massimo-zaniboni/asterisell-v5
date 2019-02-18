/*  SPDX-License-Identifier: GPL-3.0-or-later */
/* Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com> */

#ifndef __DEBUG_INFO_ASTERISELL__
#define __DEBUG_INFO_ASTERISELL__

#include "call_flow_merger.h"

typedef struct {
    int id;
    char * name;
    char * description;
    char * yaml_specification;
    char * signature;
} CallFlowMergeRuleDescription;


/**
  * Store common stats for processed chains.
  */
typedef struct {

    /**
     * @brief initializated 1 if the title was initializated
     */
    int initializated;

    /**
     * The total number of errors of the same type.
     */
    int count;

    /**
     * A short title for the section.
     */
    GString * title;

    /**
     * A common description for errors of the same type.
     */
    GString * description;

    /**
     * A JudyTree used as a list of GString,
     * describing the errors of the same type/section,
     * to list in the debug file.
     */
    Pvoid_t chainWithError;

    /**
     * The name of the link to the part of the document with the examples.
     */
    int htmlBookmark;

} ITCChainStats;

extern CallFlowMergeRuleDescription * itc_rules;

/**
 * @require !cdr->is_error
 */
void update_itc_calls_direction_stats(CDRSet chain, CDR_direction direction);

void itc_update_good_rule_application_stats(CDRSet chain, int rule_id);

void update_itc_count_direction_of_calls_with_error_stats(CDR_direction direction);

void itc_signal_rule_conflicts(CDRSet chain, int ruleId1, int ruleId2);

void itc_signal_cdr_can_not_be_billsec(CDRSet chain, int ruleId);

void itc_signal_bad_pincode(CDRSet chain);

void itc_signal_chain_too_long(int cdrsInChain);

void itc_update_stats_maxDifftimeInSecondsInTheSameChain(double newDiff, time_t calldate);

void itc_signal_ruleDoNotUseCompleteChain(CDRSet cdrsInCompleteChain, int rule);

void itc_signal_ignored_call(CDRSet chain);

/**
 * Update stats about missing rule.
 *
 * Collect also the type of chain, according frequency. In this way user are informed about the
 * most important rules to complete, divided by type.
 *
 * The requirements of the final index are:
 * - indipendent from order of the chain
 * - distinction of different type of chains
 *
 * @param chain
 * @param chainLen
 * @param chainDirection
 */
void itc_signal_missing_rule(CDRSet chain, CDR_direction direction);

void itc_signal_missing_direction(CDRSet chain, CDR_direction missingDirection, int ruleId);

void currentDebugSourceCDR_associateToCurrentSourceCDR();

/**
 * A list (index from 0 to number of elements) of pointers to time_t,
 * of CDRs with an error during CSV parsing.
 * NOTE: the high-level error message is generated in the DEBUG phase,
 * and it is not stored in this data-structure.
 * NOTE: the CSV lines without a date, are managed from the CSV meta-info code.
 */
extern Pvoid_t itc_time_of_csv_line_with_error;

extern int itc_time_of_csv_line_with_error_index;


/**
 * @brief DebugSourceCDR_initModule
 * Init all the data structure for managing this type of info
 */
void DebugSourceCDR_initModule();

//////////////////////////
// STATS ABOUT PROBLEMS //
//////////////////////////

void ITCChain_show_debug_description(GString * html, CDRSet chain);

void itc_rule_show_debug_description(GString * html, int ruleId);

/**
 * Hash function for mapping a list of words to a unique Word.
 *
 * @param key
 * @param len
 * @return
 */
Word_t jenkins_one_at_a_time_hash(KeySet keySet);

/**
 * @return binary number ?1 if the error message must be inserted,
 * binary number ?0 if the error message must be not inserted.
 *
 * 1? if the title must be inserted,
 * 0? if the title must be not inserted.
 */
int update_stats_count(PPvoid_t judyTree, Word_t index, CDRSet chain);

/**
 *
 * @param judyTree
 * @param index
 * @param title it will be cloned
 * @param description it will be cloned
 *
 * @precondition index must be associated to a stat info
 */
void update_stats_set_title_and_description(PPvoid_t judyTree, Word_t index, GString * title, GString * description);

/**
 * @brief update_stats_add_debug_message
 * @param judyTree
 * @param index
 * @param message it is cloned because it is usually stack-allocated
 */
void update_stats_add_debug_message(PPvoid_t judyTree, Word_t index, GString * message);

//////////////////////
// SIGNAL FUNCTIONS //
//////////////////////
// They follow a common pattern:
// * fast count of calls with errors
// * only in case the DEBUG is active, a string (slow) with a description is generated

/**
 * @brief is_there_merge_on_link_key
 * @param key
 * @return 1 if there is a link between two CDRs on the specified key
 */
int itc_is_there_merge_on_link_key(Word_t key);

/**
 * Write all the info contained in a series of ITCChainStats
 * @param judyTree
 * @param doc
 */
void itc_stats_generate_section_without_title(Pvoid_t judyTree, int showExamples, GString * doc);

/**
 * Write all the info contained in a series of ITCChainStats
 * @param judyTree
 * @param doc
 */
void itc_stats_generate_section(Pvoid_t judyTree, char * title, int showExamples, GString * doc);

/**
 * Write all the info contained in a series of ITCChainStats,
 * having two level of index.
 *
 * @param judyTree
 * @param doc
 */
void itc_stats_generate_section_2(Pvoid_t judyTree, char * title, int showExamples, GString * doc);

/**
 * Write the stats to a HTML file.
 * @param fileName
 */
void itc_stats_generate_html_summary_file(char * fileName, char * clipsFileName);

int itc_initStats();

/**
 * Initialize the stats of a rule,
 * completing its description and starting with a 0 counter.
 *
 * @brief itc_update_rule_with_0_stats
 * @param rule_id
 */
void itc_update_rule_with_0_stats(int rule_id);

void itc_update_all_rules_with_0_stats();

int itc_initCallFlowMergeRuleStats();

extern int config_maxDebugInfoToDisplayForEachTypeOfError;

void add_itc_time_of_csv_line_with_error(time_t t);

/**
 * @brief add_itc_csv_line_with_error
 * @param ownedString a string, that is added to the messages. It must be owned (not deallocated).
 */
void add_itc_csv_line_with_error(GString *ownedString);

#endif


