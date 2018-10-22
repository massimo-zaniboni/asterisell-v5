/*  SPDX-License-Identifier: GPL-3.0-or-later */

#include "debug_info.h"
#include "import_pincodes.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <features.h>

/**
 * A user setting (passed on the command line) about the error to display for each
 * type of error.
 */
int config_maxDebugInfoToDisplayForEachTypeOfError;

/**
 * The content of this var, is specified from automatic rule generator.
 */
CallFlowMergeRuleDescription * itc_rules;

int stats_htmlBookmarkCounter;

ITCChainStats * ITCChainStats_new() {
    ITCChainStats * r = (ITCChainStats *) xmalloc(sizeof(ITCChainStats));

    r->count = 0;
    r->initializated = 0;
    r->chainWithError = NULL;
    r->title = NULL;
    r->description = NULL;
    r->htmlBookmark = stats_htmlBookmarkCounter++;

    return r;
}

/**
 * From ruleId1 to ruleId2 to stats.
 */
Pvoid_t itc_call_flow_merge_rule_count_conflicts;

/**
 * A JudyArray from ChainDirection, to a Word_t describing the type of chain, and the stats.
 */
Pvoid_t itc_chains_without_rule_stats;

/**
 * From ruleId to number ITCChainStats.
 */
Pvoid_t itc_count_applied_rules_stats;

/**
 * From call_direction to ITCChainStats.
 */
Pvoid_t count_calls_direction_stats;

/**
 * From CDR_direction to ITCChainStats.
 */
Pvoid_t itc_count_direction_of_calls_with_error_stats;

/**
 * From rule-id to ITCChainStats
 */
Pvoid_t itc_count_rules_with_unexpected_direction;

/**
 * From rule-id to ITCChainStats
 */
Pvoid_t itc_count_rules_with_unexpected_billsec;

/**
 * From 1 (fixed-index) to the chains of an ignored call.
 */
Pvoid_t itc_count_ignored_calls;

Pvoid_t itc_count_rules_not_using_complete_chain;

/**
 * A list (index from 0 to number of elements) of pointers to time_t,
 * of CDRs with an error during CSV parsing.
 * NOTE: the high-level error message is generated in the DEBUG phase,
 * and it is not stored in this data-structure.
 * NOTE: the CSV lines without a date, are managed from the CSV meta-info code.
 */
Pvoid_t itc_time_of_csv_line_with_error;

int itc_time_of_csv_line_with_error_index;

/**
 * A list of erorr messages about CSV lines with problems.
 */
Pvoid_t itc_csv_line_with_error;

int itc_csv_line_with_error_index;

int itc_chains_too_long;

int itc_cdr_in_chains_too_long;

/**
 * List of error messages about chains with bad pincode
 */
Pvoid_t itc_bad_pincodes;

double stats_maxDifftimeInSecondsInTheSameChain;

time_t stats_calldateWhereThereIsMaxDifftimeInSecondsInTheSameChain;

/**
 * @brief maybeEmpty
 * @param in
 * @return an html escaped GString
 */
GString * maybeEmpty(char * in) {
    if (in == NULL) {
        return g_string_new("");
    } else {
        return html_escape(in);
    }
}

/**
 * @brief maybeEmpty
 * @param in
 * @return an html escaped C string
 */
char * maybeEmptyStr(char * in) {
    return maybeEmpty(in)->str;
}

/**
 * @brief safeStr NULL or GString pointer
 * @return a not NULL C string
 */
char * safeStr(GString * str) {
    if (str == NULL || str->str == NULL) {
        return "";
    } else {
        return str->str;
    }
}

//////////////////////////////////
// FUNCTIONS FOR UPDATING STATS //
//////////////////////////////////

/**
 * @require !cdr->is_error
 */
void update_itc_calls_direction_stats(CDRSet chain, CDR_direction direction) {

    if (produce_debug_info) {
        int status = update_stats_count(&count_calls_direction_stats, (Word_t) direction, chain);

        if (status & 2) {
            GString * title = g_string_new("<h2>");
            g_string_append_printf(title, "Successfully Merged Calls with Direction %s</h2>", CDR_direction_description(direction));

            GString * description = g_string_new("<p></p>");

            update_stats_set_title_and_description(&count_calls_direction_stats, (Word_t) direction, title, description);
        }

        if (status & 1) {
            GString * html = g_string_new("<p>");

            ITCChain_show_debug_description(html, chain);
            g_string_append(html, "</p>");

            update_stats_add_debug_message(&count_calls_direction_stats, (Word_t) direction, html);
        }
    }
}

void itc_update_good_rule_application_stats(CDRSet chain, int rule_id) {

    if (produce_debug_info) {

        Word_t index = (Word_t) rule_id;
        int status = update_stats_count(&itc_count_applied_rules_stats, index, chain);

        if (status & 2) {

            GString * title = g_string_new("<h2>");
            g_string_append_printf(title, "Applied rule \"%s\"</h2>", itc_rules[rule_id].name);

            GString * description = g_string_new("<p>");
            itc_rule_show_debug_description(description, rule_id);
            g_string_append(description, "</p>");

            update_stats_set_title_and_description(&itc_count_applied_rules_stats, index, title, description);
        }

        if (status & 1) {
            GString * html = g_string_new("<p>");

            ITCChain_show_debug_description(html, chain);
            g_string_append(html, "</p>");

            update_stats_add_debug_message(&itc_count_applied_rules_stats, index, html);
        }
    }
}

void update_itc_count_direction_of_calls_with_error_stats(CDR_direction direction) {
    if (produce_debug_info) {
        Word_t index = direction;
        int status = update_stats_count(&itc_count_direction_of_calls_with_error_stats, index, NULL);

        if (status & 2) {
            GString * title = g_string_new("<h2>");
            g_string_append(title,   CDR_direction_description(direction));
            g_string_append(title, "</h2>");

            GString * description = g_string_new("<p></p>");

            update_stats_set_title_and_description(&itc_count_direction_of_calls_with_error_stats, index, title, description);
        }

        if (status & 1) {
            // do not show examples for these types of calls.
        }
    }
}


void itc_signal_rule_conflicts(CDRSet chain, int ruleId1, int ruleId2) {
    if (produce_debug_info) {
        Word_t index1 = ruleId1;
        Word_t index2 = ruleId2;
        Word_t index_temp;

        // use a uniform index schema
        if (ruleId2 < ruleId1) {
            index_temp = ruleId1;
            ruleId1 = ruleId2;
            ruleId2 = index_temp;
        }

        PPvoid_t pvalue1;
        JLI(pvalue1, itc_call_flow_merge_rule_count_conflicts, index1);

        int status = update_stats_count(pvalue1, index2, chain);

        if (status & 2) {

            GString * title = g_string_new("");
            g_string_append_printf(title, "<h2>Conflict between rule \"%s\", and rule \"%s\"</h2>", itc_rules[ruleId1].name, itc_rules[ruleId2].name);

            GString * description = g_string_new("<p>");
            itc_rule_show_debug_description(description, ruleId1);
            g_string_append_printf(description, "</p>");

            if (ruleId1 == ruleId2) {
                g_string_append_printf(description, "<br/>This rule is applied on the same chain, but using a different order of the element of the chain. Make the rule unanmbigous adding more conditions. ");
            } else {
                itc_rule_show_debug_description(description, ruleId2);
            }

            update_stats_set_title_and_description(pvalue1, index2, title, description);
        }

        if (status & 1) {
            GString * html = g_string_new("<p>");

            ITCChain_show_debug_description(html, chain);
            g_string_append(html, "</p>");

            update_stats_add_debug_message(pvalue1, index2, html);
        }
    }
}

void itc_signal_cdr_can_not_be_billsec(CDRSet chain, int ruleId) {
    if (produce_debug_info) {
        Word_t index = ruleId;

        int status = update_stats_count(&itc_count_rules_with_unexpected_billsec, index, chain);

        if (status & 2) {
            GString * title = g_string_new("<h2>");
            g_string_append_printf(title, "Not valid bill-sec CDR for rule \"%s\"</h2>", itc_rules[ruleId].name);

            GString * description = g_string_new("<p>");
            itc_rule_show_debug_description(description, ruleId);
            g_string_append_printf(description, "</p>");
            g_string_append_printf(description, "<br/>This rule specifies a CDR to use as billsec, that is not the CDR associated to the external part of the chain. Change the rule, specifying the correct CDR in the billsec section.");

            update_stats_set_title_and_description(&itc_count_rules_with_unexpected_billsec, index, title, description);
        }

        if (status & 1) {
            GString * html = g_string_new("<p>");

            ITCChain_show_debug_description(html, chain);
            g_string_append(html, "</p>");
            update_stats_add_debug_message(&itc_count_rules_with_unexpected_billsec, index, html);
        }
    }
}

void itc_signal_ruleDoNotUseCompleteChain(CDRSet chain, int ruleId) {
    if (produce_debug_info) {
        Word_t index = ruleId;

        int status = update_stats_count(&itc_count_rules_not_using_complete_chain, index, chain);

        if (status & 2) {
            GString * title = g_string_new("<h2>");
            g_string_append_printf(title, "The rule do not use all the CDRS in the strong chain, \"%s\"</h2>", itc_rules[ruleId].name);

            GString * description = g_string_new("<p>");
            itc_rule_show_debug_description(description, ruleId);
            g_string_append_printf(description, "</p>");
            g_string_append_printf(description, "<br/>This rule uses some CDR from a strong chain, but not all the CDRS of the strong-chain.");

            update_stats_set_title_and_description(&itc_count_rules_not_using_complete_chain, index, title, description);
        }

        if (status & 1) {
            GString * html = g_string_new("<p>");

            ITCChain_show_debug_description(html, chain);
            g_string_append(html, "</p>");
            update_stats_add_debug_message(&itc_count_rules_not_using_complete_chain, index, html);
        }
    }
}

void itc_signal_ignored_call(CDRSet chain) {
    Word_t index = 0;

    int status = update_stats_count(&itc_count_ignored_calls, index, chain);

    if (status & 2) {
        GString * title = g_string_new("<h1>Unprocessed Calls</h1>");

        GString * description = g_string_new("<p>There is an error in application code, and these chains, do not generate an error message, or a result. So these are lost calls. Fix the engine.</p>");

        update_stats_set_title_and_description(&itc_count_ignored_calls, index, title, description);
    }

    if (status & 1) {
        GString * html = g_string_new("<p>");
        ITCChain_show_debug_description(html, chain);
        g_string_append(html, "</p>");
        update_stats_add_debug_message(&itc_count_ignored_calls, index, html);
    }
}

void itc_signal_bad_pincode(CDRSet chain) {
    if (produce_debug_info) {
        Word_t index = 0;

        int status = update_stats_count(&itc_bad_pincodes, index, chain);

        if (status & 2) {
            GString * title = g_string_new("<h2>");
            g_string_append_printf(title, "Conflicting PINCODE</h2>");

            GString * description = g_string_new("<p>The chain contains two or more PINCODES that are associated to two or more distinct extensions, and the system do not know what to chose.</p>");
            update_stats_set_title_and_description(&itc_count_rules_with_unexpected_billsec, index, title, description);
        }

        if (status & 1) {
            GString * html = g_string_new("<p>");
            ITCChain_show_debug_description(html, chain);
            g_string_append(html, "</p>");
            update_stats_add_debug_message(&itc_bad_pincodes, index, html);
        }
    }
}

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
void itc_signal_missing_rule(CDRSet chain, CDR_direction direction) {

    if (produce_debug_info) {
        // Execute only if it is needed, because this is a costly operation,
        // so there is first this quick test...
        if (config_maxDebugInfoToDisplayForEachTypeOfError > 0) {

            //
            // Store a Word_t for every element of the chain,
            // containing the type of the CDR (IBPX, MGW...),
            // and the links active on them (unique-id, callid, ...)
            //

            KeySet pseudo_key = NULL;
            int status;

            Word_t scanKey = 0;
            J1F(status, chain, scanKey);
            while(status == 1) {
                ITCSourceCDR * cdr = (ITCSourceCDR *) scanKey;

                Word_t keyValue = 0;

                // The first 4 bit identify the type of the CDR

                keyValue = cdr->file_type;
                keyValue = keyValue << 4;

                // Use a BIT for each link field: 0 if it has some link, 1 if it has no link.

                int bitValue;

                bitValue = itc_is_there_merge_on_link_key(cdr->field_first_half_of_unique_id_link);
                keyValue = keyValue << 1;
                keyValue = keyValue | bitValue;

                bitValue = itc_is_there_merge_on_link_key(cdr->field_last_half_of_unique_id_link);
                keyValue = keyValue << 1;
                keyValue = keyValue | bitValue;

                bitValue = itc_is_there_merge_on_link_key(cdr->field_unique_id_link);
                keyValue = keyValue << 1;
                keyValue = keyValue | bitValue;

                bitValue = itc_is_there_merge_on_link_key(cdr->field_call_id_link);
                keyValue = keyValue << 1;
                keyValue = keyValue | bitValue;

                bitValue = itc_is_there_merge_on_link_key(cdr->field_call_id2_link);
                keyValue = keyValue << 1;
                keyValue = keyValue | bitValue;

                bitValue = itc_is_there_merge_on_link_key(cdr->field_unique_id_before_chiocciola_link);
                keyValue = keyValue << 1;
                keyValue = keyValue | bitValue;

                int bc;
                J1S(bc, pseudo_key, keyValue);

                J1N(status, chain, scanKey);
            }

            // NOTE: the keys in pseudo_key are ordered.
            // In this way the same type of call, with a different order in CDRs,
            // became the same.

            // Calculate a unique Word_t value, associated to the array, using an hash function.
            // So the probability to obtain the same value, for two different types, are very low.
            Word_t hashed_pseudo_key = jenkins_one_at_a_time_hash(pseudo_key);
            merge_freeKeySet(&pseudo_key);

            // Insert the stats in the array

            PPvoid_t statJudyTree;
            JLI(statJudyTree, itc_chains_without_rule_stats, direction);

            status = update_stats_count(statJudyTree, hashed_pseudo_key, chain);

            if (status & 2) {
                GString * title = g_string_new("<h2>");
                g_string_append_printf(title, "Missing rule for chain of type %s and link-id %lu</h2>",
                                       CDR_direction_description(direction),
                                       hashed_pseudo_key);

                GString * description = g_string_new("<p>");
                update_stats_set_title_and_description(statJudyTree, hashed_pseudo_key, title, description);
                g_string_append(description, "</p>");
            }

            if (status & 1) {
                GString * html = g_string_new("<p>");
                ITCChain_show_debug_description(html, chain);
                g_string_append(html, "</p>");
                update_stats_add_debug_message(statJudyTree, hashed_pseudo_key, html);
            }

        }
    }
}

void itc_signal_missing_direction(CDRSet chain, CDR_direction missingDirection, int ruleId) {


    if (produce_debug_info) {
        Word_t index = (Word_t) ruleId;
        int status = update_stats_count(&itc_count_rules_with_unexpected_direction, index, chain);

        const char * missingDirectionS = CDR_direction_description(missingDirection);

        if (status & 2) {
            GString * title = g_string_new("<h2>");
            g_string_append_printf(title, "Rule \"%s\" has not expected direction %s</h2>",
                                   itc_rules[ruleId].name, missingDirectionS);

            GString * description = g_string_new("<p>");
            g_string_append_printf(title, "The chain associated to this rule should have direction \"%s\", according heuristic analysis of its special CDRs. But the rule produces a Result CDR, without the expected direction.</p>",
                                   missingDirectionS);

            itc_rule_show_debug_description(description, ruleId);

            update_stats_set_title_and_description(&itc_count_rules_with_unexpected_direction, index, title, description);

        }

        if (status & 1) {
            GString * html = g_string_new("<p>");

            ITCChain_show_debug_description(html, chain);
            g_string_append(html, "</p>");

            update_stats_add_debug_message(&itc_count_rules_with_unexpected_direction, index, html);
        }
    }

}

void itc_signal_chain_too_long(int cdrsInChain) {
    itc_chains_too_long++;
    itc_cdr_in_chains_too_long += cdrsInChain;
}

void itc_update_stats_maxDifftimeInSecondsInTheSameChain(double newDiff, time_t minDate) {
    if (newDiff > stats_maxDifftimeInSecondsInTheSameChain) {
        stats_maxDifftimeInSecondsInTheSameChain = newDiff;
        stats_calldateWhereThereIsMaxDifftimeInSecondsInTheSameChain = minDate;

    }
}

////////////////
// DEBUG INFO //
////////////////

/**
 * Map a ITCSourceCDR with the corresponding DebugSourceCDR.
 */
Pvoid_t map_ITCSourceCDRToDebugInfo;

DebugSourceCDR * ITCSourceCDR_getDebugInfo(ITCSourceCDR * s) {
    PWord_t PValue;
    JLG(PValue, map_ITCSourceCDRToDebugInfo, (Word_t) s);
    if (PValue == NULL) {
        return NULL;
    } else {
        return (DebugSourceCDR *) (* PValue);
    }
}

void currentDebugSourceCDR_associateToCurrentSourceCDR() {
    PWord_t PValue;
    JLI(PValue, map_ITCSourceCDRToDebugInfo, (Word_t) current_itcSourceCDR);
    *PValue = (Word_t ) currentDebugSourceCDR;
}

CSVFileHeader csvFileHeader_itc_mgw;

CSVFileHeader csvFileHeader_itc_sbc;

/**
 * @brief DebugSourceCDR_initModule
 * Init all the data structure for managing this type of info
 */
void DebugSourceCDR_initModule() {
    map_ITCSourceCDRToDebugInfo = NULL;

    //
    // CREATE DEFAULT CSV FILE HEADERS
    //
    // MGW and IPBX

    char * itc_mgw[] = {
        "accountcode",
        "src",
        "dst",
        "dcontext",
        "clid",
        "srcchannel",
        "dstchannel",
        "lastapp",
        "lastdata",
        "start",
        "answer",
        "end",
        "duration",
        "billsec",
        "disposition",
        "amaflags",
        "userfield",
        "uniqueid",
        NULL
    };

    csvFileHeader_itc_mgw = NULL;
    Word_t pos = 0;
    PWord_t pvalue;

    while(1) {
        char * s = itc_mgw[pos];
        if (s == NULL) {
            break;
        } else {
            JLI(pvalue, csvFileHeader_itc_mgw, pos);
            * pvalue = (Word_t) s;
        }
        pos++;

    }

    char * itc_sbc[] = {
        "SessionID",
        "Recorded",
        "CallID",
        "To",
        "From",
        "Method",
        "IncomingRequestURI",
        "PreviousHopIp",
        "PreviousHopVia",
        "OutgoingRequestURI",
        "NextHopIp",
        "NextHopDn",
        "Header",
        "Origin",
        "SetupTime",
        "ConnectTime",
        "DisconnectTime",
        "DisconnectCause",
        "Duration",
        "scpName",
        "CallID2",
        "OrigGW",
        "TermGW",
        "PacketsReceivedOnSrcLeg",
        "PacketsLostOnSrcLeg",
        "PacketsDiscardedOnSrcLeg",
        "PdvOnSrcLeg",
        "MaxJitterOnSrcLeg",
        "CodecOnSrcLeg",
        "MimeTypeOnSrcLeg",
        "LatencyOnSrcLeg",
        "MaxLatencyOnSrcLeg",
        "RFactorOnSrcLeg",
        "PacketsReceivedOnDestLeg",
        "PacketsLostOnDestLeg",
        "PacketsDiscardedOnDestLeg",
        "PdvOnDestLeg",
        "MaxJitterOnDestLeg",
        "CodecOnDestLeg",
        "MimeTypeOnDestLeg",
        "LatencyOnDestLeg",
        "MaxLatencyOnDestLeg",
        "RFactorOnDestLeg",
        "Rx1000FactorOnDestLeg",
        "Rx1000FactorOnSrcLeg",
        "MOSFmtOnDestLeg",
        "MOSFmtOnSrcLeg",
        "CallType",
        "DisconnectErrorType",
        "Ani",
        "CallSourceRegid",
        "CallDestRegid",
        "NewAni",
        "CdrType",
        "HuntingAttempts",
        "CallPDD",
        "CallSourceRealmName",
        "CallDestRealmName",
        "CallDestCRName",
        "n_peer_dst",
        "in_anchor_src",
        "in_anchor_dst",
        "in_peer_src",
        "out_peer_dst",
        "out_anchor_src",
        "out_anchor_dst",
        "out_peer_src",
        "CalledPartyAfterSrcCallingPlan",
        "LastStatusMessage",
        "LastMediaPktTimestampOnDestLeg",
        "LastMediaPktTimestampOnSrcLeg",
        "SetupTimeInt",
        "IncomingURIStripped",
        "Dnis",
        "NewDnis",
        "CustomData",
        "CreationTimestamp",
        NULL
    };

    csvFileHeader_itc_sbc = NULL;
    pos = 0;

    while(1) {
        char * s = itc_sbc[pos];
        if (s == NULL) {
            break;
        } else {
            JLI(pvalue, csvFileHeader_itc_sbc, pos);
            * pvalue = (Word_t) s;
        }
        pos++;

    }
}


/**
 * @brief ITCSourceCDR_show_debug_description
 * @param html add to this string the debug info
 * @param cdrName
 * @param cdr
 */
void ITCSourceCDR_show_debug_description(GString * html, char * cdrName, ITCSourceCDR * cdr) {

    const int buffSize = 1024;
    char buff[buffSize];
    int i;

    g_string_append_printf(html,
                           "<p>%s",
                           cdrName
                          );

    // Display near the name if it is special CDR, creating a temporary single chain.
    CDRSet singleChain = NULL;
    merge_addInCDRSet(&singleChain, cdr);
    ChainDirection cdrDirection = merge_classifyChainDirectionUsingSpecialCDRs(singleChain);
    merge_freeCDRSet(&singleChain);

    if (! cdrDirection.internal) {
        g_string_append_printf(html,
                               " (%s)",
                               ChainDirection_description(cdrDirection)
                              );

    }

    strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(& cdr->field_calldate));

    char * pinCodeStr = "";
    PinCodeInfo * pinCodeInfo = pinCodesInfo_get(cdr->field_accountcode, cdr->field_calldate);
    if (pinCodeInfo != NULL) {
        GString * n = g_string_new("(pincode associated to extension \"");
        g_string_append(n, pinCodeInfo->extension);
        g_string_append(n, "\")");
        pinCodeStr = n->str;
    }

    g_string_append_printf(html,
                           "<ul><li>type: %s</li><li>calldate: %s</li><li>billsec: %d</li><li>src: %s</li><li>dst: %s</li><li>accountcode: %s %s</li><li>lastdata: %s</li><li>src_channel_get_extension: %s</li><li>dst_channel_get_extension: %s</li><li>destregid_vendorId: %s</li><li>sourceregid_vendorId: %s</li>",
                           maybeEmptyStr(itcFileTypeName(cdr->file_type)),
                           buff,
                           cdr->field_billsec,
                           maybeEmptyStr(cdr->field_src),
                           maybeEmptyStr(cdr->field_dst),
                           maybeEmptyStr(cdr->field_accountcode), pinCodeStr,
                           maybeEmptyStr(cdr->field_lastdata),
                           maybeEmptyStr(cdr->field_src_channel_get_extension),
                           maybeEmptyStr(cdr->field_dst_channel_get_extension),
                           maybeEmptyStr(itcVendor_getVendorString(cdr->destregid_vendorId)),
                           maybeEmptyStr(itcVendor_getVendorString(cdr->sourceregid_vendorId))
                          );

    for(i = 0; i < 7; i++) {
        Word_t index = 0;
        char * indexName;

        switch(i) {
        case 0:
            indexName = "field_userfield_until_point_weak_link";
            index = cdr->field_userfield_until_point_weak_link;
            break;
        case 1:
            indexName = "field_unique_id_link";
            index = cdr->field_unique_id_link;
            break;
        case 2:
            indexName = "field_call_id_link";
            index = cdr->field_call_id_link;
            break;
        case 3:
            indexName = "field_call_id2_link";
            index = cdr->field_call_id2_link;
            break;
        case 4:
            indexName = "field_unique_id_before_chiocciola_link";
            index = cdr->field_unique_id_before_chiocciola_link;
            break;
        case 5:
            indexName = "field_last_half_of_unique_id_link";
            index = cdr->field_last_half_of_unique_id_link;
            break;
        case 6:
            indexName = "field_first_half_of_unique_id_link";
            index = cdr->field_first_half_of_unique_id_link;
            break;
        default:
            indexName = "!!unknown!!";
            index = 0;
            break;
        }

        g_string_append_printf(html,"<li>%s: %lu</li>",indexName, index);
    }

    //
    // Show special fields
    //

    g_string_append_printf(html,"<li>special fields: ");

    i = 1;
    int exitWhile = 0;
    while(! exitWhile) {
        char * indexName;
        int isSet;

        switch(i) {
        case 1:
            indexName = "isAnswered";
            isSet = cdr->status.isAnswered;
            break;
        case 2:
            indexName = "can_be_billsec";
            isSet = cdr->status.can_be_billsec;
            break;

        case 3:
            indexName = "calldestregid_isExternalVoIPVendor";
            isSet = cdr->status.calldestregid_isExternalVoIPVendor;
            break;
        case 4:
            indexName = "callsrcregid_isExternalVoIPVendor";
            isSet = cdr->status.callsrcregid_isExternalVoIPVendor;
            break;
        case 5:
            indexName = "lastData_isBackupChannel";
            isSet = cdr->status.lastData_isBackupChannel;
            break;
        case 6:
            indexName = "dstChannel_isEnumChannel";
            isSet = cdr->status.dstChannel_isEnumChannel;
            break;
        case 7:
            indexName = "dstChannel_isDAHDI";
            isSet = cdr->status.dstChannel_isDAHDI;
            break;
        case 8:
            indexName = "srcChannel_isSIP";
            isSet = cdr->status.srcChannel_isSIP;
            break;
        case 9:
            indexName = "dstChannel_isSIP";
            isSet = cdr->status.dstChannel_isSIP;
            break;
        case 10:
            indexName = "dstChannel_isSIPAndSBC";
            isSet = cdr->status.dstChannel_isSIPAndSBC;
            break;
        case 11:
            indexName = "dcontext_isInternal";
            isSet = cdr->status.dcontext_isInternal;
            break;
        case 12:
            indexName = "dcontext_isFaxTpa";
            isSet = cdr->status.dcontext_isFaxTpa;
            break;
        case 13:
            indexName = "dcontext_isFromTrunk";
            isSet = cdr->status.dcontext_isFromTrunk;
            break;
        case 14:
            indexName = "dcontext_isFax";
            isSet = cdr->status.dcontext_isFax;
            break;
        case 15:
            indexName = "dcontext_isExtLocal";
            isSet = cdr->status.dcontext_isExtLocal;
            break;
        case 16:
            indexName = "dcontext_isExtGroup";
            isSet = cdr->status.dcontext_isExtGroup;
            break;
        case 17:
            indexName = "dcontext_isToPBX";
            isSet = cdr->status.dcontext_isToPBX;
            break;
        case 18:
            indexName = "dcontext_isFromDirectDDI";
            isSet = cdr->status.dcontext_isFromDirectDDI;
            break;
        case 19:
            indexName = "dcontext_isFaxBackup";
            isSet = cdr->status.dcontext_isFaxBackup;
            break;
        case 20:
            indexName = "lastdata_isExtensionAndDomain";
            isSet = cdr->status.lastdata_isExtensionAndDomain;
            break;
        case 21:
            indexName = "lastdata_isInternalChannel";
            isSet = cdr->status.lastdata_isInternalChannel;
            break;
        case 22:
            indexName = "lastdata_isBackupChannel";
            isSet = cdr->status.lastdata_isBackupChannel;
            break;
        case 23:
            indexName = "lastdata_isSIP";
            isSet = cdr->status.lastdata_isSIP;
            break;
        case 24:
            indexName = "srcChannel_isDAHDI";
            isSet = cdr->status.srcChannel_isDAHDI;
            break;
        case 25:
            indexName = "lastapp_isResetCDR";
            isSet = cdr->status.lastapp_isResetCDR;
            break;
        case 26:
            indexName = "lastapp_isDial";
            isSet = cdr->status.lastapp_isDial;
            break;
        case 27:
            indexName = "dstChannel_isLocal";
            isSet = cdr->status.dstChannel_isLocal;
            break;
        case 28:
            indexName = "calldestregid_isIPBX";
            isSet = cdr->status.calldestregid_isIPBX;
            break;
        case 29:
            indexName = "dstChannel_isSIPWithExtension";
            isSet = cdr->status.dstChannel_isSIPWithExtension;
            break;
        case 30:
            indexName = "srcChannel_isSIPWithExtension";
            isSet = cdr->status.srcChannel_isSIPWithExtension;
            break;
        case 31:
            indexName = "dstChannel_isInstitutionDomain";
            isSet = cdr->status.dstChannel_isInstitutionDomain;
            break;
        case 32:
            indexName = "srcChannel_isInstitutionDomain";
            isSet = cdr->status.srcChannel_isInstitutionDomain;
            break;
        case 33:
            indexName = "dst_isFax";
            isSet = cdr->status.dst_isFax;
            break;
        case 34:
            indexName = "callsrcregid_isMGW";
            isSet = cdr->status.callsrcregid_isMGW;
            break;
        case 35:
            indexName = "calldstregid_isMG";
            isSet = cdr->status.calldstregid_isMG;
            break;
        case 36:
            indexName = "dstchannel_isEmpty";
            isSet = cdr->status.dstchannel_isEmpty;
            break;
        case 37:
            indexName = "callsrcregid_isNull";
            isSet = cdr->status.callsrcregid_isNull;
            break;
        case 38:
            indexName = "calldstregid_isNull";
            isSet = cdr->status.calldstregid_isNull;
            break;
        case 39:
            indexName = "dcontext_isFaxToBackup";
            isSet = cdr->status.dcontext_isFaxToBackup;
            break;

        default:
            exitWhile = 1;
        }

        if (!exitWhile) {
            if (isSet) {
                g_string_append_printf(html,"%s, ",indexName);
            }
        }

        i++;
    }

    g_string_append_printf(html,"</li>");

    DebugSourceCDR * debugCDR = ITCSourceCDR_getDebugInfo(cdr);
    if (debugCDR != NULL) {

        g_string_append_printf(html,"<li>source file, and line number: %s:%i</li>", maybeEmptyStr(debugCDR->fileName), debugCDR->lineNr);


        //
        // Show all CSV fields in a compact way
        //
        g_string_append_printf(html,"<li>cdr: ");

        Word_t index = 0;
        PWord_t pvalue;
        JLF(pvalue, debugCDR->values, index);
        while(pvalue != NULL) {

            // retrieve header
            PWord_t headerValue;
            JLG(headerValue, * debugCDR->header, index);
            if (headerValue == NULL) {
                g_string_append_printf(html,"<b>field %u</b>: ", (unsigned int) index);
            } else {
                g_string_append_printf(html,"<b>%s</b>: ", maybeEmptyStr((char *) *headerValue));
            }
            g_string_append_printf(html, "%s, ", maybeEmptyStr((char *) *pvalue));

            JLN(pvalue, debugCDR->values, index);
        }
        g_string_append_printf(html, "</li>");

    } else {
        g_string_append(html, "<li>there is no CSV line debug info associated.</li>");
    }
    g_string_append_printf(html, "</ul></p>");

}

void ITCChain_show_debug_description(GString * html, CDRSet chain) {
    int i = 0;
    int isThereCDR = 0;

    Word_t scanKey = 0;
    J1F(isThereCDR, chain, scanKey);
    while(isThereCDR) {
        ITCSourceCDR * cdr = (ITCSourceCDR *) scanKey;

        int buffSize = 255;
        char buff[buffSize];
        i++;

        snprintf(buff, buffSize, "Call Detail Record %i", i);
        ITCSourceCDR_show_debug_description(html, buff, cdr);

        J1N(isThereCDR, chain, scanKey);
    }
}

void itc_rule_show_debug_description(GString * html, int ruleId) {
    g_string_append_printf(html, "%s", itc_rules[ruleId].yaml_specification);
}

/**
 * Hash function for mapping a list of words to a unique Word.
 *
 * @param key
 * @param len
 * @return
 */
Word_t jenkins_one_at_a_time_hash(KeySet keySet)
{
    Word_t key = 0;
    int status;

    Word_t hash = 0;
    J1F(status, keySet, key);
    while(status) {
        hash += key;
        hash += (hash << 10);
        hash ^= (hash >> 6);

        J1N(status, keySet, key);
    }

    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    return hash;
}

/**
 * Errors and debug info are classified in different judyTree.
 * For each JudyTree there is a common pattern:
 * - classify the type of message
 * - show a fixed numbef or examples CHAINS respecting the example
 * This function perform this common operation, for each JudyTree passed as parameter.
 *
 * @return binary number ?1 if the error message must be inserted,
 * binary number ?0 if the error message must be not inserted.
 * 1? if the title must be inserted, (snd bit enabled)
 * 0? if the title must be not inserted. (fst bit enabled)
 */
int update_stats_count(PPvoid_t judyTree, Word_t index, CDRSet chain) {
    PPvoid_t pvalue;
    JLI(pvalue, *judyTree, index);

    if ((*pvalue) == NULL) {
        (*pvalue) = ITCChainStats_new();
    }

    ITCChainStats * stats = (ITCChainStats *) (* pvalue);

    stats->count++;

    int flag = 0;

    if (! stats->initializated) {
        flag = flag | 2;
    }

    if (config_maxDebugInfoToDisplayForEachTypeOfError > 0) {

        Pvoid_t debugTree = stats->chainWithError;
        Word_t debugCount;
        JLC(debugCount, debugTree, 0, -1);

        if (debugCount < (Word_t) config_maxDebugInfoToDisplayForEachTypeOfError) {
            // Insert if there are not too much CDRs
            flag = flag | 1;
        } else {
            // Insert also the CDRs with a forcing debug status
            if (itcSourceCDRs_isForceDebug(chain)) {
                flag = flag | 1;
            }
        }
    }

    return flag;
}

/**
 *
 * @param judyTree
 * @param index
 * @param title it will be cloned
 * @param description it will be cloned
 * @require update_stats_count is called on the same index, and so the value is initializated.
 */
void update_stats_set_title_and_description(PPvoid_t judyTree, Word_t index, GString * title, GString * description) {
    PPvoid_t pvalue;

    JLG(pvalue, *judyTree, index);
    assert(pvalue != NULL);

    ITCChainStats * stats = (ITCChainStats *) (* pvalue);

    stats->title = g_string_new(title->str);
    stats->description = g_string_new(description->str);
    stats->initializated = 1;
}

/**
 * Add a new debug message associated to an index, of a JudyTree.
 * The indexed value will be a list of debug messages.
 *
 * @brief update_stats_add_debug_message
 * @param judyTree
 * @param index
 * @param message it is cloned because it is usually stack-allocated
 */
void update_stats_add_debug_message(PPvoid_t judyTree, Word_t index, GString * message) {

    PWord_t pvalue;
    JLG(pvalue, * judyTree, index);
    assert(pvalue != NULL);

    ITCChainStats * stats = (ITCChainStats *) (* pvalue);

    Word_t newIndex;
    JLC(newIndex, stats->chainWithError, 0, -1);

    newIndex++;

    PWord_t plist;
    JLI(plist, stats->chainWithError, newIndex);
    (*plist) = (Word_t) message;
}

//////////////////////
// SIGNAL FUNCTIONS //
//////////////////////
// They follow a common pattern:
// * fast count of calls with errors
// * only in case the DEBUG is active, a string (slow) with a description is generated


/**
 * Write all the info contained in a series of ITCChainStats
 * @param judyTree
 * @param doc
 */
void itc_stats_generate_section_without_title(Pvoid_t judyTree, int showExamples, GString * doc) {

    Word_t index;
    PWord_t pvalue;

    index = 0;
    JLF(pvalue, judyTree, index);
    while (pvalue != NULL)
    {
        ITCChainStats * stats = (ITCChainStats *) (* pvalue);

        g_string_append_printf(doc,
                               "%s\n<p>There are %d cases</p><p>%s</p>",
                               safeStr(stats->title),
                               stats->count,
                               safeStr(stats->description)
                              );

        //
        // Add the html links
        //

        if (showExamples) {
            // use an anchor before writing examples
            g_string_append_printf(doc,
                                   "<a id=\"link_%i\"></a>",
                                   stats->htmlBookmark
                                  );

        } else {
            // insert a link to examples
            g_string_append_printf(doc,
                                   "<a href=\"#link_%i\">Go to details.</a>",
                                   stats->htmlBookmark
                                  );

        }


        //
        // Write examples
        //

        if (showExamples) {
            Word_t exampleIndex = 0;
            PWord_t exampleValue;
            Pvoid_t exampleTree = stats->chainWithError;
            JLF(exampleValue, exampleTree, exampleIndex);
            int i = 0;
            while(exampleValue != NULL) {
                GString * exampleElement = (GString *) (* exampleValue);

                i++;
                g_string_append_printf(doc,
                                       "<h3>Example %i</h3>\n%s",
                                       i,
                                       safeStr(exampleElement)
                                      );


                JLN(exampleValue, exampleTree, exampleIndex);
            }
        }

        JLN(pvalue, judyTree, index);
    }
}


/**
 * Write all the info contained in a series of ITCChainStats
 * @param judyTree
 * @param doc
 */
void itc_stats_generate_section(Pvoid_t judyTree, char * title, int showExamples, GString * doc) {

    g_string_append_printf(doc,
                           "<h1>%s</h1>\n",
                           title
                          );
    itc_stats_generate_section_without_title(judyTree, showExamples, doc);
}

/**
 * Write all the info contained in a series of ITCChainStats,
 * having two level of index.
 *
 * @param judyTree
 * @param doc
 */
void itc_stats_generate_section_2(Pvoid_t judyTree, char * title, int showExamples, GString * doc) {

    Word_t index;
    PWord_t pvalue;

    g_string_append_printf(doc,
                           "<h1>%s</h1>\n",
                           title
                          );

    index = 0;
    JLF(pvalue, judyTree, index);
    while (pvalue != NULL)
    {
        itc_stats_generate_section_without_title((Pvoid_t) (*pvalue), showExamples, doc);

        JLN(pvalue, judyTree, index);
    }
}

/**
 * Write the stats to a HTML file.
 * @param fileName
 */
void itc_stats_generate_html_summary_file(char * fileName, char * clips_file_name) {

    int buffSize = 1024;
    char buff[buffSize];

    FILE *f = fopen(fileName, "w+");
    if (f == NULL)
    {
        printf("Error opening file!\n");
        exit(1);
    }

    fprintf(f, "<!DOCTYPE html>\n<title>Debugging of Call Flow Merge</title>\n");

    //
    // Show Header
    //

    strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(&(currentRateParams.fromDate)));

    fprintf(f,
            "<h1>Params</h1>\n<p>Merge calls from %s",
            buff
           );

    if (currentRateParams.thereIsToDate) {
        strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(&(currentRateParams.toDate)));
        fprintf(f,
                " to %s ",
                buff
               );

    } else {
        fprintf(f," to most recent calls.");

    }

    fprintf(f, "</p>");

    strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(& currentRateParams.derived_acceptAfterDate));
    fprintf(f,
            "</p><p> Use a merge window of %i seconds from %s \n",
            currentRateParams.mergeWindowInSeconds,
            buff
           );

    if (currentRateParams.thereIsToDate) {
        strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(& (currentRateParams.derived_acceptBeforeDate)));
        fprintf(f,
                " to %s ",
                buff
               );

    } else {
        fprintf(f, " to most recent calls.");
    }


    strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(&(stats_calldateWhereThereIsMaxDifftimeInSecondsInTheSameChain)));
    fprintf(f, " The longest detected difference in seconds between CDRS on the same call-chain was %f seconds (%f minutes, %f hours), on a chain with calldate %s.",
            stats_maxDifftimeInSecondsInTheSameChain,
            (stats_maxDifftimeInSecondsInTheSameChain / 60.0),
            (stats_maxDifftimeInSecondsInTheSameChain / 60.0 / 60),
            buff
           );

    fprintf(f, "<p>The file %s contains the CLIPS debug for all calls with calldate %s</p>", clips_file_name, buff);

    strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(&(currentRateParams.fromDate)));
    fprintf(f,
            "</p>\n<p>For each type of call insert the first %i example of calls, and all the calls having the date %s in any of their fields.</p>",
            config_maxDebugInfoToDisplayForEachTypeOfError,
            buff
           );


    fprintf(f, "<p>NOTE: in case of complete chains containing strongly linked chains that are logically different calls, some of these errors can not be complete, and missing some hints on some call in the chain. The error stats are complete (the count of missed calls), but the reason about a not merged chain can not be complete. So keep fixing errors, in order to see all the hided errors.</p>");
    fprintf(f, "<p>NOTE: The stats about missing calls are not reliable in counting the backup calls. So there can be more missing outgoing calls respect reported calls.</p>");

    //
    // Show Rules
    //

    int showExamples;


    GString * doc = g_string_sized_new(1024*5);

    // show first a summary of info (showExample == 0), then the info with some examples of CDRs (showExample == 1)
    for(showExamples = 0; showExamples < 2; showExamples++) {
        g_string_truncate(doc, 0);
        itc_stats_generate_section(count_calls_direction_stats, "Succesfully Merged Calls by Direction", showExamples, doc);
        fputs(doc->str, f);

        g_string_truncate(doc, 0);
        itc_stats_generate_section(itc_count_ignored_calls, "Unprocessed Calls", showExamples, doc);
        fputs(doc->str, f);

        g_string_truncate(doc, 0);
        itc_stats_generate_section(itc_count_direction_of_calls_with_error_stats, "Unmerged Calls by Direction", showExamples,  doc);
        fputs(doc->str, f);

        if (itc_chains_too_long > 0 && showExamples == 0) {
            fprintf(f, "\n<h1>Long Chain Calls</h1>\n");
            fprintf(f, "\n<p>Due to a misconfiguration of VoIP servers, there can be very very long call chains. They are discarded, and not counted in the error stats. They are only displayed in this debug message. The true direction of these calls is not known.</p><p>There are %i call flow chain, that are too much long. There are %i CDRs in these chains.</p>\n",
                    itc_chains_too_long, itc_cdr_in_chains_too_long);
        }

        if (itc_csv_line_with_error_index > 0 && showExamples == 0) {
            fprintf(f, "\n<h1>There are CSV lines with parsing errors</h1>\n");
            fprintf(f, "\n<p>Each line is reported to Asterisell as a CDR with an error on outgoing direction. So the stats of errors are updated on the Asterisell side.</p><p>Each CDR with a parsing error, is a potential missing element from a chain, and so some of the errors reported in this document, can be affected from the missing parsed CDR, and they can be not accurate.\n");
            fprintf(f, "\n<p>There are %i errors of this type.</p>", itc_csv_line_with_error_index);
        }

        g_string_truncate(doc, 0);
        itc_stats_generate_section(itc_bad_pincodes, "Calls with bad PINCODE", showExamples,  doc);
        fputs(doc->str, f);

        g_string_truncate(doc, 0);
        itc_stats_generate_section(itc_count_applied_rules_stats, "Successfully Applied Rules", showExamples,  doc);
        fputs(doc->str, f);

        g_string_truncate(doc, 0);
        itc_stats_generate_section_2(itc_call_flow_merge_rule_count_conflicts, "Conflicting Rules", showExamples,  doc);
        fputs(doc->str, f);

        g_string_truncate(doc, 0);
        itc_stats_generate_section_2(itc_chains_without_rule_stats, "Chains with Missing Rules", showExamples,  doc);
        fputs(doc->str, f);

        g_string_truncate(doc, 0);
        itc_stats_generate_section(itc_count_rules_with_unexpected_billsec, "Chains with Not Correct Billsec CDR", showExamples, doc);
        fputs(doc->str, f);

        g_string_truncate(doc, 0);
        itc_stats_generate_section(itc_count_rules_with_unexpected_direction, "Chains with Unexpected Direction", showExamples,  doc);
        fputs(doc->str, f);

        g_string_truncate(doc, 0);
        itc_stats_generate_section(itc_count_rules_not_using_complete_chain, "Rules not using complete chain", showExamples,  doc);
        fputs(doc->str, f);

    }


    if (itc_csv_line_with_error_index > 0) {
        fprintf(f, "\n<h1>Some CSV lines with parsing errors</h1>\n");
        fprintf(f, "\n<p>Each line is reported to Asterisell as a CDR with an error on outgoing direction. So the stats of errors are updated on the Asterisell side.</p><p>Each CDR with a parsing error, is a potential missing element from a chain, and so some of the errors reported in this document, can be affected from the missing parsed CDR, and they can be not accurate.\n");

        Word_t index = 0;
        PWord_t pvalue;

        JLF(pvalue, itc_csv_line_with_error, index);
        while (pvalue != NULL)
        {
            char * msg = (char *) (*pvalue);
            fprintf(f, "\n<li>%s</li>", msg);
            JLN(pvalue, itc_csv_line_with_error, index);
        }
    }

    fclose(f);
}

/**
 * @brief html_escape escape according HTML rules the content of the text
 * @param text the text to convert
 * @return the converted GString
 */
GString * html_escape(char * text) {
    GString * str = g_string_sized_new(strlen(text) * 1.5);

    unsigned i = 0;
    char c;
    while((c = text[i++])) {
        switch(c) {
        case '"':
            g_string_append(str, "&quot;");
            break;
        case '<':
            g_string_append(str, "&lt;");
            break;
        case '>':
            g_string_append(str, "&gt;");
            break;
        case '&':
            g_string_append(str, "&amp;");
            break;
        default: {
            g_string_append_c(str, c);
            break;
        }
        }
    }

    return str;
}

/**
 * @brief add_itc_time_of_csv_line_with_error
 * @param t
 */
void add_itc_time_of_csv_line_with_error(time_t t) {
    time_t * t2 = (time_t *) xmalloc(sizeof(time_t));
    (* t2) = t;

    itc_time_of_csv_line_with_error_index++;
    Word_t index = (Word_t) itc_time_of_csv_line_with_error_index;
    PPvoid_t pvalue;
    JLI(pvalue, itc_time_of_csv_line_with_error, index);

    (*pvalue) = t2;
}

/**
 * @brief add_itc_csv_line_with_error
 * @param ownedString a string, that is added to the messages. It must be owned (not deallocated).
 */
void add_itc_csv_line_with_error(GString *ownedString) {
    itc_csv_line_with_error_index++;
    Word_t index = (Word_t) itc_csv_line_with_error_index;
    PPvoid_t pvalue;
    JLI(pvalue, itc_csv_line_with_error, index);

    (*pvalue) = ownedString->str;
}

int itc_initStats() {
    count_calls_direction_stats = NULL;
    return 1;
}

/**
 * Initialize the stats of a rule,
 * completing its description and starting with a 0 counter.
 *
 * @brief itc_update_rule_with_0_stats
 * @param rule_id
 */
void itc_update_rule_with_0_stats(int rule_id) {
    Word_t index = (Word_t) rule_id;

    PPvoid_t pvalue;
    JLI(pvalue, itc_count_applied_rules_stats, index);

    if ((*pvalue) == NULL) {
        (*pvalue) = ITCChainStats_new();
    }

    GString * title = g_string_new("<h2>");
    g_string_append_printf(title, "Applied rule \"%s\"</h2>",
                           itc_rules[rule_id].name);

    GString * description = g_string_new("<p>");
    itc_rule_show_debug_description(description, rule_id);
    g_string_append(description, "</p>");

    update_stats_set_title_and_description(&itc_count_applied_rules_stats, index, title, description);
}

void itc_update_all_rules_with_0_stats() {
    int i = 0;
    while(1) {
        i++;

        if(itc_rules[i].id == 0) {
            return;
        } else {
            itc_update_rule_with_0_stats(i);
        }
    }
}

int itc_initCallFlowMergeRuleStats() {

    itc_call_flow_merge_rule_count_conflicts = NULL;

    itc_chains_without_rule_stats = NULL;

    itc_count_applied_rules_stats = NULL;

    count_calls_direction_stats = NULL;

    itc_count_direction_of_calls_with_error_stats = NULL;

    itc_count_rules_with_unexpected_direction = NULL;

    itc_count_rules_with_unexpected_billsec = NULL;

    itc_count_ignored_calls = NULL;

    itc_time_of_csv_line_with_error_index = 0;
    itc_time_of_csv_line_with_error = NULL;

    itc_csv_line_with_error = NULL;
    itc_csv_line_with_error_index = 0;

    itc_bad_pincodes = NULL;

    itc_count_rules_not_using_complete_chain = NULL;

    itc_chains_too_long = 0;
    itc_cdr_in_chains_too_long = 0;

    stats_maxDifftimeInSecondsInTheSameChain = 0.0;

    stats_htmlBookmarkCounter = 0;

    return 1;

}


