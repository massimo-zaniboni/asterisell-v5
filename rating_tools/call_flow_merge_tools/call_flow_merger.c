/*  SPDX-License-Identifier: GPL-3.0-or-later */
/* Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com> */

/*
 * Parse and rate CSV files for ITCenter FCCN Project.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <malloc.h>
#include <clips.h>

#include "call_flow_merger.h"
#include "import_pincodes.h"
#include "debug_info.h"

// NOTE: this file is generated from the rule compiler.
#include "call_flow_merge_procedures.c.include"

// Set to 1 for enabling debug mode tests, during processing of calls
#define ENABLE_DEBUG_MODE 0

int initial_debug_messages;

////////////
// PARAMS //
////////////

void init_params() {
}

//////////////////////////
// HANDLE MEMORY ERRORS //
//////////////////////////

#define JUDYERROR(CallerFile, CallerLine, JudyFunc, JudyErrno, JudyErrID) \
       {                                                                         \
               (void) fprintf(stderr, "File ’%s’, line %d: %s(), "               \
                   "JU_ERRNO_* == %d, ID == %d\n",                               \
                   CallerFile, CallerLine,                                       \
                   JudyFunc, JudyErrno, JudyErrID);                              \
               exit(2);                                                          \
       }

// NOTE: it is important to define the previous macro before this include
#include <Judy.h>

void * xmalloc (size_t n)
{
    void *p = malloc (n);
    if (!p && n != 0) {
        fprintf(stderr, "\nOut of memory.");
        exit(2);

    }
    return p;
}

//////////////////
// COMMON TYPES //
//////////////////

const char * CDR_direction_description(CDR_direction d) {
    switch(d) {
    case CDR_UNPROCESSED:
        return "unprocessed";
    case CDR_INCOMING:
        return "incoming";
    case CDR_OUTGOING:
        return "outgoing";
    case CDR_INTERNAL:
        return "internal";
    case CDR_IGNORED:
        return "ignored";
    case CDR_ERROR:
        return "error";
    default:
        return "unknown-case";
    }
}


//////////////////////////
// PARSER STATE RELATED //
//////////////////////////

#define CSV_LINE_MAX_SIZE (64*1024)

/**
 * The maximum size of temporary strings containig info like channels,
 * keys and so on..
 */
#define PARSER_STRING_SIZE (1024 * 4)

/**
 * @brief dict_PJArray Associate a string key to an internal code,
 * for faster lookup.
 */
Pvoid_t dict_PJArray;

Word_t   dict_next_unique_index;

char parser_string[PARSER_STRING_SIZE];
int parser_string_pos;

int found_main_voip_domain_during_parsing_of_telephone_number;
int found_main_voip_domain_during_parsing_of_telephone_number_bookmark;

int produce_debug_info;

inline void startKeyIndexGeneration() {
    parser_string_pos = 0;
}

extern inline void startKeyIndexGeneration();

inline void addCharInKeyIndexGeneration(char ch) {
    parser_string[parser_string_pos++] = ch;
    if (parser_string_pos >= PARSER_STRING_SIZE) {
        fprintf(stderr, "\nParsed string is too much long");
        exit(2);
    }
}

extern inline void addCharInKeyIndexGeneration(char ch);

void addStringInKeyIndexGeneration(char * str) {
    int i = 0;
    while (str[i] != 0) {
        addCharInKeyIndexGeneration(str[i]);
        i++;
    }
}

extern void addStringInKeyIndexGeneration(char * str);

char parser_string2[PARSER_STRING_SIZE];
int parser_string_pos2;

inline void startKeyIndexGeneration2() {
    parser_string_pos2 = 0;
}

extern inline void startKeyIndexGeneration2();

inline void addCharInKeyIndexGeneration2(char ch) {
    parser_string2[parser_string_pos2++] = ch;
    if (parser_string_pos2 >= PARSER_STRING_SIZE) {
        fprintf(stderr, "\nParsed string is too much long");
        exit(2);
    }
}

extern inline void addCharInKeyIndexGeneration2(char ch);

extern inline Word_t merge_countIndexedSet(IndexedCDRS ind);

extern inline Word_t merge_countCDRSet(CDRSet set);

extern inline Word_t merge_countKeySet(KeySet set);

void addStringInKeyIndexGeneration2(char * str) {
    int i = 0;
    while (str[i] != 0) {
        addCharInKeyIndexGeneration2(str[i]);
        i++;
    }
}

inline int stopKeyIndexGeneration2() {

    // terminate the string (without advancing the pointer)
    parser_string2[parser_string_pos2] = '\0';

    return parser_string_pos2;
}

extern inline int stopKeyIndexGeneration2();

inline int stopKeyIndexGeneration() {

    // terminate the string (without advancing the pointer)
    parser_string[parser_string_pos] = '\0';

    return parser_string_pos;
}

extern inline int stopKeyIndexGeneration();

struct tm current_parsed_internal_time;

inline void initCurrentParsedInternalTime() {
    current_parsed_internal_time.tm_sec = 0;
    current_parsed_internal_time.tm_min = 0;
    current_parsed_internal_time.tm_hour = 0;
    current_parsed_internal_time.tm_mday = 0;
    current_parsed_internal_time.tm_mon = 0;
    current_parsed_internal_time.tm_year = 0;
}

extern inline void initCurrentParsedInternalTime();

RateParams currentRateParams;

///////////////////////////
// DICTIONARY OF STRINGS //
///////////////////////////
// NOTE: I can store strings in non-compressed format, because
// the dimension of data is compatible with the free RAM on ITC servers.
// I'm using a lot of these strings only for ITC project, during call-flow merge rules.
//
// NOTE: I'm using this "complex" code, because initially we meant to use
// compressed strings.

/**
 * Add the current key in the dictionary and return the corresponding unique index.
 * Register the string in the dictionary with lookup option.
 * Return 0 in case of empty string.
 */
inline StringKey addStringInDoubleDictionary(char * str) {
    return strdup(str);
}

extern inline StringKey addStringInDoubleDictionary(char * str);

/**
 * Add the current key in the dictionary and return the corresponding unique index.
 * Register the string in the dictionary with lookup option.
 */
inline StringKey addCurrentStringInDoubleDictionary() {

    stopKeyIndexGeneration();

    return addStringInDoubleDictionary(parser_string);
}

extern inline StringKey addCurrentStringInDoubleDictionary();

/**
 * Add the current key in the dictionary and return the corresponding unique index.
 * Register the string in the dictionary with lookup option.
 */
inline StringKey addCurrentString2InDoubleDictionary() {

    stopKeyIndexGeneration2();

    return addStringInDoubleDictionary(parser_string2);
}

extern inline StringKey addCurrentString2InDoubleDictionary();

/**
 * @brief getStringFromDoubleDictionary
 * @param key
 * @return
 */
inline char * getStringFromDoubleDictionary(StringKey str) {
    return str;
}

extern inline char * getStringFromDoubleDictionary(StringKey str);

////////////
// PARAMS //
////////////

/**
 * The maximum possible len of a chain of CDRs.
 * Chains of more lenght are considered invalid (there is no rule supporting them)
 * and they are discarded from the system.
 */
#define MAX_CHAIN_LEN 254

/**
 * The voip domain of the main institution, of the processed LOGS.
 */
char * itc_main_institution_domain;

char * itcFileTypeName(ITCFileType t) {
    if (t == UNSPECIFIED_FILE) {
        return "unspecified";
    } else if (t == SBC_FILE) {
        return "SBC";
    } else if (t == MGW_FILE) {
        return "MGW";
    } else if (t == IPBX_FILE) {
        return "IPBX";
    }  else {
        fprintf(stderr, "\nUnknwon type %i", t);
        exit(1);
    }
}

void RateParams_completeDerivedFields(RateParams * p) {
    p->derived_acceptAfterDate = p->fromDate - p->mergeWindowInSeconds;
    if (p->thereIsToDate) {
        p->derived_acceptBeforeDate = p->toDate + p->mergeWindowInSeconds;
    }
}

/**
 *
 * @param d
 * @return a human readable description of a chain direction.
 */
char * ChainDirection_description(ChainDirection d) {
    GString * str = g_string_new("");

    int flag = 0;

    if (d.not_answered) {
        g_string_append(str, "not_answered ");
        flag++;
    }

    if (d.outgoing) {
        g_string_append(str, "outgoing ");
        flag++;
    }
    if (d.incoming)  {
        g_string_append(str, "incoming ");
        flag++;
    }
    if (d.internal)  {
        g_string_append(str, "internal ");
        flag++;
    }
    if (d.ignored)  {
        g_string_append(str, "ignored ");
        flag++;
    }
    if (d.error)  {
        g_string_append(str, "error ");
        flag++;
    }
    if (d.pstn_backup)  {
        g_string_append(str, "pstn_backup ");
        flag++;
    }
    if (d.enum_call)  {
        g_string_append(str, "enum_call ");
        flag++;
    }

    if (flag == 0) {
        g_string_append(str, "unknown code");
    }

    return str->str;
}


void ChainDirection_printf(ChainDirection d) {
    printf("%s", ChainDirection_description((d)));
}

CDR_direction chainDirection_toDestinationType(ChainDirection d) {

    if (d.outgoing) {
        return CDR_OUTGOING;
    } else if (d.incoming) {
        return CDR_INCOMING;
    } else if (d.internal) {
        return CDR_INTERNAL;
    } else if (d.ignored) {
        return CDR_IGNORED;
    } else {
        return CDR_ERROR;
    }

}

int debug_count_cdrs;

void itc_init_rate() {

    currentRateParams.thereIsToDate = 0;
    currentRateParams.fromDate = 0;
    currentRateParams.toDate = 0;
    currentRateParams.mergeWindowInSeconds = 0;

    debug_count_cdrs = 0;
}

///////////////////////////////
// ITC SOURCE CDR MANAGEMENT //
///////////////////////////////

ITCVendor parsed_vendorId;

char * itcVendor_getVendorString(ITCVendor v) {
    switch(v) {
    case ITC_VENDOR_UNDEF:
        return "undef";
        break;
    case ITC_VENDOR_PTPRIME:
        return "PTPrime";
        break;
    case ITC_VENDOR_TMN:
        return "TMN";
        break;
    case ITC_VENDOR_VODAFONE:
        return "Vodafone";
        break;
    case ITC_VENDOR_SONAECOM:
        return "Sonaecom";
        break;
    case ITC_VENDOR_GLOBAL_PHONE:
        return "GlobalPhone_VPN_TMN";
        break;
    default:
        fprintf(stderr, "\nUnknown vendor %i", v);
        exit(1);
        return "error";
    }
}

/**
 * The initial (empty) ITCSourceCDR.
 */
ITCSourceCDR ITCSourceCDR_empty;

/**
 * @brief itcSourceCDRs_isForceDebug
 * @param chain
 * @return 1 if the debug must be forced on this chain.
 */
int itcSourceCDRs_isForceDebug(CDRSet chain) {
    int status;
    Word_t scanKey = 0;
    J1F(status, chain, scanKey);
    while(status) {
        ITCSourceCDR * cdr = (ITCSourceCDR *) scanKey;
        if (cdr->status.force_debug) {
            return 1;
        }
        J1N(status, chain, scanKey);
    }

    return 0;
}

/////////////
// PARSING //
/////////////


/**
 *
 * @brief db_itcSourceCDRs key code -> ITCSourceCDR
 * for fast lookup of chained ITCSourceCDRs.
 */
IndexedCDRS db_itcSourceCDRs;

/**
 * @brief db_itSourceCDRs_withoutLinks set of itcSourceCDR
 * without any strong or weak link. They are isolated/single CDRs.
 */
CDRSet db_itSourceCDRs_withoutLinks;

/**
 * @brief current_itcSourceCDR current CDR under parsing.
 */
ITCSourceCDR * current_itcSourceCDR;

inline void newCurrentITCSourceCDR() {

    current_itcSourceCDR = (ITCSourceCDR *) xmalloc(sizeof(ITCSourceCDR));

    *current_itcSourceCDR = ITCSourceCDR_empty;
}

extern inline void newCurrentITCSourceCDR();

/**
 * @brief ITCSourceCDR_noLinks
 * @return 1 if the CDR has no links.
 */
int ITCSourceCDR_noLinks(ITCSourceCDR * cdr) {
    Word_t t = 0;

    t += cdr->field_userfield_until_point_weak_link;
    t += cdr->field_unique_id_link;
    t += cdr->field_call_id_link;
    t += cdr->field_call_id2_link;
    t += cdr->field_unique_id_before_chiocciola_link;
    t += cdr->field_last_half_of_unique_id_link;
    t += cdr->field_first_half_of_unique_id_link;

    return (t == 0);
}

/**
 * Store the CDR in a special db only if it has no links,
 * otherwise it must be already registered for each existing link,
 * and no other action is required.
 * @brief registerCurrentITCSourceCDR
 */
inline void registerCurrentITCSourceCDR() {
    if (ITCSourceCDR_noLinks(current_itcSourceCDR)) {
        int Rc_int;
        J1S(Rc_int, db_itSourceCDRs_withoutLinks, (Word_t) current_itcSourceCDR);
    }
}

extern inline void registerCurrentITCSourceCDR();

inline int dateIsInsideMergeWindow(int d) {
    if (currentRateParams.derived_acceptAfterDate <= d
            && ( (!currentRateParams.thereIsToDate) ||
                 currentRateParams.derived_acceptBeforeDate > d)) {
        return 1;
    } else {
        return 0;
    }
}

extern inline int dateIsInsideMergeWindow(int d);

inline void registerCurrentITCSourceCDRForFastLookup(Word_t keyCode) {

    PPvoid_t PValue1;

    JLI(PValue1, db_itcSourceCDRs, keyCode);
    int Rc_int;
    J1S(Rc_int, *PValue1, (Word_t) current_itcSourceCDR);
}

extern inline void registerCurrentITCSourceCDRForFastLookup(Word_t keyCode);

extern inline void registerCDRLink(ITCSourceCDR * cdr, KeyIndex key);

void registerCDR(ITCSourceCDR * cdr) {
    registerCDRLink(cdr, cdr->field_first_half_of_unique_id_link);
    registerCDRLink(cdr, cdr->field_last_half_of_unique_id_link);
    registerCDRLink(cdr, cdr->field_unique_id_link);
    registerCDRLink(cdr, cdr->field_call_id_link);
    registerCDRLink(cdr, cdr->field_call_id2_link);
    registerCDRLink(cdr, cdr->field_unique_id_before_chiocciola_link);
    registerCDRLink(cdr, cdr->field_userfield_until_point_weak_link);
}

/**
 * Add the specified string in the dictionary and return the corresponding unique index.
 * Register also the current_itcSourceCDR with the key.
 * So the passed str must be an index key value.
 * @param str the key to use
 * @return Word_t the key corresponding to str
 */
inline Word_t addStringInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup(char * str) {

    if (str[0] == '\0') {
        return 0;
    }

    // Use a Word_t code for each unique string, because it is faster to compare.
    PWord_t PValue;
    JSLI(PValue, dict_PJArray, (uint8_t *) str);

    if (*PValue == 0) {
        // a new index was inserted, and I must initialize with the 1 value
        *PValue = dict_next_unique_index;
        dict_next_unique_index++;
    } else {
        // nothing to do: the key was already in the dictionary,
        // and PValue contains the corresponding integer code.
    }

    registerCurrentITCSourceCDRForFastLookup(*PValue);

    return *PValue;
}

extern inline Word_t addStringInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup(char * str);

/**
 * Add the current key in the dictionary and return the corresponding unique index.
 * Register also the current_itcSourceCDR with the key.
 */
inline Word_t addIndexKeyInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup() {

    if (parser_string_pos == 0) {
        return 0;
    }

    stopKeyIndexGeneration();

    return addStringInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup(parser_string);
}

extern inline Word_t addIndexKeyInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup();

extern inline void merge_addKeySet(KeySet source, KeySet *dest);

extern inline void merge_freeKeySet(KeySet * set);

extern inline void merge_freeCDRSet(CDRSet * set);

extern inline void merge_freeIndexedCDRS(KeySet * set);

extern inline int merge_isEmptyKeySet(KeySet set);

extern inline int merge_existsInKeySet(KeySet set, KeyIndex key);

extern inline int merge_existsInCDRSet(CDRSet set, ITCSourceCDR * cdr);

extern inline CDRSet merge_getCDRSet(IndexedCDRS set, KeyIndex key);

extern inline int merge_addInCDRSet(CDRSet * set, ITCSourceCDR * cdr);

extern inline int merge_addInKeySet(KeySet * set, KeyIndex key);

extern inline int merge_addInIndexedCDRS(IndexedCDRS *indexedSet, KeyIndex key, ITCSourceCDR * cdr);

extern inline void ChainDirection_reset(ChainDirection * d);

extern inline int ChainDirection_isThereRedirect(ChainDirection d);

//////////////////////////////
// MANAGEMENT OF DEBUG INFO //
//////////////////////////////

DebugSourceCDR * currentDebugSourceCDR;

Word_t currentDebugSourceCDR_column;

void currentDebugSourceCDR_new(char * fileName, int lineNr, CSVFileHeader * header) {
    currentDebugSourceCDR = (DebugSourceCDR * ) xmalloc(sizeof(DebugSourceCDR));

    currentDebugSourceCDR->header = header;
    currentDebugSourceCDR->values = NULL;

    currentDebugSourceCDR_column = 0;

    currentDebugSourceCDR->fileName = fileName;
    currentDebugSourceCDR->lineNr = lineNr;
}

void currentDebugSourceCDR_addRowStringKey(StringKey s) {
    PWord_t PValue;
    JLI(PValue, currentDebugSourceCDR->values, currentDebugSourceCDR_column);

    *PValue = (Word_t) s;

    currentDebugSourceCDR_column++;
}

void currentDebugSourceCDR_addRowValueUsingCurrentString2() {
    StringKey s = addCurrentString2InDoubleDictionary();
    currentDebugSourceCDR_addRowStringKey(s);

}

void currentDebugSourceCDR_addRowValue(char *v) {
    StringKey s = addStringInDoubleDictionary(v);
    currentDebugSourceCDR_addRowStringKey(s);
}

//////////////////////////
// DOMAIN LOGIC SUPPORT //
//////////////////////////

/**
 * In UniqueId field, there can be info like:
 * - "123 456": "123" is the first half, "456" is the last half
 * - "123 ": "123" is the first half
 * - " 123": "123" is the last half
 * - "123": it is the unique-id
 * - "123@abc": "123" is the unique-id before-chiocciola
 */
void registerCurrentITCSouceCDRWithFirstHalfAndLastHalfUniqueId() {
    int part = 1;
    int i = -1;
    int continueParsing = 1;
    int spacePosition = 0;

    stopKeyIndexGeneration();

    while(continueParsing) {
        i++;
        char ch = parser_string[i];

        //
        // Set the actions to do
        //

        int storeFirstHalf = 0;
        int storeLastHalf = 0;
        int storeUniqueId = 0;
        int storeBeforeChiocciola = 0;
        int offset = 0;
        int deleteBeforeChiocciola = 0;

        if (ch == ' ' && part == 1) {
            part = 2;
            spacePosition = i;
            if (i == 0) {
                // skip initial empty first part
            } else {
                storeFirstHalf = 1;
            }
        } else if (ch == '\0') {
            continueParsing = 0;
            if (part == 1) {
                storeUniqueId = 1;
            } else if (part == 2) {
                if (i > spacePosition + 1) {
                    deleteBeforeChiocciola = 1;
                    storeLastHalf = 1;
                    offset = spacePosition + 1;
                }
            }
        } else if (ch == '@' && part == 1) {
            storeBeforeChiocciola = 1;
        }

        //
        // Execute the actions
        //

        if (storeFirstHalf || storeLastHalf || storeUniqueId || storeBeforeChiocciola || deleteBeforeChiocciola) {
            // terminate the string before processing
            char backupChar = ch;
            parser_string[i] = '\0';

            Word_t key = addStringInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup(parser_string + offset);

            if (key != 0) {

                if (storeFirstHalf) {
                    current_itcSourceCDR->field_first_half_of_unique_id_link = key;
                }

                if (storeLastHalf) {
                    current_itcSourceCDR->field_last_half_of_unique_id_link = key;
                }

                if (storeUniqueId) {
                    current_itcSourceCDR->field_unique_id_link = key;
                }

                if (storeBeforeChiocciola) {
                    current_itcSourceCDR->field_unique_id_before_chiocciola_link = key;
                }

                if (deleteBeforeChiocciola) {
                    // Remove the CDR from the index, because it has no any more this index
                    KeyIndex keyToRemove = current_itcSourceCDR->field_unique_id_before_chiocciola_link;
                    PPvoid_t pvalue;
                    JLG(pvalue, db_itcSourceCDRs, keyToRemove);
                    if (pvalue != NULL) {
                        int ignore;
                        J1U(ignore, *pvalue, (Word_t) current_itcSourceCDR);

                        // Remove also the key-entry if the set is empty
                        if (merge_countCDRSet(*pvalue) == 0) {
                            JLD(ignore, db_itcSourceCDRs, keyToRemove);
                        }
                    }

                    // The CDR can have the same index on other fields, so add again
                    // but only for relevant links
                    current_itcSourceCDR->field_unique_id_before_chiocciola_link = 0;
                    registerCDR(current_itcSourceCDR);
                }

            }

            parser_string[i] = backupChar;

        }
    }
}


////////////////////
// INIT FUNCTIONS //
////////////////////

void ParserResult_init(ParserResult * r, char * fname) {
    r->file_name = fname;
    r->is_there_date = 0;
    r->tot_lines = 0;
    r->tot_lines_with_errors = 0;
}

ParserResult current_ParserResult;

void init_itc_csv_processor(char * institution_domain_name) {

    config_maxDebugInfoToDisplayForEachTypeOfError = 5;

    dict_next_unique_index = 1;
    dict_PJArray = (PWord_t) NULL;

    db_itcSourceCDRs = (PWord_t) NULL;

    db_itSourceCDRs_withoutLinks = (PWord_t) NULL;

    itc_main_institution_domain = (char *) institution_domain_name;

    itc_initCallFlowMergeRuleStats();

    // Init the empty ITCScourceCDR

    ITCSourceCDR_empty.field_userfield_until_point_weak_link = 0;
    ITCSourceCDR_empty.field_unique_id_link = 0;
    ITCSourceCDR_empty.field_call_id_link = 0;
    ITCSourceCDR_empty.field_call_id2_link = 0;
    ITCSourceCDR_empty.field_unique_id_before_chiocciola_link = 0;
    ITCSourceCDR_empty.field_last_half_of_unique_id_link = 0;
    ITCSourceCDR_empty.field_first_half_of_unique_id_link = 0;
    ITCSourceCDR_empty.file_type = UNSPECIFIED_FILE;
    ITCSourceCDR_empty.field_billsec = 0;
    ITCSourceCDR_empty.field_calldate = 0;
    ITCSourceCDR_empty.field_src = NULL;
    ITCSourceCDR_empty.field_dst = NULL;
    ITCSourceCDR_empty.field_accountcode = NULL;
    ITCSourceCDR_empty.field_lastdata = NULL;
    ITCSourceCDR_empty.field_dst_channel_get_extension = NULL;
    ITCSourceCDR_empty.field_src_channel_get_extension = NULL;
    ITCSourceCDR_empty.destregid_vendorId = ITC_VENDOR_UNDEF;
    ITCSourceCDR_empty.sourceregid_vendorId = ITC_VENDOR_UNDEF;
    ITCSourceCDR_empty.status.isAnswered = 0;
    ITCSourceCDR_empty.status.force_debug = 0;
    ITCSourceCDR_empty.status.calldestregid_isExternalVoIPVendor  = 0;
    ITCSourceCDR_empty.status.callsrcregid_isExternalVoIPVendor  = 0;
    ITCSourceCDR_empty.status.lastData_isBackupChannel  = 0;
    ITCSourceCDR_empty.status.dstChannel_isEnumChannel  = 0;
    ITCSourceCDR_empty.status.dstChannel_isDAHDI  = 0;
    ITCSourceCDR_empty.status.srcChannel_isSIP  = 0;
    ITCSourceCDR_empty.status.dstChannel_isSIP  = 0;
    ITCSourceCDR_empty.status.dstChannel_isSIPAndSBC  = 0;
    ITCSourceCDR_empty.status.dcontext_isInternal  = 0;
    ITCSourceCDR_empty.status.dcontext_isFaxTpa  = 0;
    ITCSourceCDR_empty.status.dcontext_isFromTrunk  = 0;
    ITCSourceCDR_empty.status.dcontext_isFax  = 0;
    ITCSourceCDR_empty.status.dcontext_isExtLocal  = 0;
    ITCSourceCDR_empty.status.dcontext_isExtGroup  = 0;
    ITCSourceCDR_empty.status.dcontext_isToPBX  = 0;
    ITCSourceCDR_empty.status.dcontext_isFromDirectDDI  = 0;
    ITCSourceCDR_empty.status.dcontext_isFaxBackup  = 0;
    ITCSourceCDR_empty.status.dcontext_isFaxToBackup = 0;
    ITCSourceCDR_empty.status.lastdata_isExtensionAndDomain  = 0;
    ITCSourceCDR_empty.status.lastdata_isInternalChannel  = 0;
    ITCSourceCDR_empty.status.lastdata_isBackupChannel  = 0;
    ITCSourceCDR_empty.status.lastdata_isSIP  = 0;
    ITCSourceCDR_empty.status.srcChannel_isDAHDI  = 0;
    ITCSourceCDR_empty.status.lastapp_isResetCDR  = 0;
    ITCSourceCDR_empty.status.lastapp_isDial  = 0;
    ITCSourceCDR_empty.status.dstChannel_isLocal  = 0;
    ITCSourceCDR_empty.status.calldestregid_isIPBX  = 0;
    ITCSourceCDR_empty.status.dstChannel_isSIPWithExtension  = 0;
    ITCSourceCDR_empty.status.srcChannel_isSIPWithExtension  = 0;
    ITCSourceCDR_empty.status.dstChannel_isInstitutionDomain  = 0;
    ITCSourceCDR_empty.status.srcChannel_isInstitutionDomain  = 0;
    ITCSourceCDR_empty.status.dst_isFax  = 0;
    ITCSourceCDR_empty.status.callsrcregid_isMGW  = 0;
    ITCSourceCDR_empty.status.calldstregid_isMG  = 0;
    ITCSourceCDR_empty.status.dstchannel_isEmpty  = 0;
    ITCSourceCDR_empty.status.calldstregid_isNull = 0;
    ITCSourceCDR_empty.status.callsrcregid_isNull = 0;
    ITCSourceCDR_empty.status.can_be_billsec = 0;
    ITCSourceCDR_empty.status.csv_parsing_error = 0;
}

/////////////////////
// CALL FLOW MERGE //
/////////////////////

/**
 * @param set
 * @return the directions supported from the chain, inspecting only special CDRs.
 * Write also in the ITCSourceCDR the status can_be_billsec
 */
ChainDirection merge_classifyChainDirectionUsingSpecialCDRs(CDRSet set) {

    ChainDirection result;

    ChainDirection_reset(&result);

    int isInternal = 1;
    int isAnswered = 0;
    int isThereSBC = 0;

    int status = 1;
    Word_t scanKey = 0;
    J1F(status, set, scanKey);
    while(status == 1) {
        ITCSourceCDR * s = (ITCSourceCDR *) scanKey;

        s->status.can_be_billsec = 0;

        if (s->status.isAnswered) {
            isAnswered = 1;

            if (s->file_type == SBC_FILE && s->status.calldestregid_isExternalVoIPVendor) {
                result.outgoing = 1;
                isInternal = 0;
                isThereSBC = 1;
                s->status.can_be_billsec = 1;
            }

            if (s->file_type == SBC_FILE && s->status.callsrcregid_isExternalVoIPVendor) {
                result.incoming = 1;
                isInternal = 0;
                isThereSBC = 1;
                s->status.can_be_billsec = 1;
            }

            if (s->file_type == MGW_FILE && s->status.lastData_isBackupChannel) {
                result.outgoing = 1;
                result.pstn_backup = 1;
                isInternal = 0;
                s->status.can_be_billsec = 1;
            }

            if (s->file_type == IPBX_FILE && s->status.dstChannel_isEnumChannel) {
                result.outgoing = 1;
                result.enum_call = 1;
                isInternal = 0;
                s->status.can_be_billsec = 1;
            }

            if (s->file_type == MGW_FILE && s->status.dstChannel_isEnumChannel) {
                result.outgoing = 1;
                result.enum_call = 1;
                isInternal = 0;
                s->status.can_be_billsec = 1;
            }
        }

        J1N(status, set, scanKey);
    }

    if (isInternal) {
        if (isThereSBC) {
            result.incoming = 1;
        } else {
            result.internal = 1;
        }

        // In this case all the elements are valid billsec
        scanKey = 0;
        J1F(status, set, scanKey);
        while(status == 1) {
            ITCSourceCDR * cdr = (ITCSourceCDR *) scanKey;
            cdr->status.can_be_billsec = 1;
            J1N(status, set, scanKey);
        }
    }

    result.not_answered = !isAnswered;

    return result;
}

/**
 * @brief merge_removeAndFreeFromIndexedSet
 * @param indexedCDRS remove all the keys from this indexed set, and free also the corresponding sets.
 * @param keys
 */
void merge_removeAndFreeFromIndexedSet(IndexedCDRS * indexedCDRS, KeySet keys) {
    int status;
    Word_t key = 0;

    J1F(status, keys, key);
    while(status == 1) {
        int ignore;

        CDRSet set = merge_getCDRSet(*indexedCDRS, key);
        if (set != NULL) {
            merge_freeCDRSet(& set);
        }
        JLD(ignore, *indexedCDRS, key);
        assert(ignore != JERR);

        J1N(status, keys, key);
    }
}

/**
 * @brief clips_sourceCDR_template
 */
void * clips_sourceCDR_template;

void * clips_resultCDR_template;

void * clips_strongChain_template;

void * clips_strongChainIsGood_template;

void * clips_expectedDirection_template;

void * clips_pincodeExtension_template;

void * clips_conflictingRules_template;

void * clips_ambigousCdrForRule_template;

void * clips_missingDirection_template;

void * clips_unprocessedChain_template;

void * clips_cdrCanNotBeBillsec_template;

void * clips_ambigousPincode_template;

void * clips_cdrStatus_template;

void * clips_ruleDoNotUseCompleteChain_template;

/**
 * @brief clips_init
 * @param clipsRuleFileName
 * @param file_name where writing CDR results
 * @param is_new_file_name 1 for creating a new file, 0 for appending to it
 */
void clips_init(char * clipsRuleFileName, char *file_name, int is_new_file_name) {
    InitializeEnvironment();

    int r = Bload(GetCurrentEnvironment(), clipsRuleFileName);
    assert(r != 0);

    // Open the file on the CLIPS side.

    char * is_new_file_name_symbol;
    if (is_new_file_name) {
        is_new_file_name_symbol = "TRUE";
    } else {
        is_new_file_name_symbol = "FALSE";
    }
    const int max_size = 1024*5;
    char params[max_size];
    snprintf(params, max_size, "\"%s\" %s", file_name, is_new_file_name_symbol);

    DATA_OBJECT result;
    FunctionCall("open-result-file", params, &result);

    // Init references to defined templates.

    clips_sourceCDR_template = FindDeftemplate("source-cdr");
    assert(clips_sourceCDR_template != NULL);

    clips_resultCDR_template = FindDeftemplate("result-cdr");
    assert(clips_resultCDR_template != NULL);

    clips_strongChain_template = FindDeftemplate("strong-chain");
    assert(clips_strongChain_template != NULL);

    clips_expectedDirection_template = FindDeftemplate("expected-direction");
    assert(clips_expectedDirection_template != NULL);

    clips_pincodeExtension_template = FindDeftemplate("pincode-extension");
    assert(clips_pincodeExtension_template  != NULL);

    clips_conflictingRules_template = FindDeftemplate("conflicting-rules");
    assert(clips_conflictingRules_template  != NULL);

    clips_ambigousCdrForRule_template = FindDeftemplate("ambigous-cdr-for-rule");
    assert(clips_ambigousCdrForRule_template  != NULL);

    clips_missingDirection_template = FindDeftemplate("missing-direction");
    assert(clips_missingDirection_template  != NULL);

    clips_unprocessedChain_template = FindDeftemplate("unprocessed-chain");
    assert(clips_unprocessedChain_template  != NULL);

    clips_cdrCanNotBeBillsec_template = FindDeftemplate("cdr-can-not-be-billsec");
    assert(clips_cdrCanNotBeBillsec_template  != NULL);

    clips_ambigousPincode_template = FindDeftemplate("ambigous-pincode");
    assert(clips_ambigousPincode_template  != NULL);

    clips_cdrStatus_template = FindDeftemplate("cdr-status");
    assert(clips_cdrStatus_template != NULL);

    clips_strongChainIsGood_template = FindDeftemplate("strong-chain-is-good");
    assert(clips_strongChainIsGood_template != NULL);

    clips_ruleDoNotUseCompleteChain_template = FindDeftemplate("rule-do-not-use-complete-chain");
    assert(clips_ruleDoNotUseCompleteChain_template != NULL);
}

void clips_closeEnvironment() {
    DATA_OBJECT result;
    FunctionCall("close-result-file", NULL, &result);
}

inline void merge_exportIntToCLIPS(void *newFact, int value, char * fieldName) {
    DATA_OBJECT theValue;

    theValue.type = INTEGER;
    theValue.value = AddLong(value);
    int isOk  = PutFactSlot(newFact,fieldName,&theValue);
    assert(isOk);
}

extern inline void merge_exportIntToCLIPS(void *newFact, int value, char * fieldName);

inline void merge_exportLinkInfoToCLIPS(void *newFact, Word_t linkValue, char * fieldName) {
    DATA_OBJECT theValue;

    if (linkValue != 0) {
        theValue.type = INTEGER;
        theValue.value = AddLong(linkValue);
        int isOk = PutFactSlot(newFact,fieldName,&theValue);
        assert(isOk);
    }
}

extern inline void merge_exportLinkInfoToCLIPS(void *newFact, Word_t linkValue, char * fieldName);

inline void merge_exportStrToCLIPS(void *newFact, char * value, char * fieldName) {
    DATA_OBJECT theValue;

    if (value != NULL) {
        theValue.type = STRING;
        theValue.value = AddSymbol(value);
        int isOk = PutFactSlot(newFact,fieldName,&theValue);
        assert(isOk);
    }
}

extern inline void merge_exportStrToCLIPS(void *newFact, char * value, char * fieldName);

inline void merge_exportSymbolToCLIPS(void *newFact, char * value, char * fieldName) {
    DATA_OBJECT theValue;

    if (value != NULL) {
        theValue.type = SYMBOL;
        theValue.value = AddSymbol(value);
        int isOk = PutFactSlot(newFact,fieldName,&theValue);
        assert(isOk);
    }
}

extern inline void merge_exportSymbolToCLIPS(void *newFact, char * value, char * fieldName);

inline void merge_exportBoolToCLIPS(void *newFact, int value, char * fieldName) {
    DATA_OBJECT theValue;

    theValue.type = SYMBOL;
    if (value == 0) {
        theValue.value = AddSymbol("FALSE");
    } else {
        theValue.value = AddSymbol("TRUE");
    }

    int isOk = PutFactSlot(newFact,fieldName,&theValue);
    if (!isOk) {
        fprintf(stderr, "\nError asserting boolean value on fact %s\n", fieldName);
        assert(isOk);
    }
}

extern inline void merge_exportBoolToCLIPS(void *newFact, int value, char * fieldName);

inline void merge_exportCallDateToCLIPS(void * newFact, time_t value,  char * fieldName) {
    const int buffSize = 1024;
    char calldate_str[buffSize];
    strftime(calldate_str, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(& value));

    merge_exportStrToCLIPS(newFact, calldate_str, fieldName);
}

extern inline void merge_exportCallDateToCLIPS(void * newFact, time_t value,  char * fieldName);

inline void merge_setGlobalVarWithCallDate(char * varName, time_t calldate) {
    const int buffSize = 1024;
    char calldate_str[buffSize];
    strftime(calldate_str, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(& calldate));

    DATA_OBJECT theValue;

    theValue.type = STRING;
    theValue.value = AddSymbol(calldate_str);
    int isOk = SetDefglobalValue(varName, &theValue);
    assert(isOk);
}

extern inline void merge_setGlobalVarWithCallDate(char * varName, time_t calldate);

inline void merge_setGlobalVarDateToNil(char * varName) {
    DATA_OBJECT theValue;

    // NOTE: use STRING for using the same type of normal dates, so it is "nil" string and not nil symbol.
    theValue.type = STRING;
    theValue.value = AddSymbol("nil");
    int isOk = SetDefglobalValue(varName, &theValue);
    assert(isOk);
}

extern inline void merge_setGlobalVarDateToNil(char * varName);

inline void merge_setGlobalVarToNil(char * varName) {
    DATA_OBJECT theValue;

    theValue.type = SYMBOL;
    theValue.value = AddSymbol("nil");
    int isOk = SetDefglobalValue(varName, &theValue);
    assert(isOk);
}

extern inline void merge_setGlobalVarToNil(char * varName);

/**
 * @brief merge_exportExpectedDirectionToCLIPS
 * @param strongChainId
 * @param direction
 */
void merge_exportExpectedDirectionToCLIPS(int strongChainId, char * direction) {
    int isOk;
    void *newFact;

    newFact = CreateFact(clips_expectedDirection_template);
    assert(newFact != NULL);

    merge_exportSymbolToCLIPS(newFact, direction, "direction");

    merge_exportIntToCLIPS(newFact, strongChainId, "chain-id");

    isOk = AssignFactSlotDefaults(newFact);
    Assert(newFact);
}

void merge_exportStrongChainIsGood(int strongChainId) {
    int isOk;
    void *newFact;

    newFact = CreateFact(clips_strongChainIsGood_template);
    assert(newFact != NULL);

    merge_exportIntToCLIPS(newFact, strongChainId, "chain-id");

    isOk = AssignFactSlotDefaults(newFact);
    Assert(newFact);
}

/**
 * @brief merge_countCDRStatus
 * @param cdr
 * @return the number of fields set in the status
 */
int merge_countCDRStatus(ITCSourceCDR * cdr) {
    int r = 0;

    if (cdr->status.isAnswered) {
        r++;
    }

    if (cdr->status.csv_parsing_error) {
        r++;
    }

    if (cdr->status.force_debug) {
        r++;
    }

    if (cdr->status.can_be_billsec) {
        r++;
    }

    if (cdr->status.calldestregid_isExternalVoIPVendor) {
        r++;
    }

    if (cdr->status.callsrcregid_isExternalVoIPVendor) {
        r++;
    }

    if (cdr->status.lastData_isBackupChannel) {
        r++;
    }

    if (cdr->status.dstChannel_isEnumChannel) {
        r++;
    }

    if (cdr->status.dstChannel_isDAHDI) {
        r++;
    }

    if (cdr->status.srcChannel_isSIP) {
        r++;
    }

    if (cdr->status.dstChannel_isSIP) {
        r++;
    }

    if (cdr->status.dstChannel_isSIPAndSBC) {
        r++;
    }

    if (cdr->status.dcontext_isInternal) {
        r++;
    }

    if (cdr->status.dcontext_isFaxTpa) {
        r++;
    }

    if (cdr->status.dcontext_isFromTrunk) {
        r++;
    }

    if (cdr->status.dcontext_isFax) {
        r++;
    }

    if (cdr->status.dcontext_isExtLocal) {
        r++;
    }

    if (cdr->status.dcontext_isExtGroup) {
        r++;
    }

    if (cdr->status.dcontext_isToPBX) {
        r++;
    }

    if (cdr->status.dcontext_isFromDirectDDI) {
        r++;
    }

    if (cdr->status.dcontext_isFaxBackup) {
        r++;
    }

    if (cdr->status.dcontext_isFaxToBackup) {
        r++;
    }


    if (cdr->status.lastdata_isExtensionAndDomain) {
        r++;
    }

    if (cdr->status.lastdata_isInternalChannel) {
        r++;
    }

    if (cdr->status.lastdata_isBackupChannel) {
        r++;
    }

    if (cdr->status.lastdata_isSIP) {
        r++;
    }

    if (cdr->status.srcChannel_isDAHDI) {
        r++;
    }


    if (cdr->status.lastapp_isResetCDR) {
        r++;
    }


    if (cdr->status.lastapp_isDial) {
        r++;
    }

    if (cdr->status.dstChannel_isLocal) {
        r++;
    }

    if (cdr->status.calldestregid_isIPBX) {
        r++;
    }

    if (cdr->status.dstChannel_isSIPWithExtension) {
        r++;
    }

    if (cdr->status.srcChannel_isSIPWithExtension) {
        r++;
    }

    if (cdr->status.dstChannel_isInstitutionDomain) {
        r++;
    }

    if (cdr->status.srcChannel_isInstitutionDomain) {
        r++;
    }

    if (cdr->status.dst_isFax) {
        r++;
    }

    if (cdr->status.callsrcregid_isMGW) {
        r++;
    }

    if (cdr->status.calldstregid_isMG) {
        r++;
    }

    if (cdr->status.dstchannel_isEmpty) {
        r++;
    }

    if (cdr->status.callsrcregid_isNull) {
        r++;
    }

    if (cdr->status.calldstregid_isNull) {
        r++;
    }

    return r;

}

/**
 * @brief merge_exportCDRStatusToCLIPS
 * @param newFact
 * @param status
 */
void merge_exportCDRStatusToCLIPS(ITCSourceCDR * cdr, char * status) {
    int isOk;
    void *newFact;

    newFact = CreateFact(clips_cdrStatus_template);
    assert(newFact != NULL);

    merge_exportIntToCLIPS(newFact, (Word_t) cdr, "cdr-id");
    merge_exportSymbolToCLIPS(newFact, status, "status");

    isOk = AssignFactSlotDefaults(newFact);
    assert(isOk);
    Assert(newFact);
}

/**
 * @brief
 * @param cdr
 */
void merge_exportAllCDRStatusToCLIPS(ITCSourceCDR * cdr) {
    if (cdr->status.isAnswered) {
        merge_exportCDRStatusToCLIPS(cdr, "isAnswered");
    }

    if (cdr->status.csv_parsing_error) {
        merge_exportCDRStatusToCLIPS(cdr, "csv_parsing_error");
    }

    if (cdr->status.force_debug) {
        merge_exportCDRStatusToCLIPS(cdr, "force_debug");
    }

    if (cdr->status.can_be_billsec) {
        merge_exportCDRStatusToCLIPS(cdr, "can_be_billsec");
    }

    if (cdr->status.calldestregid_isExternalVoIPVendor) {
        merge_exportCDRStatusToCLIPS(cdr, "calldestregid_isExternalVoIPVendor");
    }

    if (cdr->status.callsrcregid_isExternalVoIPVendor) {
        merge_exportCDRStatusToCLIPS(cdr, "callsrcregid_isExternalVoIPVendor");
    }

    if (cdr->status.lastData_isBackupChannel) {
        merge_exportCDRStatusToCLIPS(cdr, "lastData_isBackupChannel");
    }

    if (cdr->status.dstChannel_isEnumChannel) {
        merge_exportCDRStatusToCLIPS(cdr, "dstChannel_isEnumChannel");
    }

    if (cdr->status.dstChannel_isDAHDI) {
        merge_exportCDRStatusToCLIPS(cdr, "dstChannel_isDAHDI");
    }

    if (cdr->status.srcChannel_isSIP) {
        merge_exportCDRStatusToCLIPS(cdr, "srcChannel_isSIP");
    }

    if (cdr->status.dstChannel_isSIP) {
        merge_exportCDRStatusToCLIPS(cdr, "dstChannel_isSIP");
    }

    if (cdr->status.dstChannel_isSIPAndSBC) {
        merge_exportCDRStatusToCLIPS(cdr, "dstChannel_isSIPAndSBC");
    }

    if (cdr->status.dcontext_isInternal) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isInternal");
    }

    if (cdr->status.dcontext_isFaxTpa) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isFaxTpa");
    }

    if (cdr->status.dcontext_isFromTrunk) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isFromTrunk");
    }

    if (cdr->status.dcontext_isFax) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isFax");
    }

    if (cdr->status.dcontext_isExtLocal) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isExtLocal");
    }

    if (cdr->status.dcontext_isExtGroup) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isExtGroup");
    }

    if (cdr->status.dcontext_isToPBX) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isToPBX");
    }

    if (cdr->status.dcontext_isFromDirectDDI) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isFromDirectDDI");
    }

    if (cdr->status.dcontext_isFaxBackup) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isFaxBackup");
    }

    if (cdr->status.dcontext_isFaxToBackup) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isFaxToBackup");
    }

    if (cdr->status.lastdata_isExtensionAndDomain) {
        merge_exportCDRStatusToCLIPS(cdr, "lastdata_isExtensionAndDomain");
    }

    if (cdr->status.lastdata_isInternalChannel) {
        merge_exportCDRStatusToCLIPS(cdr, "lastdata_isInternalChannel");
    }

    if (cdr->status.lastdata_isBackupChannel) {
        merge_exportCDRStatusToCLIPS(cdr, "lastdata_isBackupChannel");
    }

    if (cdr->status.lastdata_isSIP) {
        merge_exportCDRStatusToCLIPS(cdr, "lastdata_isSIP");
    }

    if (cdr->status.srcChannel_isDAHDI) {
        merge_exportCDRStatusToCLIPS(cdr, "srcChannel_isDAHDI");
    }


    if (cdr->status.lastapp_isResetCDR) {
        merge_exportCDRStatusToCLIPS(cdr, "lastapp_isResetCDR");
    }

    if (cdr->status.lastapp_isDial) {
        merge_exportCDRStatusToCLIPS(cdr, "lastapp_isDial");
    }

    if (cdr->status.dstChannel_isLocal) {
        merge_exportCDRStatusToCLIPS(cdr, "dstChannel_isLocal");
    }

    if (cdr->status.calldestregid_isIPBX) {
        merge_exportCDRStatusToCLIPS(cdr, "calldestregid_isIPBX");
    }

    if (cdr->status.dcontext_isFaxToBackup ) {
        merge_exportCDRStatusToCLIPS(cdr, "dcontext_isFaxToBackup");
    }


    if (cdr->status.dstChannel_isSIPWithExtension) {
        merge_exportCDRStatusToCLIPS(cdr, "dstChannel_isSIPWithExtension");
    }

    if (cdr->status.srcChannel_isSIPWithExtension) {
        merge_exportCDRStatusToCLIPS(cdr, "srcChannel_isSIPWithExtension");
    }

    if (cdr->status.dstChannel_isInstitutionDomain) {
        merge_exportCDRStatusToCLIPS(cdr, "dstChannel_isInstitutionDomain");
    }

    if (cdr->status.srcChannel_isInstitutionDomain) {
        merge_exportCDRStatusToCLIPS(cdr, "srcChannel_isInstitutionDomain");
    }

    if (cdr->status.dst_isFax) {
        merge_exportCDRStatusToCLIPS(cdr, "dst_isFax");
    }

    if (cdr->status.callsrcregid_isMGW) {
        merge_exportCDRStatusToCLIPS(cdr, "callsrcregid_isMGW");
    }

    if (cdr->status.calldstregid_isMG) {
        merge_exportCDRStatusToCLIPS(cdr, "calldstregid_isMG");
    }

    if (cdr->status.dstchannel_isEmpty) {
        merge_exportCDRStatusToCLIPS(cdr, "dstchannel_isEmpty");
    }

    if (cdr->status.callsrcregid_isNull) {
        merge_exportCDRStatusToCLIPS(cdr, "callsrcregid_isNull");
    }

    if (cdr->status.calldstregid_isNull) {
        merge_exportCDRStatusToCLIPS(cdr, "calldstregid_isNull");
    }
}

void merge_sendStrongChainToCLIPS(int id, int chainLength) {

    int isOk;
    void *newFact;
    DATA_OBJECT theValue;

    newFact = CreateFact(clips_strongChain_template);
    assert(newFact != NULL);

    theValue.type = INTEGER;
    theValue.value = AddLong(id);
    isOk = PutFactSlot(newFact,"id",&theValue);
    assert(isOk);

    theValue.type = INTEGER;
    theValue.value = AddLong(chainLength);
    isOk = PutFactSlot(newFact,"len",&theValue);
    assert(isOk);

    isOk = AssignFactSlotDefaults(newFact);
    assert(isOk);
    Assert(newFact);

}

inline int date_respectRateParams(time_t d) {
    if (currentRateParams.fromDate <= d
            && ( (!currentRateParams.thereIsToDate) ||
                 currentRateParams.toDate > d)) {
        return 1;
    } else {
        return 0;
    }
}

extern inline int date_respectRateParams(time_t d);

/**
 * @brief merge_fromDirectionSymbol
 * @param direction something like "incoming", or "outgoing"
 * @return
 */
CDR_direction merge_fromDirectionSymbol(char *direction) {
    if (strcmp(direction, "incoming") == 0) {
        return  CDR_INCOMING;
    }

    if (strcmp(direction, "outgoing") == 0) {
        return  CDR_OUTGOING;
    }

    if (strcmp(direction, "internal") == 0) {
        return  CDR_INTERNAL;
    }

    if (strcmp(direction, "ignored") == 0) {
        return  CDR_IGNORED;
    }

    if (strcmp(direction, "unprocessed") == 0) {
        return  CDR_UNPROCESSED;
    }

    if (strcmp(direction, "error") == 0) {
        return  CDR_ERROR;
    }

    assert(0);
}

/**
 * @brief merge_sendCDRToCLIPS
 * @param cdr
 * @param chainLenght
 * @return 1 if it is all ok
 */
void merge_sendCDRToCLIPS(ITCSourceCDR * cdr, int strongChainId) {
    int isOk;
    void *newFact;

    DATA_OBJECT theValue;

    newFact = CreateFact(clips_sourceCDR_template);
    assert(newFact != NULL);

    // Use the CDR address as unique identifier.
    theValue.type = INTEGER;
    theValue.value = AddLong((Word_t) cdr);
    isOk = PutFactSlot(newFact,"id",&theValue);
    assert(isOk);

    theValue.type = INTEGER;
    theValue.value = AddLong(strongChainId);
    isOk = PutFactSlot(newFact,"strong-chain-id",&theValue);
    assert(isOk);

    theValue.type = SYMBOL;
    switch(cdr->file_type) {
    case MGW_FILE:
        theValue.value = AddSymbol("mgw");
        break;
    case IPBX_FILE:
        theValue.value = AddSymbol("ipbx");
        break;
    case SBC_FILE:
        theValue.value = AddSymbol("sbc");
        break;

    default:
        assert(0);
    }
    isOk = PutFactSlot(newFact,"is-type",&theValue);
    assert(isOk);

    theValue.type = INTEGER;
    theValue.value = AddLong(cdr->field_billsec);
    isOk = PutFactSlot(newFact,"billsec",&theValue);
    assert(isOk);

    merge_exportCallDateToCLIPS(newFact, cdr->field_calldate, "calldate");

    merge_exportLinkInfoToCLIPS(newFact, cdr->field_userfield_until_point_weak_link, "userfield_until_point");
    merge_exportLinkInfoToCLIPS(newFact, cdr->field_unique_id_link, "unique_id");
    merge_exportLinkInfoToCLIPS(newFact, cdr->field_call_id_link, "callid");
    merge_exportLinkInfoToCLIPS(newFact, cdr->field_call_id2_link, "callid2");
    merge_exportLinkInfoToCLIPS(newFact, cdr->field_unique_id_before_chiocciola_link, "unique_id_before_chiocciola");
    merge_exportLinkInfoToCLIPS(newFact, cdr->field_last_half_of_unique_id_link, "last_half_of_unique_id");
    merge_exportLinkInfoToCLIPS(newFact, cdr->field_first_half_of_unique_id_link, "first_half_of_unique_id");

    merge_exportStrToCLIPS(newFact, cdr->field_src, "src");
    merge_exportStrToCLIPS(newFact, cdr->field_dst, "dst");
    merge_exportStrToCLIPS(newFact, cdr->field_lastdata, "lastdata");
    merge_exportStrToCLIPS(newFact, cdr->field_accountcode, "accountcode");
    merge_exportStrToCLIPS(newFact, cdr->field_dst_channel_get_extension, "dst_channel_get_extension");
    merge_exportStrToCLIPS(newFact, cdr->field_src_channel_get_extension, "src_channel_get_extension");

    merge_exportStrToCLIPS(newFact, itcVendor_getVendorString(cdr->destregid_vendorId), "destregid_vendor");
    merge_exportStrToCLIPS(newFact, itcVendor_getVendorString(cdr->sourceregid_vendorId), "srcregid_vendor");

    merge_exportBoolToCLIPS(newFact, (int) cdr->status.can_be_billsec, "can-be-billsec");
    merge_exportBoolToCLIPS(newFact, (int) cdr->status.isAnswered, "is-answered");

    merge_exportAllCDRStatusToCLIPS(cdr);

    isOk = AssignFactSlotDefaults(newFact);
    assert(isOk);

    Assert(newFact);
}

void merge_sendErrorResultCDRToCLIPS(time_t callDate) {
    void *newFact;

    newFact = CreateFact(clips_resultCDR_template);
    assert(newFact != NULL);

    // use the special system rule
    merge_exportIntToCLIPS(newFact, 0, "rule-id");

    merge_exportSymbolToCLIPS(newFact, "outgoing", "destination-type");

    merge_exportCallDateToCLIPS(newFact, callDate, "calldate");

    merge_exportBoolToCLIPS(newFact, 1, "is-error");

    merge_exportBoolToCLIPS(newFact, 0, "is-redirect");

    int isOk = AssignFactSlotDefaults(newFact);
    assert(isOk);

    Assert(newFact);
}

/**
 * @brief merge_itc_exportPinCodeToCLIPS export PINCODE info to CLIPS
 * @param set
 * @param strongChainId strong
 * @return int
 */
int merge_itc_exportPinCodeToCLIPS(CDRSet set, int strongChainId) {
    int status;
    int isOk;
    Word_t scanKey = 0;
    J1F(status, set, scanKey);
    while(status == 1) {
        ITCSourceCDR * cdr = (ITCSourceCDR*) scanKey;

        if (cdr->field_accountcode != NULL) {
            PinCodeInfo * info =  pinCodesInfo_get(cdr->field_accountcode, cdr->field_calldate);
            if (info != NULL) {
                // NOTE: in case of more than one PINCODE to convert, the error is returned on the CLIPS side.

                void *newFact;

                newFact = CreateFact(clips_pincodeExtension_template);
                if (newFact == NULL) return 0;

                merge_exportIntToCLIPS(newFact, strongChainId, "strong-chain-id");
                merge_exportStrToCLIPS(newFact, info->extension, "extension");

                isOk = AssignFactSlotDefaults(newFact);
                if (!isOk) return 0;

                Assert(newFact);
            }
        }

        J1N(status, set, scanKey);
    }

    return 1;
}

void sendRateParamsToCLIPS() {
    // Set the rating timeframe.
    merge_setGlobalVarWithCallDate("rate-from-date", currentRateParams.fromDate);
    if (currentRateParams.thereIsToDate) {
        merge_setGlobalVarWithCallDate("rate-to-date", currentRateParams.toDate);
    } else {
        merge_setGlobalVarDateToNil("rate-to-date");
    }
}

/**
 * @brief merge_processCDRKey
 * @param cdr the CDR to process
 * @param key the key associated to the CDR to add
 * @param previousKeys the previous processed keys
 * @param newKeys the new discovered keys
 * @param createNewIndexedCDRS 1 for updating also a IndexedCDRS structure
 * @param newIndexedCDRS
 * @return 0 if the key was 0
 *
 * @ensure the key in newKeys, only if it is not in previousKeys
 * @ensure add CDRS in newIndexedCDRS structure, also if the key is not new
 */
int merge_processCDRKey(ITCSourceCDR *cdr, KeyIndex key, KeySet previousKeys, KeySet * newKeys, int createNewIndexedCDRS, IndexedCDRS * newIndexedCDRS) {

    if (key != 0) {
        // process only new keys
        if (!merge_existsInKeySet(previousKeys, key)) {
            merge_addInKeySet(newKeys, key);
        }

        // add CDRS in the indexed structure also if the key is not new.
        if (createNewIndexedCDRS) {
            merge_addInIndexedCDRS(newIndexedCDRS, key, cdr);
        }

        return 1;
    } else {
        return 0;
    }
}

/**
 * @brief merge_addLinkedCDRSet discover new keys that are linked to CDRS in the source
 * @param source the CDRS to process. They are only read.
 * @param dest where putting the new processed CDRS, and where there are the old processed CDRS. In the end this is the complete or strong chain.
 * @param previousKeys the already processed keys
 * @param newKeys where putting the new discovered keys (only the keys not in previousKeys)
 * @param onlyStrongLinks 1 for considering only strong links, 0 for considering also weak links
 * @param createNewIndexedCDRS 1 for adding also the processed CDRS to a IndexedCDRS structure
 * @param newIndexedCDRS updated in case createNewIndexedCDRS is equal to 1
 * @param isolatedCDRS where storing CDRS with only weak links, and that can not be stored in newIndexedCDRS
 *
 * @ensure dest will be the complete/strong chain of related CDRS, when fix point is reached
 * @ensure previousKeys + newKeys will be the complete/strong chain of related CDRS, when fix point is reached
 * @ensure newKeys will contain only new discovered keys, not already in previousKeys
 * @ensure each CDRS in source will be added to dest
 * @ensure for each new found CDR, all its new keys will be put in newKeys, so during fix point all keys and CDRS are found
 * @ensure newIndexedCDRS will never contain a map between a  key, and a CDR with a weak-link on it, also if onlyStrongLinks is specified,
 * in this way newIndexedCDRS can be used for discovering strong-chains.
 */
void merge_addLinkedCDRSet(CDRSet source, CDRSet * dest, KeySet previousKeys, KeySet * newKeys, int onlyStrongLinks, int createNewIndexedCDRS, IndexedCDRS * newIndexedCDRS, CDRSet * isolatedCDRS) {
    // Scan all CDRS in source
    int status_source;
    Word_t key_source = 0;
    J1F(status_source, source, key_source);
    while(status_source) {
        ITCSourceCDR * cdr = (ITCSourceCDR * ) key_source;

        if (merge_addInCDRSet(dest, cdr)) {
            // process each new CDR that were not already in dest, taking note of the new discovered keys

            int hasStrongLinks = 0;

            hasStrongLinks += merge_processCDRKey(cdr, cdr->field_first_half_of_unique_id_link, previousKeys, newKeys, createNewIndexedCDRS, newIndexedCDRS);
            hasStrongLinks += merge_processCDRKey(cdr, cdr->field_last_half_of_unique_id_link, previousKeys, newKeys, createNewIndexedCDRS, newIndexedCDRS);
            hasStrongLinks += merge_processCDRKey(cdr, cdr->field_unique_id_link, previousKeys, newKeys, createNewIndexedCDRS, newIndexedCDRS);
            hasStrongLinks += merge_processCDRKey(cdr, cdr->field_call_id_link, previousKeys, newKeys, createNewIndexedCDRS, newIndexedCDRS);
            hasStrongLinks += merge_processCDRKey(cdr, cdr->field_call_id2_link, previousKeys, newKeys, createNewIndexedCDRS, newIndexedCDRS);
            hasStrongLinks += merge_processCDRKey(cdr, cdr->field_unique_id_before_chiocciola_link, previousKeys, newKeys, createNewIndexedCDRS, newIndexedCDRS);

            if (!onlyStrongLinks) {
                // NOTE: in this case we process the CDR, but we do not add weak links keys in the indexed-table, because the indexed table is used for strong-chains
                // recognitions in next phase, and in case of a strong chain the weak links are not membership conditions.
                merge_processCDRKey(cdr, cdr->field_userfield_until_point_weak_link, previousKeys, newKeys, 0, newIndexedCDRS);

                if (createNewIndexedCDRS &&  hasStrongLinks == 0) {
                    // in this case it is an isolated CDRS, not saved in the index structure with some strong link
                    merge_addInCDRSet(isolatedCDRS, cdr);
                }
            }
        }
        J1N(status_source, source, key_source);
    }

#if ENABLE_DEBUG_MODE == 1
    assert(merge_testIfKeySetIsNotCorrupted(previousKeys));
    assert(merge_testIfKeySetIsNotCorrupted(*newKeys));
    assert(merge_testIfCDRSetIsNotCorrupted(*dest));
    if (newIndexedCDRS != NULL) {
        assert(merge_testIfItIsNotCorrupted(*newIndexedCDRS));
    }

    KeySet completeKeys = NULL;
    merge_addKeySet(previousKeys, &completeKeys);
    merge_addKeySet(*newKeys, &completeKeys);
    assert(merge_testForClosure(*dest, completeKeys, !onlyStrongLinks));
    merge_freeKeySet(&completeKeys);
#endif

}

/**
 * @brief merge_parametricMergePass extract a key from sourceCDRS and create a merge of all related CDRS.
 * Then stop informing if there is other data to process on sourceCDRS.
 * This function is used both for complete (createNewIndexedCDRS == 1)
 * and strong-chains (createNewIndexedCDRS == 0) creation.
 *
 * @param sourceCDRS where extracting (reading and moving) the sourcCDRS to merge.
 * @param createNewIndexedCDRS 1 for creating also an auxiliary index from key to extracted CDRS, considering only strong links.
 * @param newIndexedCDRS where storing the auxiliary index
 * @param isolatedCDRS where storing the CDRS with only weak links, and that can not be put in newIndexedCDRS
 * @param extractedCDRS where put the CDRS that are extracted
 * @param extractedKeys where put the corresponding extracted keys
 * @return 1 if there were other elements to process (call again this function), 0 otherwise.
 *
 * @ensure sourceCDRS will not contains all the extractedKeys, so the fix-point of the procedure it is when it is empty
 * @ensure newIndexedCDRS will contain only CDRS associated to a key with strong-links on it,
 * so it can be used later for deriving the strong chain of a complete chain.
 * @ensure extractedCDRS will contain a strong or complete chain
 * @ensure extractedKeys are all the keys strong or weak keys in extractedCDRS
 */
int merge_parametricMergePass(IndexedCDRS * sourceCDRS, int createNewIndexedCDRS, IndexedCDRS * newIndexedCDRS, CDRSet * isolatedCDRS, CDRSet * extractedCDRS, KeySet * extractedKeys) {

    // Start with a key to process, if there is any...

    KeyIndex initialKey;
    Word_t *pvalue;

    initialKey = 0;
    JLF(pvalue, * sourceCDRS, initialKey);
    if (pvalue == NULL) {
        return 0;
    }

    // Start with the initial key, that is for sure a new key to process,
    // because it is a key of a CDR not already processed.
    // The not processed CDRS were not part of a previous chain, so it is a new key.

    // in the first phase,
    int onlyStrongLinks;
    if (createNewIndexedCDRS) {
        // in the first phase a complete chain is created, and also weak links are followed.
        // NOTE that in any case in the newIndexedCDRS only the strong links are automatically added,
        // because they are used in later phase for extracting strong chains.
        onlyStrongLinks = 0;
    } else {
        // in this phase only strong links are used for determining the CDRS in the same class,
        // because strong chains must be built.
        onlyStrongLinks  = 1;
    }

    KeySet localNewCompleteKeys = NULL;
    merge_addInKeySet(& localNewCompleteKeys, initialKey);

    while(!merge_isEmptyKeySet(localNewCompleteKeys)) {
        // signal that these keys are not new anymore...
        merge_addKeySet(localNewCompleteKeys, extractedKeys);

        // this will be the new set of new keys to process at next phase
        KeySet localNewNewCompleteKeys = NULL;

        // Process all new keys found in previous iteration
        int status = 1;
        KeyIndex keyToProcess = 0;
        J1F(status, localNewCompleteKeys, keyToProcess);
        while(status) {
            CDRSet newCDRS = merge_getCDRSet(*sourceCDRS, keyToProcess);

            merge_addLinkedCDRSet(newCDRS, extractedCDRS, * extractedKeys, & localNewNewCompleteKeys, onlyStrongLinks, createNewIndexedCDRS, newIndexedCDRS, isolatedCDRS);

            J1N(status, localNewCompleteKeys, keyToProcess);

#if ENABLE_DEBUG_MODE == 1
            if (newIndexedCDRS != NULL) {
                assert(merge_testIfItIsNotCorrupted(*newIndexedCDRS));
            }
            assert(merge_testIfCDRSetIsNotCorrupted(*extractedCDRS));
            assert(merge_testIfKeySetIsNotCorrupted(*extractedKeys));
            assert(merge_testIfKeySetIsNotCorrupted(localNewCompleteKeys));
            assert(merge_testIfKeySetIsNotCorrupted(localNewNewCompleteKeys));
#endif
        }

        // Discard the old version of the set, and replace with the new one
        merge_freeKeySet(& localNewCompleteKeys);
        localNewCompleteKeys = localNewNewCompleteKeys;
        // contitue processing the set of new new keys (if there are any)
    }

    // Remove the extracted keys.

#if ENABLE_DEBUG_MODE == 1
    Word_t c1 = merge_countIndexedSet(*sourceCDRS);
    Word_t c2 = merge_countCDRSet(*extractedKeys);
#endif

    merge_removeAndFreeFromIndexedSet(sourceCDRS, * extractedKeys);

#if ENABLE_DEBUG_MODE == 1
    assert(merge_testForClosure(*extractedCDRS, *extractedKeys, createNewIndexedCDRS));

    Word_t c3 = merge_countIndexedSet(*sourceCDRS);
    if (!(c3 == c1 - c2)) {
        assert(merge_testIfCDRSetIsNotCorrupted(*sourceCDRS));
        assert(merge_testIfCDRSetIsNotCorrupted(*extractedCDRS));
        assert(merge_testIfKeySetIsNotCorrupted(*extractedKeys));
        assert(0);
    }
#endif

    return 1;
}

int merge_passage;

void merge_printQuickDebugInfo(ITCSourceCDR *cdr) {

    const int buffSize = 1024;
    char buff[buffSize];

    strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(& cdr->field_calldate));
    printf("CDR: type: %s, calldate: %s, billsec: %d, src: %s, dst: %s\n",
                maybeEmptyStr(itcFileTypeName(cdr->file_type)),
                buff,
                cdr->field_billsec,
                maybeEmptyStr(cdr->field_src),
                maybeEmptyStr(cdr->field_dst)
    );
}

/**
 * @brief executeMergePass execute a processing passage, merging the first CDR to process,
 * and all linked CDRS.
 * @return 0 if there is no data to merge, 1 if there is more data to process.
 *
 * @ensure db_itcSourceCDRs will be all processed after iterative calls
 * @ensure db_itSourceCDRs_withoutLinks will be all processed after iterative calls
 * @ensure for all CDRS strong and complete chains are derived and sent to CLIPS rules, and then rated
 */
int executeMergePassAndRateCall(char * clipsRuleFileName) {

    merge_passage++;

    int ignore = 0;

    //
    // Extract the complete chain of CDRS, linked by weak and strong links.
    //

#if ENABLE_DEBUG_MODE == 1

    if (initial_debug_messages) {
        printf("\nInitial tests.");

        KeyIndex indexToInspect = 73;
        CDRSet sameSet = merge_getCDRSet(db_itcSourceCDRs, indexToInspect);
        merge_displayInfoAboutMissingKey(sameSet, indexToInspect, 1);

        // Show first values
        GString * html = g_string_new("");
        Word_t index = 0;
        int status = 0;
        J1F(status, sameSet, index);
        int maxCDRS = 25;
        while(status && maxCDRS > 0) {
            ITCSourceCDR * cdr = (ITCSourceCDR *) index;

            ITCSourceCDR_show_debug_description(html, "CDR", cdr);

            J1N(status, sameSet, index);

            maxCDRS--;
        }

        printf("\n%s", html->str);

    }

    initial_debug_messages = 0;

    // Reduce the number of tests, because they are too much slow
    if (merge_passage % 200 == 0) {
        int testIndex = merge_checkAllIndexedCDRS(db_itcSourceCDRs, 1);
        if (!testIndex) {
            printf("\nError on index at merge passage %i, BEFORE CALL FLOW MERGE.", merge_passage);
        }
        assert(testIndex);
    }

#endif

    IndexedCDRS completeChainCDRS_indexed = NULL;
    CDRSet cdrsInCompleteChain = NULL;
    KeySet keysInCompleteChain = NULL;
    CDRSet isolatedCDRS = NULL;

    int isThereChain = merge_parametricMergePass(&db_itcSourceCDRs, 1, & completeChainCDRS_indexed , &isolatedCDRS, & cdrsInCompleteChain, & keysInCompleteChain);

#if ENABLE_DEBUG_MODE == 1

    Word_t howManyKeys = merge_countCDRSet(keysInCompleteChain);
    Word_t howManyIndex = merge_countIndexedSet(db_itcSourceCDRs);

    if (merge_passage % 5000 == 0) {
        printf("\n Total left keys are: %lu, merged keys in the complete chain are %lu, merged CDRS are %lu ", howManyIndex, merge_countCDRSet(keysInCompleteChain), merge_countCDRSet(cdrsInCompleteChain));
    }
#endif

    if (!isThereChain) {
        // There no more indexed CDRS to process.
        // So now process CDRS having no other link.
        // Move these CDRS to the indexed data-structure, using their unique address
        // like pseudo-index. Later they will be processed using the normal algo,
        // that will work on chains of lenght 1.
        // NOTE: it is not the most efficient way to do things, because we are performing a moving
        // of data, and we are calling a generic algo, on a simple case (chain of lenght 1)
        // where something of faster can be implemented instead.

        int atLeastOne = 0;
        int again = 0;
        Word_t scanKey = 0;
        J1F(again, db_itSourceCDRs_withoutLinks, scanKey);
        while(again) {
            ITCSourceCDR * cdr = (ITCSourceCDR *) scanKey;
            atLeastOne = 1;
            merge_addInIndexedCDRS(& db_itcSourceCDRs, (Word_t) cdr, cdr);
            J1N(again, db_itSourceCDRs_withoutLinks, scanKey);
        }
        merge_freeCDRSet(& db_itSourceCDRs_withoutLinks);

        // do not process nothing, but inform that there is data to process at next passage.
        // NOTE: if the move was already done, then 0 is returned, otherwise 1 is returned
        // and the next iterations will remove all elements from db_itcSourceCDRs
        return atLeastOne;
    }


    //
    // Decide if the chain must be processed.
    //

    time_t maxCalldateInChain;
    time_t minimumCalldateInChain;
    int isThereMinimumCalldateInChain = 0;

    int continueWithChainProcessing = 1;
    int displayCLIPSDebugInfo = 0;

    int completeChainLen = merge_countCDRSet(cdrsInCompleteChain);
    if (completeChainLen > MAX_CHAIN_LEN) {
        // The CDRs of this chain must be ignored, because it is a case of bad chain,
        // due to misconfigurations of the VoIP servers.
        continueWithChainProcessing = 0;
        itc_signal_chain_too_long(completeChainLen);

    } else {

        // Ignore the chains where all the CDRs are outside the rate params, because:
        // * they can also be incomplete, because part of the chain can be missing, and so they can genarate false errors;
        // * in any case the resulting cdr must be not rated, because it is for sure outside the rating date range window;
        //
        // Ignore also chains having a CDR with a parsing error.
        //
        // Ignore also chains having only unanswered CDRS.

        int chainRespectRateParams = 0;
        int isThereCSVParsingError = 0;
        int isThereAnsweredCDR = 0;

        displayCLIPSDebugInfo = produce_debug_info && itcSourceCDRs_isForceDebug(cdrsInCompleteChain);

        int status;
        Word_t scanKey = 0;
        J1F(status, cdrsInCompleteChain, scanKey);
        while(status) {
            ITCSourceCDR * cdr = (ITCSourceCDR * ) scanKey;

            if (isThereMinimumCalldateInChain) {
                if (minimumCalldateInChain > cdr->field_calldate) {
                    minimumCalldateInChain = cdr->field_calldate;
                }
                if (maxCalldateInChain < cdr->field_calldate) {
                    maxCalldateInChain = cdr->field_calldate;
                }

            } else {
                isThereMinimumCalldateInChain = 1;
                minimumCalldateInChain = cdr->field_calldate;
                maxCalldateInChain = cdr->field_calldate;
            }

            chainRespectRateParams = chainRespectRateParams || date_respectRateParams(cdr->field_calldate);
            isThereCSVParsingError = isThereCSVParsingError || cdr->status.csv_parsing_error;
            isThereAnsweredCDR = isThereAnsweredCDR || cdr->status.isAnswered;

            J1N(status, cdrsInCompleteChain, scanKey);
        }

        continueWithChainProcessing = (!isThereCSVParsingError && chainRespectRateParams && isThereAnsweredCDR);
    }

    //
    // Extract strong chains, from the complete chain cdrsInCompleteChain
    //

    if (continueWithChainProcessing) {
        double diffTimeInChain = difftime(maxCalldateInChain, minimumCalldateInChain);
        itc_update_stats_maxDifftimeInSecondsInTheSameChain(diffTimeInChain, minimumCalldateInChain);

        /**
         * A Set of CDRSet, for collecting all solutions, before sending to CLIPS.
         * In this way common code on a big data structure is executed in batch mode,
         * using cache and RAM in better way.
         */
        CDRSetOfSet allStrongChains = NULL;

        int isThereStrongChain;
        do {

            CDRSet cdrsInStrongChain = NULL;
            KeySet keysInStrongChain = NULL;

            isThereStrongChain = merge_parametricMergePass(& completeChainCDRS_indexed, 0, NULL , NULL, & cdrsInStrongChain, & keysInStrongChain);
            if (isThereStrongChain) {
                // Store the result, for later processing.
                J1S(ignore, allStrongChains, (Word_t) cdrsInStrongChain);

                // keys are not used anymore
                merge_freeKeySet(& keysInStrongChain);
            }

        } while(isThereStrongChain);

        // Create a (pseudo) strong chain for each isolated CDR
        Word_t scanKey = 0;
        J1F(isThereStrongChain, isolatedCDRS, scanKey);
        while(isThereStrongChain) {
            ITCSourceCDR * cdr = (ITCSourceCDR *) scanKey;

            CDRSet chain = NULL;
            merge_addInCDRSet(&chain, cdr);
            J1S(ignore, allStrongChains, (Word_t) chain);

            J1N(isThereStrongChain, isolatedCDRS, scanKey);
        }
        merge_freeCDRSet(& isolatedCDRS);

#if ENABLE_DEBUG_MODE == 1

        // NOTE: elements are extracted from completeChainCDRS_indexed at each pass, until completition.
        assert(merge_isEmptyKeySet(completeChainCDRS_indexed));

        // all CDRS in complete chain must be put in the strong chains.
        assert(merge_testForCompleteness(cdrsInCompleteChain, allStrongChains));
#endif
        //
        // Send strong chains to CLIPS.
        //

        // Tell CLIPS to not release RAM.
        Reset();
        IncrementGCLocks();

        if (displayCLIPSDebugInfo) {
            int isOk = Watch("all");
            assert(isOk);
       }

        // Set the rating timeframe.
        sendRateParamsToCLIPS();
        merge_setGlobalVarWithCallDate("minimum-calldate-of-the-chain", minimumCalldateInChain);

        // Send every strong chain to CLIPS.
        int strongChainId = 0;
        int isOk;
        int countCDRSInCompleteChain = 0;

        scanKey = 0;
        J1F(isThereChain, allStrongChains, scanKey);
        while(isThereChain) {
            CDRSet strongChain = (CDRSet) scanKey;

            Word_t chainLenght;
            J1C(chainLenght, strongChain, 0, -1);
            strongChainId++;

            ChainDirection expectedChainDirection = merge_classifyChainDirectionUsingSpecialCDRs(strongChain);

            // Send the generic chain info.
            merge_sendStrongChainToCLIPS(strongChainId, chainLenght);
            merge_exportStrongChainIsGood(strongChainId);

            countCDRSInCompleteChain += chainLenght;

            int expectInternalDirection = 1;

            // Send info about expected directions.
            if (expectedChainDirection.outgoing) {
                expectInternalDirection  = 0;
                merge_exportExpectedDirectionToCLIPS(strongChainId, "outgoing");
            }

            if (expectedChainDirection.incoming) {
                expectInternalDirection = 0;
                merge_exportExpectedDirectionToCLIPS(strongChainId, "incoming");
            }

            if (expectInternalDirection) {
                // NOTE: in this way set a direction also for ignored, and not answered calls.
                // These calls will be processed as calls to ignore on the CLIPS side, and no errors
                // will be generated. In any case the CLIPS rules expect a direction for the call,
                // so an internal direction is sent.

                merge_exportExpectedDirectionToCLIPS(strongChainId, "internal");
            }

            // Send the strong chain to CLIPS

            int isThereCDR = 0;
            Word_t scanKey2 = 0;
            J1F(isThereCDR, strongChain, scanKey2);
            while(isThereCDR) {
                ITCSourceCDR * cdr = (ITCSourceCDR * ) scanKey2;

#if ENABLE_DEBUG_MODE == 1
                // DEV NOTE: remove comments for showing were the application is halting
                // debug_count_cdrs++;
                // printf("%i / %i) ", debug_count_cdrs, countCDRSInCompleteChain);
                // merge_printQuickDebugInfo(cdr);
#endif
                merge_sendCDRToCLIPS(cdr, strongChainId);
                J1N(isThereCDR, strongChain, scanKey2);
            }

            // NOTE: do not release ITCSourceCDR because they can be used also in the debug section,
            // and in any case they are allocated from the beginning, and they will be released at
            // the end of computation.
            merge_freeCDRSet(& strongChain);

            // the strongChain is deleted, but its pointer is still in allStrongChain.
            // Then go to next pointer, discarding deleted pointer.
            J1N(isThereChain, allStrongChains, scanKey);
        }

        // All the content of this set was deleted, now delete also the set.
        merge_freeCDRSet(& allStrongChains);

        int maxRulesToCall = 10000;
        int calledRules = Run(maxRulesToCall);

        if (displayCLIPSDebugInfo) {
            int isOk = Unwatch("all");
            assert(isOk);
        }

        // detect infinite loops
        if (calledRules > (maxRulesToCall - 10)) {
            fprintf(stderr, "Infinite loops in CLIPS rules.");
            exit(2);
        }

        //
        // Manage STATS about processed CDRS.
        //
        // CLIPS already write on the LOG file the CDRS with errors.
        // The STATS are only needed for debugging porpouse.
        //

        if (produce_debug_info) {
            // if the call does no produce error, and result it is ignored.
            // If after processing this var is again to 1, then the CDR is not managed
            // from the debug info, and error stats are not reliable.
            // Set to 0 when something is generated.
            int is_ignored_call = 1;

            // Retrieve result-cdr and update stats about good directions, and directions with errors.
            void * fact = NULL;
            fact = GetNextFactInTemplate(clips_resultCDR_template, NULL);
            while (fact != NULL) {
                is_ignored_call = 0;

                DATA_OBJECT value;

                isOk = GetFactSlot(fact, "is-error", &value);
                assert(isOk);
                int isError = (strcmp(DOToString(value), "TRUE") == 0);

                isOk = GetFactSlot(fact, "destination-type", &value);
                assert(isOk);
                CDR_direction direction = merge_fromDirectionSymbol(DOToString(value));

                if (!isError) {
                    isOk = GetFactSlot(fact, "rule-id", &value);
                    assert(isOk);
                    long ruleId = DOToLong(value);

                    update_itc_calls_direction_stats(cdrsInCompleteChain, direction);
                    itc_update_good_rule_application_stats(cdrsInCompleteChain, ruleId);

                } else {
                    // NOTE: all missing directions generate a result-cdr with an error,
                    // so no info is lost.
                    // In next phases a more specific error is retrieved.

                    update_itc_count_direction_of_calls_with_error_stats(direction);
                }

                // continue scanning facts
                fact = GetNextFactInTemplate(clips_resultCDR_template, fact);
            }

            /**
             * @brief processOtherErrors used for stopping the processing of less signifactive errors
             */
            int reportOtherErrors = 1;

            fact = GetNextFactInTemplate(clips_conflictingRules_template, NULL);
            while (fact != NULL) {
                is_ignored_call = 0;

                DATA_OBJECT value;

                isOk = GetFactSlot(fact, "rule1-id", &value);
                assert(isOk);
                int rule1 = DOToInteger(value);

                isOk = GetFactSlot(fact, "rule2-id", &value);
                assert(isOk);
                int rule2 = DOToInteger(value);

                itc_signal_rule_conflicts(cdrsInCompleteChain, rule1, rule2);

                // continue scanning facts
                fact = GetNextFactInTemplate(clips_conflictingRules_template, fact);
                reportOtherErrors = 0;
            }

            if (reportOtherErrors) {
                fact = GetNextFactInTemplate(clips_ambigousCdrForRule_template, NULL);
                while (fact != NULL) {
                    is_ignored_call = 0;

                    DATA_OBJECT value;

                    isOk = GetFactSlot(fact, "rule-id", &value);
                    assert(isOk);
                    int rule = DOToInteger(value);

                    itc_signal_rule_conflicts(cdrsInCompleteChain, rule, rule);

                    // continue scanning facts
                    fact = GetNextFactInTemplate(clips_ambigousCdrForRule_template, fact);
                    reportOtherErrors = 0;
                }
            }


            if (reportOtherErrors) {
                fact = GetNextFactInTemplate(clips_ruleDoNotUseCompleteChain_template, NULL);
                while (fact != NULL) {
                    is_ignored_call = 0;

                    DATA_OBJECT value;

                    isOk = GetFactSlot(fact, "rule-id", &value);
                    assert(isOk);
                    int rule = DOToInteger(value);

                    itc_signal_ruleDoNotUseCompleteChain(cdrsInCompleteChain, rule);

                    // continue scanning facts
                    fact = GetNextFactInTemplate(clips_ruleDoNotUseCompleteChain_template, fact);
                    reportOtherErrors = 0;
                }
            }


            if (reportOtherErrors) {
                fact = GetNextFactInTemplate(clips_missingDirection_template, NULL);
                while (fact != NULL) {
                    is_ignored_call = 0;

                    DATA_OBJECT value;

                    isOk = GetFactSlot(fact, "direction", &value);
                    assert(isOk);
                    CDR_direction direction = merge_fromDirectionSymbol(DOToString(value));

                    isOk = GetFactSlot(fact, "rule-id", &value);
                    assert(isOk);
                    int rule = DOToInteger(value);

                    itc_signal_missing_direction(cdrsInCompleteChain, direction, rule);

                    // continue scanning facts
                    fact = GetNextFactInTemplate(clips_missingDirection_template, fact);
                    reportOtherErrors = 0;
                }
            }


            if (reportOtherErrors) {
                fact = GetNextFactInTemplate(clips_unprocessedChain_template, NULL);
                while (fact != NULL) {
                    is_ignored_call = 0;

                    DATA_OBJECT value;

                    isOk = GetFactSlot(fact, "direction", &value);
                    assert(isOk);
                    CDR_direction direction = merge_fromDirectionSymbol(DOToString(value));

                    itc_signal_missing_rule(cdrsInCompleteChain, direction);

                    // continue scanning facts
                    fact = GetNextFactInTemplate(clips_unprocessedChain_template, fact);

                    reportOtherErrors = 0;
                }
            }

            if (reportOtherErrors) {
                fact = GetNextFactInTemplate(clips_cdrCanNotBeBillsec_template, NULL);
                while (fact != NULL) {
                    is_ignored_call = 0;

                    DATA_OBJECT value;

                    isOk = GetFactSlot(fact, "rule-id", &value);
                    assert(isOk);
                    int rule = DOToInteger(value);

                    itc_signal_cdr_can_not_be_billsec(cdrsInCompleteChain, rule);

                    // continue scanning facts
                    fact = GetNextFactInTemplate(clips_cdrCanNotBeBillsec_template, fact);
                    reportOtherErrors = 1;
                }

            }

            if (reportOtherErrors) {
                fact = GetNextFactInTemplate(clips_ambigousPincode_template, NULL);
                while (fact != NULL) {
                    is_ignored_call = 0;

                    itc_signal_bad_pincode(cdrsInCompleteChain);

                    // continue scanning facts
                    fact = GetNextFactInTemplate(clips_ambigousPincode_template, fact);
                    reportOtherErrors = 1;
                }
            }

            if (reportOtherErrors && is_ignored_call) {
                itc_signal_ignored_call(cdrsInCompleteChain);
            }
        }

        DecrementGCLocks();
    }

    merge_freeCDRSet(& cdrsInCompleteChain);
    merge_freeKeySet(& keysInCompleteChain);
    merge_freeIndexedCDRS(& completeChainCDRS_indexed);

    // Signal that there were a processed key.
    return 1;
}

void generateErrorForEachParsingError() {

    // Tell CLIPS to not release RAM.
    Reset();
    IncrementGCLocks();

    // Set the rating timeframe.
    sendRateParamsToCLIPS();

    //
    // Write an error ResultCDR with direction outgoing (the worst possible scenario)
    // for CDRs with an error in parsing phase.
    //

    if (itc_time_of_csv_line_with_error_index > 0) {
        Word_t index = 0;
        PWord_t pvalue;

        JLF(pvalue, itc_time_of_csv_line_with_error, index);
        while (pvalue != NULL)
        {
            CDR_direction direction = CDR_OUTGOING;
            update_itc_count_direction_of_calls_with_error_stats(direction);

            time_t * cdrTime = (time_t *) (*pvalue);
            merge_sendErrorResultCDRToCLIPS(*cdrTime);
            merge_setGlobalVarWithCallDate("minimum-calldate-of-the-chain", *cdrTime);

            int maxRulesToCall = 10000;
            int calledRules = Run(maxRulesToCall);
            // detect infinite loops
            assert(calledRules < (maxRulesToCall - 10));

            DecrementGCLocks();

            JLN(pvalue, itc_time_of_csv_line_with_error, index);
        }
    }
}

/**
 * @brief is_there_merge_on_link_key
 * @param key
 * @return 1 if there is a link between two CDRs on the specified key
 */
int itc_is_there_merge_on_link_key(Word_t key) {

    if (key == 0) {
        return 0;
    }

    PWord_t pvalue;
    JLG(pvalue, db_itcSourceCDRs, key);

    if (pvalue == NULL) {
        return 0;
    }

    Pvoid_t cdrsWithKey = (Pvoid_t) *pvalue;

    Word_t count;
    J1C(count, cdrsWithKey, 0, -1);

    if (count > 1) {
        return 1;
    } else {
        return 0;
    }
}



//////////////////
// RAGEL PARSER //
//////////////////

#include "csv_parser.c.include"

//////////////////////////
// MAIN RATING FUNCTION //
//////////////////////////

char * read_input_param(int argc, char **argv, int index) {
    if (index >= argc) {
        fprintf(stderr,"Missing parameter number %i", (index + 1));
        exit(1);
    }

    return argv[index];
}

int itc_main(int argc, char **argv, ParseCommand parseCommand, RateParams rateParams)
{
    init_params();
    itc_init_rate();

    currentRateParams = rateParams;

    initial_debug_messages = 1;
    merge_passage = 0;

    if (argc > 1) {
        int inputParam = 0;

        char * institution_domain = read_input_param(argc, argv, inputParam++);

        char * debugMode = read_input_param(argc, argv, inputParam++);

        char * debug_file_name = NULL;

        char * debug_clips_file_name = NULL;

        char * out_file_name = NULL;
        int out_file_is_new;

        int with_debug;
        if (strcmp(debugMode, "debug") == 0) {
            with_debug = 1;
            produce_debug_info = 1;
            debug_file_name = read_input_param(argc, argv, inputParam++);

            GString * tmp = g_string_new(debug_file_name);
            g_string_append(tmp, ".clips.txt");
            debug_clips_file_name = tmp->str;

            // truncate the file
            FILE * file = fopen(debug_clips_file_name, "w");
            if (file != NULL) {
                fclose(file);
            }

        } else if (strcmp(debugMode, "nodebug") == 0) {
            with_debug = 0;
            produce_debug_info = 0;
        } else {
            fprintf(stderr,"\nUnknon debug mode \"%s\"", debugMode);
            exit(1);
        }

        int isCallDate = 0;
        int isRate = 0;

        if (parseCommand == MERGE_CMD) {
            isRate = 1;

            if (strcmp(read_input_param(argc, argv, inputParam++), "--pincodes") != 0) {
                fprintf(stderr,"\nUnspecified --pincodes setting");
                exit(1);
            }

            char * pincodes_fileName = read_input_param(argc, argv, inputParam++);
            if (importPinCodes(pincodes_fileName) == -1) {
                fprintf(stderr,"\nError reading pin-codes on \"%s\"", pincodes_fileName);
                exit(1);
            }

            char * creationMode = read_input_param(argc, argv, inputParam++);

            if (strcmp("--create", creationMode) == 0) {
                out_file_name = read_input_param(argc, argv, inputParam++);
                out_file_is_new = 1;
            } else if (strcmp("--append", creationMode) == 0) {
                out_file_name = read_input_param(argc, argv, inputParam++);
                out_file_is_new = 0;
            } else {
                fprintf(stderr, "\nMissing --create FILE-NAME param\n");
                exit(1);
            }
        }

        if (parseCommand == CALLDATE_CMD) {
            isCallDate = 1;
        }

        DebugSourceCDR_initModule();
        itc_initStats();
        init_itc_csv_processor(institution_domain);

        char * clipsRuleOption = read_input_param(argc, argv, inputParam++);
        if (strcmp(clipsRuleOption, "--clips-rules") != 0) {
            fprintf(stderr,"\nUnspecified --clips-rules");
            exit(1);
        }
        char * clipsRuleFileName = read_input_param(argc, argv, inputParam++);

        if (isRate) {
            itc_init_rules();
            clips_init(clipsRuleFileName, out_file_name, out_file_is_new);
            itc_update_all_rules_with_0_stats();
        }

        char * fileListOption = read_input_param(argc, argv, inputParam++);
        if (strcmp(fileListOption, "--files-to-merge") != 0) {
            fprintf(stderr,"\nUnspecified --files-to-merge");
            exit(1);
        }

        char * fileListsName = read_input_param(argc, argv, inputParam++);
        FILE * fileList = fopen(fileListsName, "r");
        if (fileList == NULL) {
            fprintf(stderr,"\nCan not open %s", fileListsName);
            exit(1);
        }

        int totR = 0;
        int processFileFlag = 0;
        int line_to_read_size = 1024*10;
        char * line_to_read = (char *) malloc(sizeof(char) * (line_to_read_size + 1));
        char * file_type = NULL;
        char * file_name = NULL;

        while (fgets (line_to_read, line_to_read_size, fileList) != NULL ) {
            // remove ln or rc
            int len = strlen(line_to_read);
            if (line_to_read[len - 1] == '\n' || line_to_read[len - 1] == '\r') {
                    line_to_read[len - 1] = '\0';
            }
            len = strlen(line_to_read);
            if (line_to_read[len - 1] == '\n' || line_to_read[len - 1] == '\r') {
                line_to_read[len - 1] = '\0';
            }

            processFileFlag++;
            if (processFileFlag % 2 == 1) {
                file_type = strdup(line_to_read);
            } else {
                file_name = strdup(line_to_read);
                ParserResult_init(& current_ParserResult, file_name);

                if (strcmp("--mgw", file_type) == 0) {
                    itc_process_csv_file(parseCommand, MGW_FILE, file_name, with_debug);
                } else if (strcmp("--ipbx", file_type) == 0) {
                    itc_process_csv_file(parseCommand, IPBX_FILE, file_name, with_debug);
                }  else if (strcmp("--sbc", file_type) == 0) {
                    itc_process_csv_file(parseCommand, SBC_FILE, file_name, with_debug);
                } else {
                    fprintf(stderr, "\nUnknown file type %s", file_type);
                    exit(1);
                }

                totR += current_ParserResult.tot_lines;

                if (isCallDate) {
                    // write results about the CSV file

                    if (current_ParserResult.is_there_date) {
                        const int buffSize = 1024;
                        char buff[buffSize];

                        strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(& current_ParserResult.min_date));
                        printf("%s", buff);

                        strftime(buff, buffSize, "%Y-%m-%d %H:%M:%S", gmtime(& current_ParserResult.max_date));
                        printf(",%s", buff);
                    } else {
                        printf("null,null");
                    }

                    printf(",%i,%i", current_ParserResult.tot_lines, current_ParserResult.tot_lines_with_errors);

                    int i;
                    for (i = 0; i < current_ParserResult.tot_lines_with_errors; i++) {
                        printf(",%i:%i", current_ParserResult.lines_with_errors[i], current_ParserResult.fields_with_errors[i]);
                    }
                }
            }
        }
        fclose (fileList);

        if (isRate) {

            //
            // Merge Calls
            //

            if (produce_debug_info) {
              int isOk = DribbleOn(debug_clips_file_name);
              assert(isOk);

              isOk = Unwatch("all");
              assert(isOk);
            }

            int again = 1;
            while(again) {
                again = executeMergePassAndRateCall(clipsRuleFileName);
            }

            if (produce_debug_info) {
                int isOk = DribbleOff();
                assert(isOk);
            }

            generateErrorForEachParsingError();
            if (with_debug) {
                itc_stats_generate_html_summary_file(debug_file_name, debug_clips_file_name);
            }

            clips_closeEnvironment();

            // return the total number of processed lines

            printf("%i", totR);
        }
    } else {
        fprintf(stderr,"\nMissing expected voip domain.");
        exit(1);
    }

    return 0;
}

////////////////
// UNIT TESTS //
////////////////

/**
 * @brief merge_testIfItIsNotCorrupted used in debug mode
 * @param IndexedCDRS
 * @return 1 if it is all ok
 */
int merge_testIfItIsNotCorrupted(IndexedCDRS indexedCDRS) {
    int count = 0;

    Word_t index1 = 0;
    PWord_t pvalue;

    JLF(pvalue, indexedCDRS, index1);
    while(pvalue != NULL) {
        CDRSet set = (CDRSet) *pvalue;
        count += merge_testIfCDRSetIsNotCorrupted(set);
        JLN(pvalue, indexedCDRS, index1);
    }

    if (count >= 0) {
        return 1;
    } else {
        return 0;
    }
}

int merge_testIfKeySetIsNotCorrupted(CDRSet set) {
    int count = 0;

    Word_t index = 0;
    int again;

    J1F(again, set, index);
    while(again) {
        count += index;
        J1N(again, set, index);
    }

    if (count >= 0) {
        return 1;
    } else {
        return 0;
    }
}

int merge_testIfCDRSetIsNotCorrupted(CDRSet set) {
    int count = 0;

    Word_t index = 0;
    int again;

    J1F(again, set, index);
    while(again) {
        ITCSourceCDR * cdr = (ITCSourceCDR *) index;

        count += cdr->field_billsec;
        J1N(again, set, index);
    }

    if (count >= 0) {
        return 1;
    } else {
        return 0;
    }
}

KeySet failedKeySetCheck;

int merge_checkCDR(ITCSourceCDR * cdr, KeyIndex key, IndexedCDRS ind) {
    int isOk;

    if (key != 0) {
        isOk = merge_existsInCDRSet(merge_getCDRSet(ind, key), cdr);
    } else {
        isOk = 1;
    }

#if ENABLE_DEBUG_MODE == 1
    if (!isOk) {
        if (!merge_existsInKeySet(failedKeySetCheck, key)) {
            printf("\n  merge_checkCDR fail on key %lu, ", key);
            merge_addInKeySet(&failedKeySetCheck, key);
        }
    }
#endif

    return isOk;
}

int merge_checkAllCDRSet(CDRSet set, IndexedCDRS ind, KeyIndex mainIndex, int isCompleteChain) {
    int count = 0;

    Word_t index = 0;
    int again;

    int isOk = 1;

    J1F(again, set, index);
    while(again) {
        ITCSourceCDR * cdr = (ITCSourceCDR *) index;

        isOk = merge_checkCDR(cdr, cdr->field_first_half_of_unique_id_link, ind) && isOk;
        isOk = merge_checkCDR(cdr, cdr->field_last_half_of_unique_id_link, ind) && isOk;
        isOk = merge_checkCDR(cdr, cdr->field_unique_id_link, ind) && isOk;
        isOk = merge_checkCDR(cdr, cdr->field_call_id_link, ind) && isOk;
        isOk = merge_checkCDR(cdr, cdr->field_call_id2_link, ind) && isOk;
        isOk = merge_checkCDR(cdr, cdr->field_unique_id_before_chiocciola_link, ind) && isOk;

        if (isCompleteChain) {
            isOk = merge_checkCDR(cdr, cdr->field_userfield_until_point_weak_link, ind) && isOk;
        }

        Word_t key = mainIndex;
        int isThere = 0;

        if (cdr->field_first_half_of_unique_id_link == key) {
            isThere = 1;
        }

        if (cdr->field_last_half_of_unique_id_link == key) {
            isThere = 1;
        }

        if (cdr->field_unique_id_link == key) {
            isThere = 1;
        }

        if (cdr->field_call_id_link == key) {
            isThere = 1;
        }

        if (cdr->field_call_id2_link == key) {
            isThere = 1;
        }

        if (cdr->field_unique_id_before_chiocciola_link == key) {
            isThere = 1;
        }

        if (isCompleteChain) {
            if (cdr->field_userfield_until_point_weak_link == key) {
                isThere = 1;
            }
        }

        if (!isThere) {
            // the CDR was in an index were it can not stay
            isOk = 0;
        }

#if ENABLE_DEBUG_MODE == 1
        if (!isThere) {
            if (!merge_existsInKeySet(failedKeySetCheck, key)) {
                printf("\n  The index %lu contains CDRS having no key on it.", key);
                merge_addInKeySet(&failedKeySetCheck, key);
            }
        }
#endif

        J1N(again, set, index);
    }

    return isOk;

}

int merge_checkAllIndexedCDRS(IndexedCDRS ind, int isCompleteChain) {

    int count = 0;

    Word_t index1 = 0;
    PWord_t pvalue;

    failedKeySetCheck = NULL;

    int isOk = 1;

    JLF(pvalue, ind, index1);
    while(pvalue != NULL) {
        CDRSet set = (CDRSet) *pvalue;
        isOk = merge_checkAllCDRSet(set, ind, index1, isCompleteChain) && isOk;
        JLN(pvalue, ind, index1);
    }

    merge_freeKeySet(&failedKeySetCheck);

    return isOk;
}

void merge_displayInfoAboutMissingKey(CDRSet chain, KeyIndex key, int isCompleteChain) {

    int status;
    Word_t index = 0;

    Word_t count = 0;

    J1F(status, chain, index);
    while (status) {
        ITCSourceCDR * cdr = (ITCSourceCDR *) index;
        int isThere = 0;

        if (cdr->field_first_half_of_unique_id_link == key) {
            isThere = 1;
        }

        if (cdr->field_last_half_of_unique_id_link == key) {
            isThere = 1;
        }

        if (cdr->field_unique_id_link == key) {
            isThere = 1;
        }

        if (cdr->field_call_id_link == key) {
            isThere = 1;
        }

        if (cdr->field_call_id2_link == key) {
            isThere = 1;
        }

        if (cdr->field_unique_id_before_chiocciola_link == key) {
            isThere = 1;
        }

        if (isCompleteChain) {
            if (cdr->field_userfield_until_point_weak_link == key) {
                isThere = 1;
            }
        }

        if (isThere) {
            count++;
        }

        J1N(status, chain, index);
    }

    CDRSet set = merge_getCDRSet(db_itcSourceCDRs, key);
    Word_t setCount = merge_countCDRSet(set);

    if (count != setCount) {
        printf("\n  In the Asterisell calculated chain of len %lu, there are %lu CDRS with the key %lu, but in the global index thera are %lu CDRS associated to the key. ",
               merge_countCDRSet(chain),
               count,
               key,
               setCount);
    }

    index = 0;
    J1F(status, set, index);
    while (status) {
        ITCSourceCDR * cdr = (ITCSourceCDR *) index;

        if (!merge_existsInCDRSet(chain, cdr)) {
            printf("\n  The CDR with address %lu, is registered with the key %lu in db_itcSourceCDRs, but it is not in the Asterisell calculated chain.", index, key);
        }

        J1N(status, set, index);
    }
}

/**
 * @brief merge_testForClosure
 * @param cdrSet
 * @param keySet
 * @param isCompleteChain
 * @return 1 if all the keys of CDRS are in the keyset and viceversa.
 */
int merge_testForClosure(CDRSet cdrSet, KeySet keySet, int isCompleteChain) {
    int isOk = 1;

    KeySet keySet2 = NULL;

    Word_t index1 = 0;
    Word_t again1;

    int count = 0;
    J1F(again1, cdrSet, index1);
    while(again1) {
        count++;
        ITCSourceCDR * cdr = (ITCSourceCDR *) index1;

        merge_addInKeySet(&keySet2, cdr->field_first_half_of_unique_id_link);
        merge_addInKeySet(&keySet2, cdr->field_last_half_of_unique_id_link);
        merge_addInKeySet(&keySet2, cdr->field_unique_id_link);
        merge_addInKeySet(&keySet2, cdr->field_call_id_link);
        merge_addInKeySet(&keySet2, cdr->field_call_id2_link);
        merge_addInKeySet(&keySet2, cdr->field_unique_id_before_chiocciola_link);

        if (isCompleteChain) {
            merge_addInKeySet(&keySet2, cdr->field_userfield_until_point_weak_link);
        }

        J1N(again1, cdrSet, index1);
    }

    assert(count == merge_countCDRSet(cdrSet));

    // Remove the null link, because it is not a valid link.
    Word_t oldKeySet2Count = merge_countKeySet(keySet2);
    J1U(again1, keySet2, 0);
    if (again1) {
        oldKeySet2Count--;
    }
    assert(oldKeySet2Count ==  merge_countCDRSet(keySet2));

    // Compare keySet with keySet2
    index1 = 0;
    J1F(again1, keySet, index1);
    while(again1) {

        int wasPresent;
        J1U(wasPresent, keySet2, index1);

        if (!wasPresent) {
            isOk = 0;
            printf("\n   !!! the index %lu was in the Asterisell calculated closure of CDRS, but it was not present on the CDRS chain keyset of len %lu, and cdrset of len %lu.",
                   index1,
                   merge_countKeySet(keySet),
                   merge_countCDRSet(cdrSet));

            merge_displayInfoAboutMissingKey(cdrSet, index1, isCompleteChain);
        }

        J1N(again1, keySet, index1);
    }

    // Compare keySet with keySet2
    index1 = 0;
    J1F(again1, keySet2, index1);
    while(again1) {
        isOk = 0;

        printf("\n   !!! the index %lu was in the CDRS chain, but it was not present on the Asterisell calculated keyset of len %lu, and cdrset of len %lu.",
               index1,
               merge_countKeySet(keySet),
               merge_countCDRSet(cdrSet));

        merge_displayInfoAboutMissingKey(cdrSet, index1, isCompleteChain);

        J1N(again1, keySet2, index1);
    }

    if (!isOk) {
        assert(merge_testIfItIsNotCorrupted(db_itcSourceCDRs));
    }

    return isOk;
}

/**
 * @param chain
 * @param chains
 * @return 1 if the chains contains all the elements of chain, without repetitions
 */
int merge_testForCompleteness(CDRSet chain, CDRSetOfSet chains) {

    CDRSet cloneChain = NULL;
    merge_addKeySet(chain, &cloneChain);

    Word_t index = 0;
    int again;
    J1F(again, chains, index);
    while(again) {
        CDRSet innerChain = (CDRSet) index;

        Word_t index2 = 0;
        int again2;
        J1F(again2, innerChain, index2);

        while(again2) {
            ITCSourceCDR * cdr = (ITCSourceCDR *) index2;

            int exists;
            J1U(exists, cloneChain, (Word_t) cdr);
            if (!exists) {
                // all the CDRS must appear exactly only one time in chain
                return 0;
            }

            J1N(again2, innerChain, index2);
        }

        J1N(again, chains, index);
    }

    if (merge_countCDRSet(cloneChain) > 0) {
        // all the CDRS should be removed
        return 0;
    }


    // in this case it is all ok
    return 1;
}

/**
 * @brief merge_tests
 * @return the number of failing tests, 0 if it is all ok.
 */
int merge_tests() {

    int r = 0;

    KeySet set1 = NULL;
    Word_t keyIndex = 0;
    int status;

    ITCSourceCDR * cdr1 = (ITCSourceCDR *) malloc(sizeof(ITCSourceCDR));
    ITCSourceCDR * cdr2 = (ITCSourceCDR *) malloc(sizeof(ITCSourceCDR));
    ITCSourceCDR * cdr3 = (ITCSourceCDR *) malloc(sizeof(ITCSourceCDR));

    cdr1->field_billsec = 1;
    cdr2->field_billsec = 2;
    cdr3->field_billsec = 3;

    merge_addInCDRSet(&set1, cdr1);
    merge_addInCDRSet(&set1, cdr2);

    r += testIfFails(merge_testIfKeySetIsNotCorrupted(set1), "Test set corruption 1");
    r += testIfFails(merge_testIfKeySetIsNotCorrupted(set1), "Test set corruption 2");

    r += testIfFails((merge_countCDRSet(set1) == 2), "Fail merge test 1");
    r += testIfFails((merge_existsInCDRSet(set1, cdr1)), "Fail merge exists test 1");
    r += testIfFails((merge_existsInCDRSet(set1, cdr2)), "Fail merge exists test 2");
    r += testIfFails(!(merge_existsInCDRSet(set1, cdr3)), "Fail merge exists test 3");

    merge_freeKeySet(& set1);

    r += testIfFails((merge_countCDRSet(set1) == 0), "Fail merge test 2");

    IndexedCDRS indexSet = NULL;
    merge_addInIndexedCDRS(&indexSet, 1, cdr1);
    merge_addInIndexedCDRS(&indexSet, 1, cdr2);
    merge_addInIndexedCDRS(&indexSet, 1, cdr3);
    merge_addInIndexedCDRS(&indexSet, 2, cdr2);
    merge_addInIndexedCDRS(&indexSet, 2, cdr3);
    merge_addInIndexedCDRS(&indexSet, 3, cdr3);

    r += testIfFails(merge_testIfItIsNotCorrupted(indexSet), "Test indexSet corruption 1");

    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(indexSet, 1), cdr1), "Fail index merge test 1");
    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(indexSet, 1), cdr2), "Fail index merge test 2");
    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(indexSet, 1), cdr3), "Fail index merge test 3");
    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(indexSet, 2), cdr2), "Fail index merge test 4");
    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(indexSet, 3), cdr3), "Fail index merge test 5");
    r += testIfFails(!merge_existsInCDRSet(merge_getCDRSet(indexSet, 3), cdr2), "Fail index merge test 6");

    KeySet keySet = NULL;
    merge_addInKeySet(&keySet, 1);
    merge_addInKeySet(&keySet, 3);
    merge_removeAndFreeFromIndexedSet(& indexSet, keySet);

    r += testIfFails((merge_getCDRSet(indexSet, 3) == NULL), "Fail index merge test 7");
    r += testIfFails((merge_getCDRSet(indexSet, 1) == NULL), "Fail index merge test 8");
    r += testIfFails((merge_getCDRSet(indexSet, 2) != NULL), "Fail index merge test 9");
    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(indexSet, 2), cdr2), "Fail index merge test 10");

    r += testIfFails(merge_testIfItIsNotCorrupted(indexSet), "Test indexSet corruption 3");

    // Test dictionaries

    init_itc_csv_processor("nothing");

    newCurrentITCSourceCDR();
    cdr1 = current_itcSourceCDR;
    Word_t key1 = addStringInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup("1");
    Word_t key2 = addStringInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup("2");

    newCurrentITCSourceCDR();
    cdr2 = current_itcSourceCDR;
    Word_t key3 = addStringInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup("3");
    Word_t key4 = addStringInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup("4");

    newCurrentITCSourceCDR();
    cdr3 = current_itcSourceCDR;
    Word_t key5 = addStringInDictionaryAndRegisterCurrentITCSourceCDRForFastLookup("4");

    r += testIfFails(key5 == key4, "Dictionary 1");
    r += testIfFails(key5 != key1, "Dictionary 2");
    r += testIfFails(key1 != key2, "Dictionary 3");

    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(db_itcSourceCDRs, key5), cdr3), "Dictionary 4");
    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(db_itcSourceCDRs, key4), cdr3), "Dictionary 5");
    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(db_itcSourceCDRs, key5), cdr2), "Dictionary 6");
    r += testIfFails(merge_existsInCDRSet(merge_getCDRSet(db_itcSourceCDRs, key1), cdr1), "Dictionary 6");

    r += testIfFails((merge_countCDRSet(merge_getCDRSet(db_itcSourceCDRs, key1)) == 1), "Dictionary 7");
    r += testIfFails((merge_countCDRSet(merge_getCDRSet(db_itcSourceCDRs, key5)) == 2), "Dictionary 8");

    // Test erasing

    IndexedCDRS ind1 = NULL;
    merge_freeIndexedCDRS(&ind1);
    r += testIfFails((ind1 == NULL), "Delete");
    r += testIfFails(merge_testIfItIsNotCorrupted(ind1), "corruption 1");

    IndexedCDRS ind2 = NULL;
    r += testIfFails((ind2 == NULL), "Delete 1");
    merge_addInIndexedCDRS(&ind2, 1, cdr1);
    r += testIfFails(merge_testIfItIsNotCorrupted(ind2), "corruption 2");
    r += testIfFails((ind2 != NULL), "Delete 2");
    merge_freeIndexedCDRS(&ind2);
    r += testIfFails(merge_isEmptyKeySet(ind2), "Delete 3");
    r += testIfFails((ind2 == NULL), "Delete 4");
    r += testIfFails(merge_testIfItIsNotCorrupted(ind2), "corruption 3");
    merge_freeIndexedCDRS(&ind2);
    r += testIfFails(merge_isEmptyKeySet(ind2), "Delete 5");
    r += testIfFails((ind2 == NULL), "Delete 6");
    r += testIfFails(merge_testIfItIsNotCorrupted(ind2), "corruption 4");

    return r;
}

