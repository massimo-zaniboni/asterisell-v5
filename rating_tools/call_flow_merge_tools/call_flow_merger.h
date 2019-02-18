/*  SPDX-License-Identifier: GPL-3.0-or-later */
/* Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com> */

#ifndef __CALL_FLOW_MERGER_17753__
#define __CALL_FLOW_MERGER_17753__

#include <time.h>
#include <glib.h>
#include <stdio.h>

#define JUDYERROR(CallerFile, CallerLine, JudyFunc, JudyErrno, JudyErrID) \
       {                                                                         \
               (void) fprintf(stderr, "File ’%s’, line %d: %s(), "               \
                   "JU_ERRNO_* == %d, ID == %d\n",                               \
                   CallerFile, CallerLine,                                       \
                   JudyFunc, JudyErrno, JudyErrID);                              \
               exit(2);                                                          \
       }


void * xmalloc (size_t n);

// NOTE: it is important to define the previous macro before this include
#include <Judy.h>

typedef enum {
    MERGE_CMD = 0,
    CALLDATE_CMD = 1
} ParseCommand;

#define MAX_NUMBER_OF_CSV_LINES_WITH_ERRORS_SIGNALED 50

typedef char * StringKey;

typedef Word_t StringDictIndex;

/**
 * Contain the result of the parsing of a CSV file.
 */
typedef struct {

    int is_there_date;

    time_t max_date;
    time_t min_date;

    char * file_name;

    int tot_lines;
    int tot_lines_with_errors;

    int lines_with_errors[MAX_NUMBER_OF_CSV_LINES_WITH_ERRORS_SIGNALED];
    int fields_with_errors[MAX_NUMBER_OF_CSV_LINES_WITH_ERRORS_SIGNALED];

} ParserResult;

void ParserResult_init(ParserResult * r, char * file_name);

extern ParserResult current_ParserResult;

/**
 * IMPORTANT: the content of these fields must be mantained in synchro with DestinationType fields
 * @param d
 * @return a name for the call direction.
 */
typedef enum {
    CDR_UNPROCESSED = 0,
    CDR_INCOMING = 1,
    CDR_OUTGOING = 2,
    CDR_INTERNAL = 3,
    CDR_IGNORED = 4,
    CDR_ERROR = 5
} CDR_direction;

const char * CDR_direction_description(CDR_direction d);

/**
 * Params about rate method to use.
 */
typedef struct {

    time_t fromDate;

    time_t toDate;

    int thereIsToDate;

    int mergeWindowInSeconds;

    time_t derived_acceptAfterDate;
    time_t derived_acceptBeforeDate;

} RateParams;

extern RateParams currentRateParams;

extern int produce_debug_info;

typedef enum { UNSPECIFIED_FILE = 0, SBC_FILE = 1, MGW_FILE = 2, IPBX_FILE = 3 } ITCFileType;

char * itcFileTypeName(ITCFileType t);

int itc_main(int argc, char **argv, ParseCommand parseCommand, RateParams rateParams);


/**
 * An Asterisell call after the merge, and not yet rated.
 */
typedef struct {

    // IMPORTANT: every time a new field is added, remember to update ResultCDR_empty settings.

    time_t calldate;
    CDR_direction destination_type;
    int is_redirect;
    int billsec;
    char * internal_extension;
    char * external_telephone_number;
    char * ported_operator_prefix;
    char * vendor_domain;
    int is_error;

} ResultCDR;


typedef struct {
    // IMPORTANT: if you add some field, make sure to update
    // * the init newCurrentITCSourceCDR(),
    // * setting initial values,
    // * call_flow_merge.c
    // * debug_info.c
    // * CProcedureGenerator with the list of supported status
    // * CProcedureGnerator in the parsing section of status

    /**
     * NOTE: insert also not answered SourceCDR, because it is important merging
     * with other CDR, for setting the entire chain as not answered.
     */
    unsigned int isAnswered : 1;

    // TODO I'm not sure this is needed
    unsigned int csv_parsing_error : 1;

    /**
     * @brief force_debug 1 if the SourceCDR must be inserted in the debug-info in any case,
     * also if it does not respect other criteria. Used for forcing the debug of certain calls.
     */
    unsigned int force_debug : 1;

    /**
     * @brief can_be_billsec 1 if the CDR is related to an external call,
     * and can be used for billsec.
     */
    unsigned int can_be_billsec : 1;

    unsigned int calldestregid_isExternalVoIPVendor : 1;
    unsigned int callsrcregid_isExternalVoIPVendor : 1;
    unsigned int lastData_isBackupChannel : 1;
    unsigned int dstChannel_isEnumChannel : 1;
    unsigned int dstChannel_isDAHDI : 1;
    unsigned int srcChannel_isSIP : 1;
    unsigned int dstChannel_isSIP : 1;
    unsigned int dstChannel_isSIPAndSBC : 1;
    unsigned int dcontext_isInternal : 1;
    unsigned int dcontext_isFaxTpa : 1;
    unsigned int dcontext_isFromTrunk : 1;
    unsigned int dcontext_isFax : 1;
    unsigned int dcontext_isExtLocal : 1;
    unsigned int dcontext_isExtGroup : 1;
    unsigned int dcontext_isToPBX : 1;
    unsigned int dcontext_isFromDirectDDI : 1;
    unsigned int dcontext_isFaxBackup : 1;
    unsigned int lastdata_isExtensionAndDomain : 1;
    unsigned int lastdata_isInternalChannel : 1;
    unsigned int lastdata_isBackupChannel : 1;
    unsigned int lastdata_isSIP : 1;
    unsigned int srcChannel_isDAHDI : 1;
    unsigned int lastapp_isResetCDR : 1;
    unsigned int lastapp_isDial : 1;
    unsigned int dstChannel_isLocal : 1;
    unsigned int calldestregid_isIPBX : 1;
    unsigned int dstChannel_isSIPWithExtension : 1;
    unsigned int srcChannel_isSIPWithExtension : 1;
    unsigned int dstChannel_isInstitutionDomain : 1;
    unsigned int srcChannel_isInstitutionDomain : 1;
    unsigned int dst_isFax : 1;
    unsigned int callsrcregid_isMGW : 1;
    unsigned int calldstregid_isMG : 1;
    unsigned int dstchannel_isEmpty : 1;
    unsigned int callsrcregid_isNull : 1;
    unsigned int calldstregid_isNull : 1;
    unsigned int dcontext_isFaxToBackup : 1;

} ITCSourceCDRStatus;

typedef enum {

    ITC_VENDOR_UNDEF = 0,
    ITC_VENDOR_PTPRIME = 1,
    ITC_VENDOR_TMN = 2,
    ITC_VENDOR_VODAFONE = 3,
    ITC_VENDOR_SONAECOM = 4,
    ITC_VENDOR_GLOBAL_PHONE = 5

} ITCVendor;

/**
 * ITCSourceCDR
 *
 * Requirements:
 * - fast to process
 * - use few RAM, because a lot of these must be allocated in RAM during call-flow merge
 */
typedef struct {

    //
    // Link Fields
    //
    // IMPORTANT: if you add fields here, remember to:
    // - update init code
    // - update the parser setting the value
    // - update the call flow merging code
    // - update the code setting ITCSourceCDR_empty
    //

    Word_t field_userfield_until_point_weak_link;
    Word_t field_unique_id_link;
    Word_t field_call_id_link;
    Word_t field_call_id2_link;
    Word_t field_unique_id_before_chiocciola_link;
    Word_t field_last_half_of_unique_id_link;
    Word_t field_first_half_of_unique_id_link;

    //
    // Data Fields
    //

    ITCFileType file_type;

    int field_billsec;
    time_t field_calldate;

    ITCSourceCDRStatus status;

    StringKey field_src;
    StringKey field_dst;
    StringKey field_accountcode;
    StringKey field_lastdata;

    StringKey field_dst_channel_get_extension;
    StringKey field_src_channel_get_extension;

    ITCVendor destregid_vendorId;
    ITCVendor sourceregid_vendorId;

} ITCSourceCDR;


void writeResultCDR(ResultCDR * cdr);

typedef struct {

    time_t minCallDate;

    time_t maxCallDate;

    int totalLines;

    int linesWithErrors;

    int importedSourceCDRs;

} CSVFileMetaInfo;


/**
 * @brief CSVFileHeader
 * Map an Integer from 0 to the number of fields,
 * to the char * with the name of the field.
 */
typedef Pvoid_t CSVFileHeader;

/**
 * @brief CSVFileValues
 * Map an Integer from 0 to the number of fields,
 * to a StringKey with its value.
 */
typedef Pvoid_t CSVFileRowValues;

/**
 * Contains additional debug info,
 * associated to a ITCSourceCDR.
 *
 * These info are generated only in debug mode.
 *
 */
typedef struct {

    StringKey fileName;
    int lineNr;
    CSVFileHeader * header;
    CSVFileRowValues values;

} DebugSourceCDR;

void DebugSourceCDR_initModule();

void init_rate();

void RateParams_completeDerivedFields(RateParams * p);

/**
 * @return 1 if the date respect rate params.
 */
int date_respectRateParams(time_t d);


/**
 * @brief current_itcSourceCDR current CDR under parsing.
 */
extern ITCSourceCDR * current_itcSourceCDR;

/**
 * @brief html_escape escape according HTML rules the content of the text
 * @param text the text to convert
 * @return the converted GString
 */
GString * html_escape(char * text);

/**
 * @brief maybeEmpty
 * @param in
 * @return an html escaped GString
 */
GString * maybeEmpty(char * in);

/**
 * @brief maybeEmpty
 * @param in
 * @return an html escaped C string
 */
char * maybeEmptyStr(char * in);

/**
 * @brief safeStr
 * @param str NULL or GString
 * @return a not NULL C string
 */
char * safeStr(GString * str);


typedef Word_t KeyIndex;

/**
 * @brief CDRSet a Judy1 set of CDR references.
 */
typedef Pvoid_t CDRSet;

/**
 * @brief GroupedCDRS CDRS indexed and grouped by KeyIndex.
 */
typedef Pvoid_t IndexedCDRS;

/**
 * @brief CDRSetOfSet a Judy1 set of CDRSet.
 */
typedef Pvoid_t CDRSetOfSet;

/**
 * @brief KeySet a Judy1 set of Keys.
 */
typedef Pvoid_t KeySet;

extern IndexedCDRS db_itcSourceCDRs;

inline void merge_addKeySet(KeySet source, KeySet *dest) {
    int status;
    KeyIndex key = 0;

    J1F(status, source, key);
    while(status == 1) {
        int ignore;
        J1S(ignore, *dest, key);
        J1N(status, source, key);
    }
}

inline void merge_freeKeySet(KeySet * set) {
    int ignore;
    J1FA(ignore, * set);
}

inline void merge_freeCDRSet(CDRSet * set) {
    int ignore;
    J1FA(ignore, * set);
}

inline void merge_freeIndexedCDRS(IndexedCDRS *ind) {
    Word_t ignore;

    JLFA(ignore, *ind);
}

inline int merge_isEmptyKeySet(KeySet set) {
    Word_t c;
    J1C(c, set, 0, -1);

    return (c == 0);
}

inline Word_t merge_countCDRSet(CDRSet set) {
    Word_t c;
    J1C(c, set, 0, -1);

    return (int) c;
}

inline Word_t merge_countKeySet(KeySet set) {
    Word_t c;
    J1C(c, set, 0, -1);

    return (int) c;
}

inline Word_t merge_countIndexedSet(IndexedCDRS ind) {
    Word_t c;
    JLC(c, ind, 0, -1);
    return c;
}


/**
 * @brief merge_existsInKeySet
 * @param set
 * @param key
 * @return 1 if the element exists
 */
inline int merge_existsInKeySet(KeySet set, KeyIndex key) {
    int status = 0;

    J1T(status, set, key);

    return status;
}

inline int merge_existsInCDRSet(CDRSet set, ITCSourceCDR * cdr) {
    return merge_existsInKeySet((KeySet) set, (KeyIndex) cdr);
}

/**
 * @brief merge_getCDRSet
 * @param set
 * @param key
 * @return NULL or a CDRSet
 */
inline CDRSet merge_getCDRSet(IndexedCDRS set, KeyIndex key) {
    PWord_t  pvalue;

    JLG(pvalue, set, key);
    if (pvalue == NULL) {
        return NULL;
    } else {
        return (CDRSet) *pvalue;
    }
}

/**
 * @brief merge_getCDRSet
 * @param set
 * @param key
 * @return 1 if the value was new, 0 if it was already existing
 */
inline int merge_addInCDRSet(CDRSet * set, ITCSourceCDR * cdr) {
    int status = 0;
    J1S(status, *set, (Word_t) cdr);
    return status;
}

/**
 * @param set
 * @param key
 * @return 1 if the value was new, 0 if it was already existing
 */
inline int merge_addInKeySet(KeySet * set, KeyIndex key) {
    int status = 0;
    J1S(status, *set, key);
    return status;
}

/**
 * @brief merge_addInIndexedCDRS
 * @param indexedSet
 * @param key
 * @param cdr
 * @return 1 if the value was new in the set.
 */
inline int merge_addInIndexedCDRS(IndexedCDRS *indexedSet, KeyIndex key, ITCSourceCDR * cdr) {

    PWord_t pvalue;
    JLI(pvalue, *indexedSet, key);

    int status = 0;
    J1S(status, * (CDRSet *)  pvalue, (Word_t) cdr);

    return status;
}

inline void registerCDRLink(ITCSourceCDR * cdr, KeyIndex key) {
    if (key != 0) {
        merge_addInIndexedCDRS(&db_itcSourceCDRs, key, cdr);
    }
}

void registerCDR(ITCSourceCDR * cdr);


int itcSourceCDRs_isForceDebug(CDRSet chain);

/**
 * A chain can have multiple directions, in case of redirect.
 * Use a bit field mask for testing all of them.
 */
typedef struct {
    unsigned int outgoing : 1;
    unsigned int incoming : 1;
    unsigned int internal : 1;
    unsigned int ignored  : 1;
    unsigned int error    : 1;
    unsigned int pstn_backup : 1;
    unsigned int enum_call : 1;
    unsigned int not_answered : 1;
    unsigned int error_in_billsec : 1;
} ChainDirection;

inline void ChainDirection_reset(ChainDirection * d) {
    d->outgoing = 0;
    d->incoming = 0;
    d->internal = 0;
    d->ignored = 0;
    d->error = 0;
    d->pstn_backup = 0;
    d->enum_call = 0;
    d->not_answered = 0;
    d->error_in_billsec = 0;
}

/**
 * @brief isThereRedirect
 * @param d
 * @return 1 if there is more than one direction, and so there is a redirection.
 */
inline int ChainDirection_isThereRedirect(ChainDirection d) {
    int c = 0;
    if (d.outgoing) {
        c++;
    }
    if (d.incoming) {
        c++;
    }
    if (d.internal) {
        c++;
    }

    if (c > 1) {
        return 1;
    } else {
        return 0;
    }
}

/**
 *
 * @param d
 * @return a human readable description of a chain direction.
 */
char * ChainDirection_description(ChainDirection d);

void ChainDirection_printf(ChainDirection d);

CDR_direction chainDirection_toDestinationType(ChainDirection d);

/**
 * @param set
 * @return the directions supported from the chain, inspecting only special CDRs.
 * Write also in the ITCSourceCDR the status can_be_billsec
 */
ChainDirection merge_classifyChainDirectionUsingSpecialCDRs(CDRSet set);

/**
 * @brief is_there_merge_on_link_key
 * @param key
 * @return 1 if there is a link between two CDRs on the specified key
 */
int itc_is_there_merge_on_link_key(Word_t key);

char * itcVendor_getVendorString(ITCVendor v);

extern DebugSourceCDR * currentDebugSourceCDR;

void itc_init_rules();

extern CSVFileHeader csvFileHeader_itc_mgw;

extern CSVFileHeader csvFileHeader_itc_sbc;

/**
 * @brief merge_tests
 * @return the number of failing tests, 0 if it is all ok.
 */
int merge_tests();

/**
 * @brief testIfFails
 * @param t
 * @param d
 * @return 1 if the test fail, 0 if the test is correct.
 */
int testIfFails(int t, char *d);

/**
 * @brief merge_testIfItIsNotCorrupted used in debug mode
 * @param IndexedCDRS
 * @return 1 if it is all ok
 */
int merge_testIfItIsNotCorrupted(IndexedCDRS IndexedCDRS);

int merge_testForClosure(CDRSet cdrSet, KeySet keySet, int isCompleteChain);

int merge_testIfKeySetIsNotCorrupted(CDRSet set);

int merge_testIfCDRSetIsNotCorrupted(CDRSet set);

void merge_displayInfoAboutMissingKey(CDRSet chain, KeyIndex key, int isCompleteChain);

int merge_testForCompleteness(CDRSet chain, CDRSetOfSet chains);

#endif

