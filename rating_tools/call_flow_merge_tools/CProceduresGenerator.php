<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

//
// This code read a specification file with call flow merge rules and generate:
// * a C file with rule descriptions;

include_once('yaml/sfYaml.php');

main($argc, $argv);
exit(0);

/**
 * Generate CLIPS rules, and C data, starting from an high level rule specification file, in YAML format.
 *
 * In this way all the low level details for processing calls are inside the logic of this class,
 * and the rule file contains only high level rules.
 */
class CProceduresGenerator
{

    // IMPORTANT: the value of this const must be kept in synchro with call_flow_merger.c and fabric script.
    const rule_result_file_name = "call_flow_merge_rules.bsave";
    const rule_source_file_name = "call_flow_merge_rules.lisp";
    const clips_compiling_script = "install_rules.lisp";
    const rule_descriptions_to_c = "call_flow_merge_procedures.c.include";

    //////////////
    // CONSTANT //
    //////////////
    // IMPORTANT: the values of these constants must be the same of "apps/asterisell/lib/DestinationType.php"

    const unprocessed = 0;

    const incoming = 1;

    const outgoing = 2;

    const internal = 3;

    const ignored = 4;

    const error = 5;

    /**
     * IMPORTANT: the values of these constants must be the same of
     * "apps/asterisell/lib/provider_specific/itcenter_fccn/admin/ITCInitVoIPChannels.php"
     *
     * How Vendor and Channels are managed:
     * - the rule specify "vendor" and "channel" values, they can be "implicit"
     * - the rule compiler match for any valid pair "vendor" and "channel" a "vendor_domain" string sent to Asterisell rating engine
     * - the Asterisell rate engine use the "vendor_domain" table for associating the "channel" and "vendor" to the call
     * - only valid pairs are processed from the compiler, otherwise an error is signaled
     * - the code on Asterisell rate engine, and rule compiler, must be kept in synchro
     *
     * @param UserRule $userRule
     * @param string|null $vendorValue
     * @param string|null $channelValue
     * @return Expression|null the Asterisell vendor_domain to use for rating the call,
     * or a piece of C code returning this value.
     */
    protected function convertVendorAndChannelSpecification($userRule, $vendorValue, $channelValue)
    {

        if ($this->isEmptyOrNull(trim($vendorValue))) {
            $vendorValue = 'implicit';
        }
        if ($this->isEmptyOrNull(trim($channelValue))) {
            $channelValue = 'implicit';
        }

        //
        // Test each possible case, returning a value.
        //

        // Explicit vendor case.
        // Note: the channel is usually linked to the vendor name
        // on the Asterisell side.

        $m = array();

        if (preg_match('/^([\w\d_]+)[.]calldestregid$/i', $vendorValue, $m) && $channelValue == 'implicit') {
            // the C-code put in this slot the value of the vendor.

            $cdrName = $m[1];
            $fieldName = 'destregid_vendor';
            $cdr = $userRule->getCdr($cdrName);
            $varName = $userRule->getCdrFieldValueVar($cdr, $fieldName);
            $cdr->registerNeededField($fieldName);

            return new VarExpression($varName);
        }

        if (preg_match('/^([\w\d_]+)[.]callsrcregid$/i', $vendorValue, $m) && $channelValue == 'implicit') {
            // the C-code put in this slot the value of the vendor.

            $cdrName = $m[1];
            $fieldName = 'srcregid_vendor';
            $cdr = $userRule->getCdr($cdrName);
            $varName = $userRule->getCdrFieldValueVar($cdr, $fieldName);
            $cdr->registerNeededField($fieldName);

            return new VarExpression($varName);
        }

        // Explicit channels, with implicit vendor.
        // Note: usually these are used for incoming calls,
        // where the vendor is left generic, on the Asterisell side.

        if ($vendorValue == 'implicit') {
            if ($channelValue == 'free_enum') {
                return new StringExpression("free-enum");
            }

            if ($channelValue == 'sip_mobile') {
                return new StringExpression("undef-sip-mobile");
            }

            if ($channelValue == 'sip_fixed_line') {
                return new StringExpression("undef-sip-fixed-line");
            }

            if ($channelValue == 'local_network') {
                return new StringExpression("local-network");
            }
        }

        // Manage backup calls

        if ($channelValue == 'pstn_backup') {
            if ($vendorValue == 'ptprime') {
                return new StringExpression("ptprime-pstn-backup");
            }

            // up to date only ptprime manage backup calls
        }


        // There is no valid case

        return null;
    }

    /**
     * @param string $str
     * @return bool
     */
    protected function isEmptyOrNull($str)
    {
        if (is_null($str)) {
            return TRUE;
        } else {
            if (strlen($str) == 0) {
                return TRUE;
            } else {
                return FALSE;
            }
        }
    }

    ////////////
    // PARAMS //
    ////////////

    /**
     * @var string
     */
    protected $inFileName;

    ///////////////////
    // PARSING STATE //
    ///////////////////

    /**
     * @var string
     */
    protected $currentRuleName = '<init>';

    /**
     * @param string $msg
     * @return Exception
     */
    protected function signalError($msg)
    {
        return new Exception("Error during parsing of rule \"{$this->currentRuleName}\", of file \"{$this->inFileName}\": $msg");
    }

    /**
     * @param string $inFileName
     */
    function __construct($inFileName)
    {
        $this->inFileName = $inFileName;
    }

    /**
     * @return void
     * @throws Exception
     */
    public function generateCode()
    {

        if (!file_exists($this->inFileName)) {
            throw($this->signalError("Can not parse YAML file \"{$this->inFileName}\"."));
        }

        try {
            $codeSpec = sfYaml::load($this->inFileName);
        } catch (Exception $e) {
            throw($this->signalError("Can not parse YAML file. " . $e->getMessage()));
        }

        $maxId = $this->addInfoToCodeSpec($codeSpec);
        $this->generateCHeaders($codeSpec, $maxId);
        $this->generateCLIPSRules($codeSpec);

        // Generate the compiling CLIPS file
        file_put_contents(self::clips_compiling_script, '
        (if (load ' . self::rule_source_file_name . ') then (bsave ' . self::rule_result_file_name . ') (exit 0) else (exit 1))'
        );

    }

    /**
     * Derive info analyzing the rule, and add it to new fields of the parsed YAML structure.
     *
     * @param array $codeSpec
     * @return int the max id generated..
     */
    protected function addInfoToCodeSpec(&$codeSpec)
    {
        $expandedRules = & $codeSpec['rules'];

        // rules start with index 1
        $i = 0;
        foreach ($expandedRules as & $rule) {
            $i++;
            $rule['id'] = $i;
        }

        return $i;
    }

    /**
     * Generate C Header files, and associate a unique id to each rule.
     *
     * @param array $codeSpec
     * @param int $maxId
     * @return void
     * @throws Exception
     */
    protected function generateCHeaders(&$codeSpec, $maxId)
    {

        $fh = fopen(self::rule_descriptions_to_c, 'w');

        if ($fh == false) {
            throw($this->signalError("Can not write to output file \"" . self::rule_descriptions_to_c . "\""));
        }

        $header = '';

        //
        // Add rules descriptions that are used from the C runtime mainly during debug file generation.
        //

        $header .= "\nvoid itc_init_rules() {\n  ";

        $expandedRules = $codeSpec['rules'];

        $nrOfRules = count($expandedRules) + 3;
        $header .= "\n  itc_rules = (CallFlowMergeRuleDescription *) malloc(sizeof(CallFlowMergeRuleDescription) * $nrOfRules);\n";

        // insert system rule with index 0
        $r = "itc_rules[0]";
        $header .= "\n$r.id = 0;";
        $header .= "\n$r.yaml_specification = \"System generated CDR\";";
        $header .= "\n$r.description = \"System generated CDR\";";
        $header .= "\n$r.signature = \"\";";;
        $header .= "\n$r.name = \"system_rule\";";

        // generate user rules
        foreach ($expandedRules as $rule) {
            $i = $rule['id'];

            $r = "itc_rules[$i]";

            $header .= "\n$r.id = $i;";
            $header .= "\n$r.yaml_specification = \"" . self::convertStringToCSVAndC($this->convertRuleDescriptionToHTML(sfYaml::dump($rule))) . "\";";
            $header .= "\n$r.description = \"" . self::convertStringToCSVAndC(htmlentities($this->getParam($rule, 'description'))) . "\";";
            $header .= "\n$r.signature = \"" . self::convertStringToCSVAndC(htmlentities($this->getParam($rule, 'merge'))) . "\";";
            $header .= "\n$r.name = \"" . self::convertStringToCSVAndC(htmlentities($this->getParam($rule, 'name'))) . "\";";
        }

        // signal the end of the rules
        $i = $maxId + 1;
        $header .= "\nitc_rules[$i].id = 0;";

        $header .= "\n}\n";

        fwrite($fh, $header);

        fclose($fh);
    }

    /**
     * First represent the rules in an internal format,
     * then analyze and transform it,
     * then generate corresponding CLIPS code.
     *
     * @require first must be called addInfoToCodeSpec.
     *
     * @param array $codeSpec
     * @return void
     * @throws Exception
     */
    protected function generateCLIPSRules(&$codeSpec)
    {
        //
        // Open file and write initial code.
        //

        $fh = fopen(self::rule_source_file_name, 'w');

        if ($fh == false) {
            throw($this->signalError("Can not write to output file \"" . self::rule_source_file_name . "\""));
        }

        fwrite($fh, $this->getCLIPSGenericHeader());

        $expandedRules = $codeSpec['rules'];

        $i = 0;
        foreach ($expandedRules as & $rule) {
            $i++;
            $ruleName = $this->getParam($rule, 'name');
            $this->currentRuleName = $ruleName;

            $ruleId = $this->getParam($rule, 'id');
            if (array_key_exists('merge', $rule)) {
                $ruleTypeSpec = $this->getParam($rule, 'merge');
            } else {
                throw($this->signalError("Rule must contain \"merge\" specification."));
            }

            $ruleCDRTypes = $this->extractCDRTypesFromRuleSpec($ruleTypeSpec);
            $ruleCDRNames = $this->extractCDRNamesFromRuleSpec($ruleTypeSpec);

            if (count($ruleCDRNames) !== count($ruleCDRTypes)) {
                throw($this->signalError("Bad format of rule specification \"$ruleTypeSpec\""));
            }

            assert(count($ruleCDRNames) == count($ruleCDRTypes));

            if (count($ruleCDRNames) == 0) {
                throw($this->signalError("Bad format of rule specification \"$ruleTypeSpec\""));
            }

            assert(count($ruleCDRNames) > 0);

            $callFlowLen = count($ruleCDRNames);
            $firstCDRName = $ruleCDRNames[0];
            $lastCDRName = $ruleCDRNames[$callFlowLen - 1];

            //
            // Create UserRule specifying only the CDRs.
            //

            $userRule = new UserRule();
            $userRule->id = $ruleId;
            $userRule->name = $ruleName;

            foreach ($ruleCDRNames as $ind => $cdrName) {
                $cdr = $userRule->getCdr($cdrName);
                $cdr->type = $ruleCDRTypes[$ind];
            }

            //
            // Process a rule result two times: normal and optional redirect part.
            //

            $ruleActionToProcess = array();
            $ruleActionToProcess[] = $rule;
            $ruleActionToProcessIsRedirect[] = false;
            if (array_key_exists('redirect', $rule)) {
                $ruleActionToProcess[] = $this->getParam($rule, 'redirect');
            }
            $isFirstRule = true;

            $ruleGoodResult = 0;
            foreach ($ruleActionToProcess as $ruleAction) {
                /**
                 * @var array $ruleAction
                 */

                $ruleGoodResult++;
                if ($isFirstRule) {
                    $isRedirectS = false;
                    $isFirstRule = false;
                } else {
                    $isRedirectS = true;
                }

                $callDirection = $this->getParam($ruleAction, 'direction');
                $vendorValue = $this->getParam($ruleAction, 'vendor');
                $channelValue = $this->getParam($ruleAction, 'channel');
                $vendorExpression = $this->convertVendorAndChannelSpecification($userRule, $vendorValue, $channelValue);
                if (is_null($vendorExpression)) {
                    throw($this->signalError("There is no valid conversion to a unique channel-domain, for the vendor \"$vendorValue\", and the channel \"$channelValue\". Specify a correct pair of values."));
                }

                $maybeForceDirection = $this->getParam($ruleAction, 'force_direction', true);
                if ($this->isEmptyOrNull($maybeForceDirection)) {
                    $isForcedDirection = false;
                } else {
                    if ($maybeForceDirection) {
                        $isForcedDirection = true;
                    } else {
                        $isForcedDirection = false;
                    }
                }

                // set the internal and external telephone field expressions

                $internalTelephoneField = null;
                $externalTelephoneField = null;

                if (array_key_exists('internal', $ruleAction)) {
                    $internalTelephoneField = $this->convertExpressionToCdrExpression($userRule, $ruleAction['internal']);
                }

                if (array_key_exists('external', $ruleAction)) {
                    $externalTelephoneField = $this->convertExpressionToCdrExpression($userRule, $ruleAction['external']);
                }

                if (is_null($internalTelephoneField)) {
                    if ($callDirection == 'outgoing') {
                        $internalTelephoneField = $this->generateCdrExpression($userRule, $firstCDRName, 'src');
                    } else if ($callDirection == 'incoming') {
                        $internalTelephoneField = $this->generateCdrExpression($userRule, $lastCDRName, 'dst');
                    } else if ($callDirection == 'internal') {
                        $internalTelephoneField = $this->generateCdrExpression($userRule, $firstCDRName, 'src');
                    } else if ($callDirection == 'ignored') {
                        $internalTelephoneField = $this->generateCdrExpression($userRule, $firstCDRName, 'src');
                    }
                }

                if (is_null($externalTelephoneField)) {
                    if ($callDirection == 'outgoing') {
                        $externalTelephoneField = $this->generateCdrExpression($userRule, $lastCDRName, 'dst');

                    } else if ($callDirection == 'incoming') {
                        $externalTelephoneField = $this->generateCdrExpression($userRule, $firstCDRName, 'src');
                    } else if ($callDirection == 'internal') {
                        $externalTelephoneField = $this->generateCdrExpression($userRule, $lastCDRName, 'dst');
                    } else if ($callDirection == 'ignored') {
                        $externalTelephoneField = $this->generateCdrExpression($userRule, $lastCDRName, 'dst');
                    }
                }

                $billsecCDRName = $this->getParam($ruleAction, 'billsec_from');
                $billsecCDR = $userRule->getCdr($billsecCDRName);
                $billsecCDR->registerNeededField('can-be-billsec');
                $billsecCDR->registerNeededField('is-answered');

                $ruleResult = new UserRuleResult();
                $userRule->outResults[] = $ruleResult;
                $ruleResult->direction = $callDirection;

                $ruleResult->channel = $vendorExpression;

                $ruleResult->external = $externalTelephoneField;
                $ruleResult->internal = $internalTelephoneField;

                $ruleResult->billsecFrom = $userRule->getCdr($billsecCDRName, false);
                $ruleResult->isRedirect = $isRedirectS;
                $ruleResult->isForcedDirection = $isForcedDirection;
            }

            //
            // Convert rule conditions
            //

            $sourceConditions = $this->getParam($rule, 'conditions');
            foreach ($sourceConditions as $sourceCondition) {
                $this->addCondition($userRule, $sourceCondition);
            }

            //
            // Generate CLIPS rules.
            //

            $userRule->registerNeededFields();
            $userRule->calculateStrongChainLenghts();
            $ruleChainLen = $userRule->getCompleteChainLen();
            $rulePriority = 100 + $ruleChainLen;

            $r = "

(defrule $ruleName

  (declare (salience $rulePriority))

";

            foreach ($userRule->getStrongChains() as $strongChain) {
                /**
                 * @var StrongChain $strongChain
                 */

                // > (strong-chain (id ?strong-chain1-id) (len 5))
                $r .= "\n  (strong-chain (id " . $userRule->getStrongChainVarId($strongChain) . ') (len ' . $strongChain->len . ')) ';

                // > (strong-chain-is-good (chain-id ?strong-chain1-id))
                $r .= "\n  (strong-chain-is-good (chain-id " . $userRule->getStrongChainVarId($strongChain) . '))';
            }

            $cdrIdsToCompare = array();

            foreach ($userRule->cdrs as $cdr) {
                /**
                 * @var Cdr $cdr
                 */

                // Filter on type and strong-chain, so all the info is used.
                // > ?cdr0 <- (source-cdr (is-type mgw) (strong-chain-len 2)
                $r .= "\n" . '   ?' . $cdr->name . ' <- (source-cdr (is-type ' . $cdr->type . ') (strong-chain-id ' . $userRule->getStrongChainVarId($cdr->strongChain) . ') ';

                // Retrieve id and compare with already CDRs included in the chain,
                // because it must be distinct.
                // > (id ?cdr1-id &: (<> ?cdr0-id ?cdr1-id))
                $cdrIdVar = $userRule->getCdrFieldValueVar($cdr, 'id');
                $r .= '(id ' . $userRule->getCdrFieldValueVar($cdr, 'id') . ' ';
                if (count($cdrIdsToCompare) > 0) {
                    $r .= '&: ';

                    if (count($cdrIdsToCompare) > 1) {
                        $r .= '(and ';
                    }

                    foreach ($cdrIdsToCompare as $cdrIdToCompare) {
                        $r .= '(<> ' . $cdrIdVar . ' ' . $cdrIdToCompare . ') ';
                    }

                    if (count($cdrIdsToCompare) > 1) {
                        $r .= ') ';
                    }

                }

                $r .= ') ';

                // Add conditions on null fields
                // > (unique_id ?cdr0-unique_id &: (<> ?cdr0-unique_id 0))
                foreach ($cdr->nullLinkConditions as $nullField) {
                    $r .= "\n   ($nullField " . $userRule->getCdrFieldValueVar($cdr, $nullField) . ' &: (= ' . $userRule->getCdrFieldValueVar($cdr, $nullField) . ' 0))';
                }

                // Add not-null conditions
                // Add conditions on null fields
                // > (unique_id ?cdr0-unique_id &: (<> ?cdr0-unique_id 0))
                foreach ($cdr->notNullLinkConditions as $nullField) {
                    $r .= "\n   ($nullField " . $userRule->getCdrFieldValueVar($cdr, $nullField) . ' &: (<> ' . $userRule->getCdrFieldValueVar($cdr, $nullField) . ' 0))';
                }

                // Retrieve needed vars (vars needed in read)
                // If they are link add a condition that they must be different from 0.
                // > (last-half-of-unique-id-link ?cdr0-last-half-of-unique-id-link &: (neq 0 ?cdr0-last-half-of-unique-id-link))
                foreach ($cdr->neededFields as $fieldName => $ignore) {
                    $isLink = CdrField::isLinkField($fieldName);
                    $r .= "\n   ($fieldName " . $userRule->getCdrFieldValueVar($cdr, $fieldName);
                    if ($isLink) {
                        $r .= ' &: (<> 0 ' . $userRule->getCdrFieldValueVar($cdr, $fieldName) . ') ';
                    }

                    $r .= ') ';

                }

                $r .= ")\n\n";

                // Complete info of distinct cdrs
                $cdrIdsToCompare[] = $cdrIdVar;
            }

            // Add link conditions between declarated CDRs
            // >   (test (=  ?cdr0-last-half-of-unique-id-link ?cdr1-call-id-link))

            foreach ($userRule->conditions as $condition) {
                /**
                 * @var UserRuleCondition $condition
                 */
                $r .= "\n    " . $condition->toCLIPS($userRule);
            }

            // Do not apply rule, if a rule with higher priority was already applied.
            // > (not (rule-application (cdr-id ?cdr0-id) (chain-len ?previous-len &: (?previous-len > 5)))
            foreach ($userRule->cdrs as $cdr) {
                $r .= "\n    (not (rule-application (cdr-id " . $userRule->getCdrFieldValueVar($cdr, 'id') . ") (chain-len ?previous-rule-len &: (> ?previous-rule-len " . $ruleChainLen . "))))";
            }

            // Add conditions on the status fields of CDRS

            // Filter on needed status fields
            // > (cdr-status (cdr-id ?cdr0-id) (status dstChannel_isEnumChannel))
            foreach($userRule->cdrs as $cdr) {
                foreach ($cdr->statusConditions as $status) {
                    $r .= "\n    (cdr-status (cdr-id " . $userRule->getCdrFieldValueVar($cdr, 'id') . ") (status $status))";
                }
            }

            // Filter on not needed status fields
            // > (cdr-status (cdr-id ?cdr0-id) (status dstChannel_isEnumChannel))
            foreach($userRule->cdrs as $cdr) {
                foreach ($cdr->notStatusConditions as $status) {
                    $r .= "\n    (not (cdr-status (cdr-id " . $userRule->getCdrFieldValueVar($cdr, 'id') . ") (status $status)))";
                }
            }

            // Start the action part
            // > =>
            $r .= "\n\n=>\n\n";

            $resultInd = 0;
            foreach ($userRule->outResults as $outRule) {
                $resultInd++;

                if ($outRule->isRedirect) {
                    $isRedirectSymbol = "TRUE";
                } else {
                    $isRedirectSymbol = "FALSE";
                }

                // Assign the ignored direction, if the outgoing CDR is not answered.
                // > (bind ?result-cdr-direction (if (eq ?cdr0-is-answered TRUE) then outgoing else ignored))
                $r .= "\n   (bind ?result-cdr-direction (if (eq " . $userRule->getCdrFieldValueVar($outRule->billsecFrom, 'is-answered') . " TRUE) then " . $outRule->direction . " else ignored ))";

                // Test if the outgoing CDR can be an outgoing field, according other heuristics
                // > (if (eq ?cdr0-can-be-billsec FALSE)
                // >  then
                // >  (signal-error ?*low-priority-error* (assert (cdr-can-not-be-billsec (cdr-id ?cdr0-id) (rule-id 1))))
                // >  (invalidate-chain-of-cdr ?cdr0-id)
                // >
                $r .= "\n    (if (and (neq ?result-cdr-direction ignored) (eq " . $userRule->getCdrFieldValueVar($outRule->billsecFrom, 'can-be-billsec') . ' FALSE)) then (signal-error ?*low-priority-error* (assert (cdr-can-not-be-billsec (cdr-id ' . $userRule->getCdrFieldValueVar($outRule->billsecFrom, 'id') . ') (rule-id ' . $ruleId . ')))) (invalidate-chain-of-cdr ' . $userRule->getCdrFieldValueVar($outRule->billsecFrom, 'id') . ')';

                // Create the result-cdr, in case there were no errors in previous phases
                //
                // > else
                // > (bind ?result-cdr
                // >  (assert (result-cdr
                // >  (rule-id ?rule-id)
                // >  (destination-type ?result-cdr-direction)
                // >  (is-error FALSE)
                // >  (is-redirect FALSE)
                // >  (billsec ?cdr0-billsec)
                // >  (calldate ?cdr0-calldate)
                // >  (internal-extension ?cdr0-src)
                // >  (external-telephone-number ?cdr1-dst)
                // >  (ported-operator-prefix "")
                // >  (vendor-domain "free_enum"))))

                $resultVarName = '?result-' . $resultInd;

                $r .= "\n else (bind " . $resultVarName . ' (assert (result-cdr ';
                $r .= '(rule-id ' . $userRule->id . ') ';
                $r .= '(destination-type ?result-cdr-direction) ';
                $r .= '(is-error FALSE) (is-redirect ' . $isRedirectSymbol . ') (billsec ' . $userRule->getCdrFieldValueVar($outRule->billsecFrom, 'billsec') . ') ';
                $r .= '(calldate ' . $userRule->getCdrFieldValueVar($outRule->billsecFrom, 'calldate') . ') ';


                $r .= '(internal-extension ' . $outRule->internal->toCLIPS($userRule) . ') ';
                $r .= '(external-telephone-number ' . $outRule->external->toCLIPS($userRule) . ') ';
                $r .= '(ported-operator-prefix "") (vendor-domain ' . $outRule->channel->toCLIPS($userRule) . '))))';

                $isForcedS = ' (is-forced-direction ';
                if ($outRule->isForcedDirection) {
                    $isForcedS  .= 'TRUE';
                } else {
                    $isForcedS .= 'FALSE';
                }
                $isForcedS .= ') ';

                // Take note of the generated result-cdr, and of the position
                // > assert (rule-application
                // >   (slot rule-id (type INTEGER))
                // >   (slot chain-id (type INTEGER))
                // >   (slot chain-len (type INTEGER))
                // >   (slot cdr-id (type INTEGER))
                // >   (cdr-position (type INTEGER))
                // >   (result-cdr)
                // >   (is-forced-direction FALSE)
                // >   (is-redirect (type SYMBOL) (allowed-symbols TRUE FALSE)))
                //
                // > (assert (generated-direction (chain-id ?cdr1-chain-id) (direction outgoing) (is-forced-direction FALSE)))


                 $chainPos = 0;
                foreach ($userRule->cdrs as $cdr) {
                    $chainPos++;
                    $r .= "\n   (assert (rule-application (rule-id $ruleId) (chain-id " . $userRule->getStrongChainVarId($cdr->strongChain) . ") (chain-len $ruleChainLen ) (cdr-id " . $userRule->getCdrFieldValueVar($cdr,'id') . ") (cdr-position $chainPos) (is-redirect $isRedirectSymbol) (result-cdr $resultVarName) $isForcedS))";
                    $r .= "\n   (assert (generated-direction (chain-id " . $userRule->getStrongChainVarId($cdr->strongChain)  . ") (direction ?result-cdr-direction) $isForcedS))";
                }

                $r .= "\n  )"; // close the "if" part on the billsec error
            }
            $r .= "\n)\n";

            // Write the rules to file.

            fwrite($fh, $r);

        }

        fclose($fh);
    }

    /**
     * @param string $spec
     * @return array
     */
    protected function extractCDRNamesFromRuleSpec($spec)
    {
        return $this->extractCDRTypesOrNamesFromRuleSpec($spec, true);
    }

    /**
     * @param string $spec
     * @return array type names are converted to corresponding C type name
     * @throws Exception
     */
    protected function extractCDRTypesFromRuleSpec($spec)
    {
        $types = $this->extractCDRTypesOrNamesFromRuleSpec($spec, false);

        $result = array();
        foreach ($types as $typeName) {
            if ($typeName == 'MGW') {
                $result[] = 'mgw';
            } else if ($typeName == 'IPBX') {
                $result[] = 'ipbx';
            } else if ($typeName == 'SBC') {
                $result[] = 'sbc';
            } else {
                throw($this->signalError("Unknown CDR type \"$typeName\""));
            }
        }

        return $result;
    }

    /**
     * @param string $spec
     * @param bool $extractNames
     * @return array
     * @throws Exception
     */
    protected function extractCDRTypesOrNamesFromRuleSpec($spec, $extractNames)
    {
        $elements = explode('->', $spec);

        $r = array();
        foreach ($elements as $element) {
            $e = strstr($element, ':', $extractNames);
            if ($e == false) {
                throw($this->signalError("No CDR type specified in \"$spec\""));
            }

            $e = trim($e);

            if (!$extractNames) {
                $e = substr($e, 1);
                $e = trim($e);
            }

            $r[] = $e;
        }

        return $r;
    }

    /**
     * @param array $rule
     * @param string $key
     * @param bool $optional
     * @return string|array
     * @throws Exception
     */
    protected function getParam($rule, $key, $optional = false)
    {
        if (array_key_exists($key, $rule)) {
            return $rule[$key];
        } else {
            if ($optional) {
                return "";
            } else {
                throw($this->signalError("parameter \"$key\" is not specified."));
            }
        }
    }

    /**
     * @param UserRule $userRule
     * @param string $cdrName
     * @param string $fieldName
     * @return CdrExpression
     */
    protected function generateCdrExpression($userRule, $cdrName, $fieldName)
    {
        $cdr = $userRule->getCdr($cdrName);
        $expr = new CdrField();
        $expr->cdr = $cdr;
        $expr->fieldName = $fieldName;
        return $expr;
    }

    const CDR_REG = '\s*([\d\w_-]+)';
    const CDR_WITH_FIELD_REG = '\s*([\d\w_-]+)[.]([\d\w_-]+)\s*';

    /**
     * @param UserRule $userRule
     * @param string $expr
     * @return CdrExpression
     * @throws Exception
     */
    protected function convertExpressionToCdrExpression($userRule, $expr)
    {
        $m = array();

        // Recognize
        // > itc_get_dst_telephone_maybe_from_dstchannel(cdr1, cdr1)
        if (preg_match('/\s*itc_get_dst_telephone_maybe_from_dstchannel[(]' . self::CDR_REG . '\s*,' . self::CDR_REG . '\s*[)]\s*$/i', $expr, $m)) {
            $cdrName1 = trim($m[1]);
            $cdrName2 = trim($m[2]);
            $r = new Get_dst_telephone_maybe_from_dstchannel();
            $r->cdr1 = $userRule->getCdr($cdrName1);
            $r->cdr2 = $userRule->getCdr($cdrName2);
            return $r;
        }

        if (preg_match('/' . self::CDR_WITH_FIELD_REG . '$/i', $expr, $m)) {
            $cdrName = trim($m[1]);
            $fieldName = trim($m[2]);
            return $this->generateCdrExpression($userRule, $cdrName, $fieldName);
        }

        throw (new Exception("Unrecognized expression \"" . $expr . "\",in rule \"" . $userRule->name . "\"."));
    }

    /**
     * @param UserRule $rule
     * @param string $sourceCondition
     * @return void
     * @throws Exception
     */
    protected function addCondition($rule, $sourceCondition)
    {
        $m = array();

        $validLinks = CdrField::getValidLinkFields();

        // TODO copy values in rule documentation file
        $validStringFields = CdrField::getValidStringContentFields();

        // Recognize null link
        if (preg_match('/' . self::CDR_WITH_FIELD_REG . '([!]?=)\s+null\s*$/i', $sourceCondition, $m)) {
            $cdr1Name = trim($m[1]);
            $field1Name = trim($m[2]);

            $operator = trim($m[3]);
            if ($operator == '=') {
                $isEqual = true;
            } else {
                $isEqual = false;
            }

            if (in_array($field1Name, $validLinks)) {
                $cdr = $rule->getCdr($cdr1Name);
                if ($isEqual) {
                    $cdr->nullLinkConditions[] = $field1Name;
                } else {
                    $cdr->notNullLinkConditions[] = $field1Name;
                }

                return;
            }
        }

        // Recognize link

        if (preg_match('/' . self::CDR_WITH_FIELD_REG . '([!]?=)' . self::CDR_WITH_FIELD_REG . '$/i', $sourceCondition, $m)) {
            $cdr1Name = trim($m[1]);
            $field1Name = trim($m[2]);

            $operator = trim($m[3]);

            $cdr2Name = trim($m[4]);
            $field2Name = trim($m[5]);

            if ($operator == '=') {
                $isEqual = true;
            } else {
                $isEqual = false;
            }

            if (in_array($field1Name, $validLinks) && in_array($field2Name, $validLinks)) {
                $condition = new LinkCondition();
                $condition->value1 = $this->generateCdrExpression($rule, $cdr1Name, $field1Name);
                $condition->value2 = $this->generateCdrExpression($rule, $cdr2Name, $field2Name);
                $condition->isEqual = $isEqual;
                $rule->conditions[] = $condition;

                return;
            } else if (in_array($field1Name, $validStringFields) && in_array($field2Name, $validStringFields)) {
                $condition = new EqualStringFieldCondition();
                $condition->value1 = $this->generateCdrExpression($rule, $cdr1Name, $field1Name);
                $condition->value2 = $this->generateCdrExpression($rule, $cdr2Name, $field2Name);
                $condition->isEqual = $isEqual;
                $rule->conditions[] = $condition;
                return;
            }
        }

        // Recognize status field

        if (preg_match('/\s*not\s+' . self::CDR_REG . '[.]status[.]' . self::CDR_REG . '/i', $sourceCondition, $m)) {
            $cdr1Name = trim($m[1]);
            $field1Name = trim($m[2]);

            $cdr = $rule->getCdr($cdr1Name);
            $cdr->notStatusConditions[] = $field1Name;

            return;
        }

        if (preg_match('/' . self::CDR_REG . '[.]status[.]' . self::CDR_REG . '/i', $sourceCondition, $m)) {
            $cdr1Name = trim($m[1]);
            $field1Name = trim($m[2]);

            $cdr = $rule->getCdr($cdr1Name);
            $cdr->statusConditions[] = $field1Name;

            return;
        }

        // Signal error

        throw (new Exception("In rule \"" . $rule->name . "\", there is an error in condition \"$sourceCondition\""));

    }

    /**
     * @param string $str
     * @return string a version of $str that can be used a field of a CSV file.
     */
    static public function convertStringToCSVField($str)
    {
        $str = str_replace('"', '""', $str);
        $str = str_replace("\n", '\n', $str);
        $str = str_replace("\t", '\t', $str);
        $str = str_replace("\r", '\r', $str);

        return $str;
    }

    /**
     * @param string $str
     * @return string a version of $str that can be used a field of a CSV file.
     */
    static public function convertStringToCField($str)
    {
        $str = str_replace('\\', '\\\\', $str);
        $str = str_replace('"', '\"', $str);
        $str = str_replace("\n", '\n', $str);
        $str = str_replace("\t", '\t', $str);
        $str = str_replace("\r", '\r', $str);

        return $str;
    }

    /**
     * @param $str
     * @return string a HTML description of a rule.
     */
    protected function convertRuleDescriptionToHTML($str)
    {

        $str = htmlentities($str);
        $str = str_replace("\n", '<li>', $str);
        return '<ul><li>' . $str . '</ul>';

    }

    /**
     * @param $str
     * @return string
     */
    protected function convertStringToCSVAndC($str)
    {
        return CProceduresGenerator::convertStringToCField($str);
    }

    protected function getCLIPSGenericHeader()
    {

        $r = <<<'CLIPS_RULES'

;;
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!                                                  !!
;; !! THIS CODE IS AUTOGENERATED FROM HIGH LEVEL RULES !!
;; !!                                                  !!
;; !!!!!!!!!!!!!!!!!!!!!!z!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;
;; Asterisell CLIPS rules, for call flow merging of call chains.
;;

;; The requirements of this code are:
;; * a strong chain is composed from CDRS linked from strong links, and that for sure must be processed from the same rule
;; * a complete chain is composed of strong chains linked by weak links, and they can or no be part of the same rule
;; * a rule can work only on CDRS on the same complete chain
;; * if a rule use a CDR, then all the CDRS on the same strong chain must be used
;; * errors on bad behaviour of rules must be detected and CDR with errors must be signaled
;; * a STAT error is generated, for each invalidated chain, but also an informative message is generated, describing the reason, in order to use during the rule improving phase

;; The contracts (assumption) of this code are:
;; * the input data is indicated from comments, and it must be supplied from the caller, before the merge phase
;; * only "(strong-chain-is-good)" can be processed in rules, and in error detection rules
;; * if an error detection rule, find an error, then it call "(invalidate-chain)"
;; * the STATS reporting the chains with errors works on reported "(strong-chain-has-error)"

;;;;;;;;;;;;;;;;
;; DATA TYPES ;;
;;;;;;;;;;;;;;;;

;; This must be generated from the C caller, before merging.
(deftemplate strong-chain
  "A strong chain is a group of CDRS that are for sure linked togheter.
   A call can be composed of one or more strong-chains, if there are weak links,
   but for sure all the CDRS of each strong chain are part of the same call."

  (slot id (type INTEGER))

  (slot len (type INTEGER))
)

;; This must be generated from the C caller, before merging.
(deftemplate strong-chain-is-good
  "A strong chain can be used for rule applications, because is free from conflicts.
  This condition is used for avoiding infinite loops in rule applications, in case of bad chains."
  (slot chain-id (type INTEGER))
)

(deftemplate strong-chain-has-error
  "A strong chain can not be used for rule applications, because is has conflicts."
  (slot chain-id (type INTEGER))
)

;; This must be generated from the C caller, before merging.
(deftemplate pincode-extension
  "A PINCODE force a call to having a certain extension as internal telephone number."

  (slot strong-chain-id (type INTEGER))

  (slot extension (type STRING))
)

;; This must be generated from the C caller, before merging.
(deftemplate expected-direction
  "The expected direction of a chain of calls.
   A chain can be: outgoing, incoming, internal, or outgoing and incoming."

   (slot chain-id (type INTEGER))

   (slot direction (type SYMBOL) (allowed-symbols outgoing incoming internal ignored))
)

(deftemplate generated-direction
  "The direction of the call generated from the rules."

   (slot chain-id (type INTEGER))

   (slot direction (type SYMBOL) (allowed-symbols outgoing incoming internal ignored))

   (slot is-forced-direction (type SYMBOL) (allowed-symbols TRUE FALSE))
)

;; This must be generated from the C caller, before merging.
(deftemplate source-cdr
  (slot id
	(type INTEGER)
	(default 0))

  (slot strong-chain-id (type INTEGER))

  (slot is-type
	(type SYMBOL)
	(allowed-symbols mgw ipbx sbc))

  (slot calldate (type STRING) (default ""))
  (slot billsec (type INTEGER) (default 0))
  (slot src (type STRING) (default ""))
  (slot dst (type STRING) (default ""))
  (slot lastdata (type STRING) (default ""))
  (slot accountcode (type STRING) (default ""))
  (slot dst_channel_get_extension (type STRING) (default ""))
  (slot src_channel_get_extension (type STRING) (default ""))
  (slot destregid_vendor (type STRING) (default ""))
  (slot srcregid_vendor (type STRING) (default ""))
  (slot userfield_until_point (type INTEGER) (default 0))
  (slot unique_id (type INTEGER) (default 0))
  (slot callid (type INTEGER) (default 0))
  (slot callid2 (type INTEGER) (default 0))
  (slot unique_id_before_chiocciola (type INTEGER) (default 0))
  (slot last_half_of_unique_id (type INTEGER) (default 0))
  (slot first_half_of_unique_id (type INTEGER) (default 0))
  (slot can-be-billsec (type SYMBOL) (allowed-symbols TRUE FALSE) (default TRUE))
  (slot is-answered (type SYMBOL) (allowed-symbols TRUE FALSE) (default TRUE))
)

;; This must be generated from the C caller, before merging.
(deftemplate cdr-status
  (slot cdr-id (type INTEGER))
  (slot status(type SYMBOL)
	     (allowed-symbols
            isAnswered
            csv_parsing_error
            force_debug
            can_be_billsec
            calldestregid_isExternalVoIPVendor
            callsrcregid_isExternalVoIPVendor
            lastData_isBackupChannel
            dstChannel_isEnumChannel
            dstChannel_isDAHDI
            srcChannel_isSIP
            dstChannel_isSIP
            dstChannel_isSIPAndSBC
            dcontext_isInternal
            dcontext_isFaxTpa
            dcontext_isFromTrunk
            dcontext_isFax
            dcontext_isExtLocal
            dcontext_isExtGroup
            dcontext_isToPBX
            dcontext_isFromDirectDDI
            dcontext_isFaxBackup
            dcontext_isFaxToBackup
            lastdata_isExtensionAndDomain
            lastdata_isInternalChannel
            lastdata_isBackupChannel
            lastdata_isSIP
            srcChannel_isDAHDI
            lastapp_isResetCDR
            lastapp_isDial
            dstChannel_isLocal
            calldestregid_isIPBX
            dstChannel_isSIPWithExtension
            srcChannel_isSIPWithExtension
            dstChannel_isInstitutionDomain
            srcChannel_isInstitutionDomain
            dst_isFax
            callsrcregid_isMGW
            calldstregid_isMG
            dstchannel_isEmpty
            callsrcregid_isNull
            calldstregid_isNull))
)

(deftemplate result-cdr
  (slot id (default-dynamic (gensym)))

  (slot rule-id (type INTEGER))

  (slot destination-type
	(type SYMBOL)
	(allowed-symbols outgoing incoming internal ignored))

  (slot calldate (type STRING))

  (slot is-error
	(type SYMBOL)
	(allowed-symbols TRUE FALSE)
	(default FALSE))

  (slot is-redirect
	(type SYMBOL)
	(allowed-symbols TRUE FALSE)
	(default FALSE))

  (slot billsec (type INTEGER) (default 0))
  (slot internal-extension (type STRING) (default ""))
  (slot external-telephone-number (type STRING) (default ""))
  (slot ported-operator-prefix (type STRING) (default ""))
  (slot vendor-domain (type STRING) (default ""))
)

(deftemplate rule-application
  "Signal how rules are applied, in order to discover double usage of CDRS."

  (slot rule-id (type INTEGER))
  (slot chain-len (type INTEGER))
  (slot cdr-id (type INTEGER))
  (slot cdr-position (type INTEGER))
  (slot chain-id (type INTEGER))
  (slot result-cdr)
  (slot is-redirect (type SYMBOL) (allowed-symbols TRUE FALSE))
  (slot is-forced-direction (type SYMBOL) (allowed-symbols TRUE FALSE))
)

;;;;;;;;;;;;;;;;;;;
;; RATING PARAMS ;;
;;;;;;;;;;;;;;;;;;;

(defglobal ?*rate-from-date* = nil)
(defglobal ?*rate-to-date* = nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACT RELATED FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction itc_get_dst_telephone_maybe_from_dstchannel (?cdr0 ?cdr1)
  "If var_dcontext is one of the good known case, use dstchannel as destination telephone number, use dst otherwise."

  (bind ?cdr0-id (fact-slot-value ?cdr0 id))

  (bind ?result (fact-slot-value ?cdr1 dst))

  (do-for-fact ((?cdr-status cdr-status))
               (= ?cdr-status:cdr-id ?cdr0-id)
               (or (eq ?cdr-status:status dcontext_isExtGroup)
                   (eq ?cdr-status:status dcontext_isExtLocal))
               (bind ?result (fact-slot-value ?cdr1 dst_channel_get_extension)))

  (return ?result)
)

;;;;;;;;;;;
;; RULES ;;
;;;;;;;;;;;

;; Each rule has a salience (priority) in one of these phases.
;; This allows for the processing of info in different and distinct phases, when there is no any more info to process
;; in the previous phase.

;; IMPORTANT: nevere change merge-phase constant value,
;; because it is used in the code for ordering rules by chain len.
(defglobal ?*merge-phase* = 100)

(defglobal ?*conflicting-rules-phase* = 90)
(defglobal ?*adjust-expected-directions-phase* = 85)
(defglobal ?*unprocessed-source-cdrs-phase* = 80)
(defglobal ?*missing-direction-phase* = 70)
(defglobal ?*apply-pincode-phase* = 60)
(defglobal ?*write-results-phase-1* = 10)
(defglobal ?*write-results-phase-2* = 5)
(defglobal ?*write-results-phase-3* = 0)

;;;;;;;;;;;;;;;;;;;;;;
;; ERROR MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;

(deftemplate cdr-can-not-be-billsec
  (slot cdr-id (type INTEGER))
  (slot rule-id (type INTEGER))
)

(deftemplate conflicting-rules
  (slot rule1-id (type INTEGER) (default 0))
  (slot rule2-id (type INTEGER) (default 0))
)

(deftemplate unprocessed-chain
  (slot direction (type SYMBOL) (allowed-symbols outgoing incoming internal ignored))
)

(deftemplate ambigous-cdr-for-rule
  (slot rule-id (type INTEGER) (default 0))
  (slot cdr-id (type INTEGER) (default 0))
)

(deftemplate ambigous-pincode
  "A rule combine two chains with two different PINCODES."
)

(deftemplate rule-do-not-use-complete-chain
  (slot rule-id (type INTEGER))
)

(deftemplate missing-direction
  (slot rule-id (type INTEGER))
  (slot direction (type SYMBOL) (allowed-symbols outgoing incoming internal ignored))
)

(defglobal ?*low-priority-error* = 10)
(defglobal ?*high-priority-error* = 20)

(defglobal ?*error-message-type* = nil)
(defglobal ?*error-message-priority* = 0)

;; Configured from the C-caller
(defglobal ?*minimum-calldate-of-the-chain* = nil)

(deffunction signal-error (?p ?e)
  (if (> ?p ?*error-message-priority*)
      then
      (assert (is-there-error-in-the-chain))
      (bind ?*error-message-type* ?e)
      (bind ?*error-message-priority* ?p))
)

(deffunction invalidate-chain (?chain-id)
  "Invalidate a chain, signaling that it can not be processed any more."

  (do-for-fact
     ((?chain strong-chain-is-good))
     (= ?chain:chain-id ?chain-id)
     (retract ?chain)
     (assert (strong-chain-has-error (chain-id ?chain-id)))
  )
)

(deffunction invalidate-two-chains (?chain1-id ?chain2-id)
  "Convenience function."

  (invalidate-chain ?chain1-id)
  (if (<> ?chain1-id ?chain2-id) then (invalidate-chain ?chain2-id))
)

(deffunction invalidate-chain-of-cdr (?cdr-id)
  "Invalidate a chain, signaling that it can not be processed any more."

  (do-for-fact
     ((?cdr source-cdr))
     (= ?cdr:id ?cdr-id)
     (invalidate-chain ?cdr:strong-chain-id)
  )
)

;;;;;;;;;;;;;;;;
;; CSV EXPORT ;;
;;;;;;;;;;;;;;;;

(deffunction export-string (?string)
  "Encode all string delimiters in ?string.

   Each \" in ?string becomes \"\"."

  (bind ?result "")
  (loop-for-count (?c 0 (length$ ?string))

    (bind ?curr-position (- (length$ ?string) ?c))
    (bind ?char (sub-string ?curr-position ?curr-position ?string))
    (if (eq ?char "\"") then
      (bind ?result (str-cat "\"" ?result)))
    (bind ?result (str-cat ?char ?result)))
  (return ?result))

(deffunction open-result-file (?file-name ?is-new)
  "Open the file where putting the results. TRUE for creating a new file. FALSE for appending to an already existing file."
  (bind ?mode (if (eq ?is-new TRUE) then "w" else "a"))
  (open ?file-name CSV ?mode)
)

;; To call at the end
(deffunction close-result-file ()
  (close CSV)
)

CLIPS_RULES;

        //
        // Generate a CLIPS function according the constant.
        //

        $unprocessed_id = self::unprocessed;
        $incoming_id = self::incoming;
        $outgoing_id = self::outgoing;
        $error_id = self::error;
        $internal_id = self::internal;
        $ignored_id = self::ignored;

        $r .= "


(deffunction export-direction (?direction)
  \"Convert from symbolic direction, to internal integer code.\"

  (switch ?direction
    (case uprocessed then $unprocessed_id)
	  (case incoming then $incoming_id)
	  (case outgoing then $outgoing_id)
	  (case internal then $internal_id)
	  (case ignored then $ignored_id)
	  (case error then $error_id)
	  (default $error_id))
)
";

        $r .= <<<'CLIPS_RULES'

(deffunction export-bool (?b)
  "Convert from symbolic boolean, to MySQL integer."

  (if (eq ?b TRUE) then 1 else 0)
)

(deffunction export-all-result-cdrs ()
  "Export a result-cdr record to a CSV file. Require that strings are already in CSV friendly format.
   Do not export ignored calls.
   Do not export calls with a calldate not inside rating range."

  (do-for-all-facts ((?r result-cdr))
		    (and (neq ?r:destination-type ignored)
   	             (or (= 0 (str-compare ?*rate-from-date* "nil")) (>= (str-compare ?r:calldate ?*rate-from-date*) 0))
		         (or (= 0 (str-compare ?*rate-to-date* "nil"))
		             (< (str-compare ?r:calldate ?*rate-to-date*) 0)))

            (bind ?direction-code (export-direction ?r:destination-type))

		    (if (eq ?r:is-error TRUE)
			then
		      (bind ?destination-type (export-direction error))
		      (bind ?error-destination-type ?direction-code)

		    else
			  (bind ?destination-type ?direction-code)
			  (bind ?error-destination-type ?direction-code)
			)

		    (printout CSV
			      "\"" ?r:calldate "\","
			      "\"" ?destination-type "\","
			      "\"" ?error-destination-type "\","
			      "\"" (export-bool ?r:is-redirect) "\","
			      "\"" ?r:billsec "\","
			      "\"" ?r:billsec "\","
			      "\"" ?r:internal-extension "\","
			      "\"" ?r:external-telephone-number "\","
			      "\"" ?r:vendor-domain "\""
			      crlf
			      )
		    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERROR CORRECTION RULES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: only the first error message is generated, so rule phases for error recognition,
;; are from the most specific, to the most generic.

(defrule manage-unprocessed-cdrs
  "Signal error, if there are chains not processed from rules.
  Chains with len 1, do not generate errors in this phase, because they can be part of a complete chain (strong and weak part),
  and if the weakly linked strong chain is processed, then this chain can be ignored."

  (declare (salience ?*unprocessed-source-cdrs-phase*))

  (strong-chain (id ?chain-id) (len ?chain-len &: (> ?chain-len 1)))
  (strong-chain-is-good (chain-id ?chain-id))
  (not (generated-direction (chain-id ?chain-id)))

  (generated-direction (chain-id ?some-chain))
  ;; this rule is fired when there is at least one rule application,
  ;; but there are not processed CDRS in the chain.
  ;; The case of no rule activation for the chain, is managed from another rule,
  ;; with a better error message.

  (expected-direction (chain-id ?chain-id) (direction ?direction))

  =>

    (signal-error ?*high-priority-error* (assert (unprocessed-chain (direction ?direction))))
    (invalidate-chain ?chain-id)
)

(defrule manage-missing-rules
  "Signal an error if there were no rule activation on the chain.
  This rule is activated in particular if there are chains or len 1, not recognized as errors in the previous case."

  (declare (salience ?*unprocessed-source-cdrs-phase*))

  (strong-chain-is-good (chain-id ?chain-id))
  (not (generated-direction (chain-id ?another-chain-id)))
  (expected-direction (chain-id ?chain-id) (direction ?direction))

  =>
    (signal-error ?*high-priority-error* (assert (unprocessed-chain (direction ?direction))))
    (invalidate-chain ?chain-id)
)

(defrule manage-conflicting-rules-1
  "Signal an error if a CDR can be applied to more than a rule."

  (declare (salience ?*conflicting-rules-phase*))

  (rule-application (chain-id ?chain1) (rule-id ?rule1) )
  (rule-application (chain-id ?chain1) (rule-id ?rule2 &: (neq ?rule1 ?rule2)))
  (strong-chain-is-good (chain-id ?chain1))

  =>

  (signal-error ?*high-priority-error* (assert (conflicting-rules (rule1-id ?rule1) (rule2-id ?rule2))))
  (invalidate-chain ?chain1)
)

(defrule manage-conflicting-rules-2
  "Signal an error if a CDR can be used in more than one position, in the same rule."

  (declare (salience ?*conflicting-rules-phase*))

  (rule-application (cdr-id ?cdr1) (rule-id ?rule1) (cdr-position ?position1) (chain-id ?chain-id))
  (rule-application (cdr-id ?cdr1) (rule-id ?rule1) (cdr-position ?position2 &: (neq ?position1 ?position2)))
  (source-cdr (id ?cdr1) (strong-chain-id ?chain1-id))
  (strong-chain-is-good (chain-id ?chain1-id))

  =>

  (signal-error ?*high-priority-error* (assert (ambigous-cdr-for-rule (rule-id ?rule1) (cdr-id ?cdr1))))
  (invalidate-chain ?chain1-id)

)

(defrule manage-conflicting-rules-3
  "Signal an error if not all the CDRS of the same strong chain are used."

  (declare (salience ?*conflicting-rules-phase*))

  (rule-application (chain-id ?chain-id) (rule-id ?rule-id))
  (strong-chain-is-good (chain-id ?chain-id))
  (source-cdr (id ?cdr-id) (strong-chain-id ?chain-id))
  (not (rule-application (chain-id ?chain-id) (rule-id ?rule-id) (cdr-id ?cdr-id)))

  =>

  (signal-error ?*high-priority-error* (assert (rule-do-not-use-complete-chain (rule-id ?rule-id))))
  (invalidate-chain ?chain-id)
)

(defrule manage-missing-direction-in-rule-case-1
  "Signal an error if there are expected directions, that are missing from the generated result-cdrs.
  The internal directions are mandatory only if there are no other generated directions."

  (declare (salience ?*missing-direction-phase*))

  (expected-direction (chain-id ?chain-id) (direction ?direction &: (neq ?direction internal)))
  (strong-chain-is-good (chain-id ?chain-id))
  (rule-application (rule-id ?rule-id) (chain-id ?chain-id))
  (not (generated-direction (chain-id ?chain-id) (is-forced-direction TRUE)))
  (not (generated-direction (chain-id ?chain-id) (direction ?direction)))

  =>

  (signal-error ?*high-priority-error* (assert (missing-direction (rule-id ?rule-id) (direction ?direction))))
  (invalidate-chain ?chain-id)
)

(defrule manage-missing-direction-in-rule-case-2
  "Signal an error if there is internal expected direction, or no other direction is generated."

  (declare (salience ?*missing-direction-phase*))

  (expected-direction (chain-id ?chain-id) (direction internal))
  (strong-chain-is-good (chain-id ?chain-id))
  (rule-application (rule-id ?rule-id) (chain-id ?chain-id))
  (not (generated-direction (chain-id ?chain-id) (direction ?direction)))

  =>

  (signal-error ?*high-priority-error* (assert (missing-direction (rule-id ?rule-id) (direction internal))))
  (invalidate-chain ?chain-id)
)

(defrule apply-pincode
  "Associate the extension PINCODE to the call."

  (declare (salience ?*apply-pincode-phase*))

  (pincode-extension (strong-chain-id ?chain-id) (extension ?extension))
  (strong-chain-is-good (chain-id ?chain-id))
  (rule-application (cdr-id ?cdr-id) (chain-id ?chain-id) (result-cdr ?result-cdr))

  =>

  (if (eq outgoing (fact-slot-value ?result-cdr direction)) then (modify ?result-cdr (internal-extension ?extension)))
)

(defrule signal-pincode-conflicts
  "Signal if a call has more than one pincode."

  (declare (salience ?*apply-pincode-phase*))

  (pincode-extension (strong-chain-id ?chain-id1) (extension ?extension1))
  (pincode-extension (strong-chain-id ?chain-id2) (extension ?extension2 &: (neq ?extension2 extension1)))
  (strong-chain-is-good (chain-id ?chain-id1))
  (strong-chain-is-good (chain-id ?chain-id2))

  (rule-application (chain-id ?chain-id1) (result-cdr ?result-cdr))
  (rule-application (chain-id ?chain-id2) (result-cdr ?result-cdr))

  =>

  (if (eq outgoing (fact-slot-value ?result-cdr direction))
   then (signal-error ?*high-priority-error* (assert (ambigous-pincode)))
        (invalidate-two-chains ?chain-id1 ?chain-id2))
)

(defrule remove-result-in-case-of-errors-on-chain
  "In case of errors in the chain, all the results are wrong, and they are removed. Better result-cdr with error stats will generated later."

  (declare (salience ?*write-results-phase-1*))

  (strong-chain-has-error (chain-id ?chain-id))
  (rule-application (chain-id ?chain-id) (result-cdr ?result-cdr))

=>

  (if (eq FALSE (fact-slot-value ?result-cdr is-error)) then (retract ?result-cdr))
  ;; note: is-error FALSE for not deleting also error stats, generated later.
)

(defrule generate-error-stats
  "Generate an error stats for each expected direction."

  (declare (salience ?*write-results-phase-2*))

  (strong-chain-has-error (chain-id ?chain-id))
  (expected-direction (chain-id ?chain-id) (direction ?direction))

=>

  (assert (result-cdr
	   (destination-type ?direction)
	   (is-error TRUE)
	   (calldate ?*minimum-calldate-of-the-chain*)))
  ;; NOTE: result-cdr has a gensym slot, so it is generated a new version for each error
)

(defrule write-results
  "Write results or error stats."

  (declare (salience ?*write-results-phase-3*))

=>

  (export-all-result-cdrs)
)

;;;;;;;;;;;;;;;;;;;;;
;; GENERATED RULES ;;
;;;;;;;;;;;;;;;;;;;;;

CLIPS_RULES;

        return $r;

    }

}

/**
 * Represent RULES before converting to CLIPS.
 */
class UserRule
{
    /**
     * @var int
     */
    public $id;

    /**
     * @var string
     */
    public $name;

    /**
     * @var UserRuleResult[]
     */
    public $outResults;

    /**
     * @var Cdr[]
     */
    public $cdrs;

    /**
     * Get or create.
     *
     * @param string $name
     * @param bool $canCreate true for creating new Cdrs.
     * @return Cdr
     * @throws Exception
     */
    public function getCdr($name, $canCreate = true)
    {
        foreach ($this->cdrs as $cdr) {
            if ($cdr->name == $name) {
                return $cdr;
            }
        }

        if ($canCreate) {
            $cdr = new Cdr();
            $cdr->name = $name;
            $this->cdrs[] = $cdr;
        } else {
            throw (new Exception("Unknown CDR \"$name\" in rule \"" . $this->name . "\""));
        }

        return $cdr;
    }

    /**
     * @var UserRuleCondition[]
     */
    public $conditions;

    public function __construct()
    {
        $this->conditions = array();
        $this->cdrs = array();
        $this->outResults = array();
    }

    /**
     * @param Cdr $cdr
     * @param string $fieldName
     * @return string something like ?cdr0-calldate
     */
    public function getCdrFieldValueVar($cdr, $fieldName)
    {
        return '?' . $cdr->name . '-' . $fieldName;
    }

    public function getStrongChainVarId(StrongChain $strongChain) {
        return '?strong-chain' . $strongChain->id . '-id';
    }

    /**
     * @param Cdr $cdr
     * @return string
     */
    public function getCdrVarFact($cdr)
    {
        return '?' . $cdr->name;
    }

    /**
     * @param CdrField $cdrWithField
     * @return string
     */
    public function getCdrWithFieldValueVar($cdrWithField)
    {
        return $this->getCdrFieldValueVar($cdrWithField->cdr, $cdrWithField->fieldName);
    }

    /**
     * @return void
     */
    public function registerNeededFields()
    {
        foreach ($this->conditions as $c) {
            /**
             * @var UserRuleCondition $c
             */
            $c->registerNeededFields($this);
        }

        foreach ($this->outResults as $c) {
            /**
             * @var UserRuleResult $c
             */
            $c->registerNeededFields($this);
        }
    }

    public function calculateStrongChainLenghts()
    {
        //
        // Init components
        //

        /**
         * @var int[Cdr] map Cdr as key to group-index
         */
        $components = array();
        $groupIndex = 1;
        foreach ($this->cdrs as $cdr) {
            $components[$cdr->name] = $groupIndex++;
        }

        //
        // Calculate components
        //

        $notFixPoint = true;

        while ($notFixPoint) {
            $notFixPoint = false;

            foreach ($this->conditions as $condition) {
                if ($condition instanceof LinkCondition) {
                    /**
                     * @var LinkCondition $condition
                     */
                    if ($condition->isStrong()) {

                        $i1 = $components[$condition->value1->cdr->name];
                        $i2 = $components[$condition->value2->cdr->name];

                        if (!is_null($i1) && !is_null($i2) && $i1 !== $i2) {

                            $notFixPoint = true;

                            foreach($components as $cdrName => $groupIndex) {
                                if ($groupIndex == $i1) {
                                    $components[$cdrName] = $i2;
                                }
                            }
                        }
                    }
                }
            }
        }

        // Count the elements of each strong chain.

        $countElements = array();
        foreach ($components as $cdrName => $groupIndex) {
            if (array_key_exists($groupIndex, $countElements)) {
                $countElements[$groupIndex]++;
            } else {
                $countElements[$groupIndex] = 1;
            }
        }

        // Create strong chains.

        $strongChains = array();
        foreach ($countElements as $groupIndex => $groupCount) {
            $strongChain = new StrongChain();
            $strongChain->id = $groupIndex;
            $strongChain->len = $groupCount;

            $strongChains[$groupIndex] = $strongChain;
        }

        // Assign strong chains to CDRS.

        foreach ($this->cdrs as $cdr) {
            $groupIndex = $components[$cdr->name];
            $cdr->strongChain = $strongChains[$groupIndex];
        }

    }

    protected function mergeComponents(&$c1, &$c2)
    {
        return array_merge($c1, $c2);
    }

    /**
     * @return StrongChain[int]
     */
    public function getStrongChains() {
        $r = array();
        foreach($this->cdrs as $cdr) {
            $r[$cdr->strongChain->id] = $cdr->strongChain;
        }
        return $r;
    }

    /**
     * @return int the len of the complete chain of cdrs
     */
    public function getCompleteChainLen() {
        return count($this->cdrs);
    }

}

class StrongChain {
    /**
     * @var int
     */
    public $id;

    /**
     * @var int
     */
    public $len;
}

class Cdr
{

    /**
     * @var string
     */
    public $name;

    /**
     * @var string
     */
    public $type;

    /**
     * @var StrongChain
     */
    public $strongChain;

    /**
     * Needed status conditions according the rule.
     * @var string[]
     */
    public $statusConditions;

    /**
     * Negated status conditions according the rule.
     * @var string[]
     */
    public $notStatusConditions;

    /**
     * Link fields that must be to 0 (null)
     * @var string[]
     */
    public $nullLinkConditions;

    /**
     * Link fields that must be to not 0 (not null)
     * @var string[]
     */
    public $notNullLinkConditions;

    /**
     * Fields needed by the rules result.
     * @var bool[string]
     */
    public $neededFields;

    /**
     * @param string $fieldName
     */
    public function registerNeededField($fieldName)
    {
        $this->neededFields[$fieldName] = true;
    }

    public function __construct()
    {
        $this->statusConditions = array();
        $this->notStatusConditions = array();
        $this->neededFields = array();
        $this->nullLinkConditions = array();
        $this->notNullLinkConditions = array();
    }

}

class UserRuleResult
{

    /**
     * @var string
     */
    public $direction;

    /**
     * @var Cdr
     */
    public $billsecFrom;


    /**
     * @var CdrExpression
     */
    public $internal;

    /**
     * @var CdrExpression
     */
    public $external;

    /**
     * @var Expression
     */
    public $channel;

    /**
     * @var bool
     */
    public $isRedirect;

    /**
     * @var bool
     */
    public $isForcedDirection = false;

    /**
     * @param UserRule $rule
     * @return void
     */
    public function registerNeededFields($rule)
    {
        $this->internal->registerNeededFields($rule);
        $this->external->registerNeededFields($rule);
        $this->channel->registerNeededFields($rule);
        $this->billsecFrom->registerNeededField('billsec');
        $this->billsecFrom->registerNeededField('calldate');
    }
}

class Expression
{
    /**
     * @param UserRule $rule
     * @return void
     */
    public function registerNeededFields($rule)
    {

    }

    /**
     * @param UserRule $userRule
     * @return string
     */
    public function toCLIPS($userRule)
    {

    }

}

class StringExpression extends Expression
{
    /**
     * @var string
     */
    public $value;

    /**
     * @param string $v
     */
    public function __construct($v)
    {
        $this->value = $v;
    }

    /**
     * @param UserRule $userRule
     * @return string
     */
    public function toCLIPS($userRule)
    {
        return "\"" . CProceduresGenerator::convertStringToCField($this->value) . "\"";
    }
}

class VarExpression extends Expression
{
    /**
     * @var string
     */
    public $value;

    /**
     * @param string $v
     */
    public function __construct($v)
    {
        $this->value = $v;
    }

    /**
     * @param UserRule $userRule
     * @return string
     */
    public function toCLIPS($userRule)
    {
        return $this->value;
    }
}


class CdrExpression extends Expression
{

}

class CdrField extends CdrExpression
{

    /**
     * @var Cdr
     */
    public $cdr;

    /**
     * @var string
     */
    public $fieldName;

    /**
     * @return bool
     */
    public function isWeakLink()
    {
        if (trim($this->fieldName) === 'userfield_until_point') {
            return true;
        } else {
            return false;
        }
    }

    static public function getValidLinkFields() {
        $fields = array(
            'userfield_until_point',
            'unique_id',
            'callid',
            'callid2',
            'unique_id_before_chiocciola',
            'last_half_of_unique_id',
            'first_half_of_unique_id',
        );

        return $fields;
    }

    static public function getValidStringContentFields() {
        $r = array(
          'src'
        , 'dst'
        , 'lastdata'
        , 'src_channel_get_extension'
        , 'dst_channel_get_extension'
        , 'accountcode'
        );

        return $r;
    }

    static public function isLinkField($fieldName)
    {

        return in_array($fieldName, self::getValidLinkFields());
    }

    /**
     * @param UserRule $rule
     * @return void
     */
    public function registerNeededFields($rule)
    {
        $this->cdr->neededFields[$this->fieldName] = true;
    }

    /**
     * @param UserRule $userRule
     * @return string
     */
    public function toCLIPS($userRule)
    {
        return $userRule->getCdrWithFieldValueVar($this);
    }
}

class Get_dst_telephone_maybe_from_dstchannel extends CdrExpression
{

    /**
     * @var Cdr
     */
    public $cdr1;

    /**
     * @var Cdr
     */
    public $cdr2;

    /**
     * @param UserRule $userRule
     * @return string
     */
    public function toCLIPS($userRule)
    {
        return "(itc_get_dst_telephone_maybe_from_dstchannel " . $userRule->getCdrVarFact($this->cdr1) . ' ' . $userRule->getCdrVarFact($this->cdr2) . ') ';
    }
}

abstract class UserRuleCondition
{
    /**
     * @param UserRule $rule
     * @return void
     */
    public function registerNeededFields($rule)
    {
    }

    /**
     * @param UserRule $userRule
     * @return string
     */
    abstract public function toCLIPS($userRule);

}

class LinkCondition extends UserRuleCondition
{

    /**
     * @var CdrField
     */
    public $value1;

    /**
     * @var CdrField
     */
    public $value2;

    /**
     * @var bool
     */
    public $isEqual = true;

    /**
     * @return bool
     */
    public function isStrong()
    {
        // NOTE: every link that uses one weak field, it is not strong,
        // because there can be false linked CDRS. So the relation can not
        // be considered strong.
        $isWeak = ($this->value1->isWeakLink() || $this->value2->isWeakLink());
        return !$isWeak;
    }

    /**
     * @param UserRule $rule
     * @return void
     */
    public function registerNeededFields($rule)
    {
        $this->value1->registerNeededFields($rule);
        $this->value2->registerNeededFields($rule);
    }

    /**
     * NOTE: require that the fields are neq 0
     * @param UserRule $userRule
     * @return string
     */
    public function toCLIPS($userRule)
    {
        if ($this->isEqual) {
            $op = '=';
        } else {
            $op = '<>';
        }
        $op .= ' ';

        $r = '(test (' . $op . $userRule->getCdrWithFieldValueVar($this->value1) . ' ' . $userRule->getCdrWithFieldValueVar($this->value2) . ' ))';

        return $r;
    }

}

/**
 * Compare two CDR fields, if they are equals, assuming they are strings.
 */
class EqualStringFieldCondition extends UserRuleCondition
{

    /**
     * @var CdrField
     */
    public $value1;

    /**
     * @var CdrField
     */
    public $value2;

    /**
     * @var bool false for testing if they are not equals
     */
    public $isEqual = true;

    /**
     * @param UserRule $rule
     * @return void
     */
    public function registerNeededFields($rule)
    {
        $this->value1->registerNeededFields($rule);
        $this->value2->registerNeededFields($rule);
    }

    public function toCLIPS($userRule)
    {
        if ($this->isEqual) {
            $eq = 'eq';
        } else {
            $eq = 'neq';
        }
        $eq .= ' ';

        return "(test (" . $eq . $userRule->getCdrWithFieldValueVar($this->value1) . ' ' . $userRule->getCdrWithFieldValueVar($this->value2) . '))';
    }

}


function showUsage()
{
    echo "\nUsage:\n";
    echo "\nphp CProceduresGenerator.php init-project instance-name instance-type";
    echo "\n  create a CMake project file, for compiling the project, only if the CMakeLists.txt file does not exist.";
    echo "\n";
    echo "\nphp CProceduresGenerator.php compile instance-name instance-type";
    echo "\n  generate call flow merging rules for the instance-name";
    echo "\n";
}

/**
 * @param int $argc
 * @param string[] $argv
 * @return int
 */
function main($argc, $argv)
{

    if ($argc < 4) {
        showUsage();
        exit(1);
    }

    $cmd = $argv[1];
    $instanceName = $argv[2];
    $instanceType = $argv[3];

    if ($instanceType == 'production') {
        $projectParams = "-Wall -O2";
    } else if ($instanceType == 'dev') {
        $projectParams = "-g -Wall -O0";
    } else if ($instanceType == 'test') {
        $projectParams = "-Wall -O2";
    } else {
        echo "\nUnknown instance type \"$instanceType\"\n";
        exit(1);
    }
    $projectParams .= ' -std=c99';

    $instanceDir = 'provider_specific/' . $instanceName . '/';
    $instanceRules = $instanceDir . 'CallFlowMergeRules.yaml';
    $instanceParser = $instanceDir . 'csv_parser.rl';

    if (!file_exists($instanceRules)) {
        echo "\nfile \"$instanceRules\" does not exist.\n";
        exit(1);
    }

    if ($cmd == "init-project") {

        $cmakeProject = 'CMakeLists.txt';
        if (!file_exists($cmakeProject)) {
            // Generate project file only if it does not exists

            $cmake = '

project(call_flow_merge)
cmake_minimum_required(VERSION 2.6)

find_package(PkgConfig)
pkg_search_module(LIBGLIB REQUIRED glib-2.0)
include_directories(${LIBGLIB_INCLUDE_DIRS})
link_directories (${LIBGLIB_LIBRARY_DIRS})

pkg_search_module(LIBCLIPS REQUIRED clips-6)
include_directories(${LIBCLIPS_INCLUDE_DIRS})
link_directories (${LIBCLIPS_LIBRARY_DIRS})

set(CMAKE_C_FLAGS "' . $projectParams . '")
add_definitions(-D_POSIX_C_SOURCE=200809L)
add_definitions(-D_XOPEN_SOURCE)

add_custom_command (
  OUTPUT csv_parser.c.include
  COMMAND ragel -C -T0 ' . $instanceParser . ' -o csv_parser.c.include
  DEPENDS ' . $instanceParser . '
)

add_custom_command (
  OUTPUT ' . CProceduresGenerator::rule_source_file_name . ' ' . CProceduresGenerator::rule_descriptions_to_c . '

  COMMAND php CProceduresGenerator.php compile ' . $instanceName . ' ' . $instanceType . '
  DEPENDS ' . $instanceRules . '
)

add_custom_command (
  OUTPUT ' . CProceduresGenerator::rule_result_file_name . '

  COMMAND clips < ' . CProceduresGenerator::clips_compiling_script . '
  DEPENDS ' . CProceduresGenerator::rule_source_file_name . '
)

add_executable(call_flow_merge main.c import_pincodes.c call_flow_merger.c call_flow_merger.h debug_info.h debug_info.c import_pincodes.h csv_parser.c.include ' . CProceduresGenerator::rule_result_file_name . ' ' . CProceduresGenerator::rule_descriptions_to_c . ' )

TARGET_LINK_LIBRARIES(call_flow_merge Judy ${LIBGLIB_LIBRARIES} ${LIBCLIPS_LIBRARIES})
';

            file_put_contents($cmakeProject, $cmake);

            // Generate Makefile

            $discard = array();
            $exitStatus = 0;
            $cmd = "cmake $cmakeProject && make clean ";
            exec($cmd, $discard, $exitStatus);
            if ($exitStatus !== 0) {
                echo "\nError executing: \"$cmd\"\n";
                exit($exitStatus);
            }

            return 0;
        }
        return 0;

    } else if ($cmd == "compile") {

        try {

            $compiler = new CProceduresGenerator($instanceRules, CProceduresGenerator::rule_descriptions_to_c, CProceduresGenerator::rule_source_file_name);
            $compiler->generateCode();

        } catch (Exception $e) {
            echo "\nError: " . $e->getMessage() . "\n";
            exit(1);
        }
        return 0;
    } else {
        showUsage();
        exit(1);
    }
}
