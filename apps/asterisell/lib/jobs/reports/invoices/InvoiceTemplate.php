<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

define('EURO', chr(128));

include_once(getAsterisellCompleteRootDirectory() . '/ext_libs/fpdf/fpdf.php');
include_once(getAsterisellCompleteRootDirectory() . '/ext_libs/fpdi/fpdi.php');

/**
 * An invoice template to customize.
 */
abstract class InvoiceTemplate extends FPDI
{

    //
    // Abstract Functions to Implement
    //

    /**
     * @return void
     */
    abstract public function customInit();

    /**
     * @return bool True for using the logo specified in the web UI
     */
    abstract public function useDefaultLogo();

    /**
     * @return string|null null for not using any PDF template,
     * the name of a PDF file to use as page template background.
     * Then the rest of the invoice will be printed on top of the
     * template.
     */
    abstract public function backgroundPDFTemplate();

    /**
     * @return void
     */
    abstract public function addedNewPage();

    /**
     * @return void
     */
    public function addNewPage()
    {
        $this->AddPage();
        if ($this->tplIdx != null) {
            $this->useTemplate($this->tplIdx);
        }
        $this->addedNewPage();
    }

    /**
     * @param string $name
     * @param string $address
     * @return void
     */
    abstract public function writeSellerInfo($name, $address);

    /**
     * @return void
     */
    abstract public function writePageNumber();

    /**
     * @param string $ref
     * @return void
     */
    abstract public function writeInvoiceNr($ref);

    /**
     * @param string $date
     * @return void
     */
    abstract public function writeDate($date);

    /**
     * @param string $name
     * @param string $address
     * @return void
     */
    abstract public function writeBuyer($name, $address);

    /**
     * Describe the time-frame of selled calls.
     * @param string $s
     * @return void
     */
    abstract public function writeTimeFrame($s);

    /**
     * @return void
     */
    abstract public function writeDetailsHeader();

    /**
     * Add a detail line.
     *
     *
     * @param array $line list( "DESCRIPTION"    => string,
     *                            "QUANTITY"     => string,
     *                            "DURATION"      => string,
     *                            "COST" => string
     *                            "V_QUANTITY" => int,
     *                            "V_DURATION_SECONDS" => int,
     *                            "V_COST" => int)
     *
     * @return array list(true, true) if another line can be written, and there is space for totals, and invoice notes
     *               list(false, false) if there is no space for another line, and no space for totals and invoice notes
     *               list(true, false) if there is space for another line, but no space for totals and invoice notes
     */
    abstract public function writeDetailLine($line);

    /**
     * @param string $incomeWithoutTax
     * @param string $vatPerc
     * @param string $incomeTax
     * @param string $incomeWithTax
     * @return void
     */
    abstract public function writeTotals($incomeWithoutTax, $vatPerc, $incomeTax, $incomeWithTax);

    /**
     * @param string $remarque
     * @return void
     */
    abstract public function writeRemarque($remarque);

    //
    // Common Params
    //

    public $pageWidth = 210;
    public $pageBorder = 20;

    public $pageHeight = 297;
    public $pageTopBorder = 15;
    public $pageBottomBorder = 15;

    public $defaultFont = 'Arial';
    public $defaultFontSize = 10;

    /**
     * @var float the height of a line of text.
     * It is strange but FPDF library can not tell what is the height of a font
     * so this value must be hard-coded here.
     */
    public $defaultFontHeight = 6.0;

    /**
     * @var int how much increase font or decrease switching from normal to details or titles
     * */
    public $fontSizeIncr = 2;

    public $fontHeightIncr = 1.5;

    /**
     * @var ArReport
     */
    protected $report;

    /**
     * @var $characterSet
     */
    protected $characterSet = "UTF-8";

    public function setCharacterSet($v)
    {
        $this->characterSet = $v;
    }

    /**
     * @param ArReport $report
     */
    public function setArReport($report)
    {
        $this->report = $report;
    }

    /**
     * @return ArReport|null
     */
    public function getArReport()
    {
        return $this->report;
    }

    /**
     * @var ArParty
     */
    protected $cachedParty = null;

    /**
     * @return ArParty
     */
    public function getArParty()
    {
        if (is_null($this->cachedParty)) {
            $date = fromMySQLTimestampToUnixTimestamp($this->getArReport()->getLegalDate());
            $partyId = OrganizationUnitInfo::getInstance()->getArPartyId($this->getArReport()->getArOrganizationUnitId(), $date);
            $party = ArPartyPeer::retrieveByPK($partyId);
            $this->cachedParty = $party;
        }

        // TODO if there are no errors, replace with function with a return $buyer
        // NOTE: apparently no, investigate
        // assert($this->cachedParty->getId() == $this->buyer->getId());
        return $this->cachedParty;
    }

    /**
     * @var ArParty
     */
    protected $buyer;

    public function setBuyer($b)
    {
        $this->buyer;
    }

    /**
     * @var bool true for a legal invoice, false for a report of calls, similar to an invoice
     */
    protected $isLegalInvoice = true;

    /**
     * @param bool $v
     */
    public function setIsLegalInvoice($v)
    {
        $this->isLegalInvoice = $v;
    }

    /**
     * @var ArParams|null
     */
    private $seller;

    private $logoYXProportion;

    const TEXT_HEIGTH = 4;
    const BORDER = 10;

    protected $colonnes;
    protected $angle = 0;

    protected $tplIdx;


    //
    // Utility
    //

    /**
     * @var null
     */
    protected $cachedInvoiceFooter = null;

    /**
     * Use the generic Invoice Footer, and also the Customer TAGS.
     * @return string
     */
    public function getInvoiceFooter()
    {
        if (is_null($this->cachedInvoiceFooter)) {
            $params = ArParamsPeer::getDefaultParams();
            $r = '';

            $t = $params->getInvoiceNotes();
            if (!isEmptyOrNull($t)) {
                $r .= $t . "\n";
            }

            $t = $params->getInvoicePaymentTerms();
            if (!isEmptyOrNull($t)) {
                $r .= $t . "\n";
            }

            $c = new Criteria();
            $c->addJoin(ArPartyHasTagPeer::AR_PARTY_ID, $this->getArParty()->getId());
            $c->addJoin(ArPartyHasTagPeer::AR_TAG_ID, ArTagPeer::ID);
            $c->addAscendingOrderByColumn(ArTagPeer::INTERNAL_NAME);
            $tags = ArPartyHasTagPeer::doSelect($c);
            foreach ($tags as $tag) {
                /**
                 * @var ArPartyHasTag $tag
                 */
                $t = $tag->getArTag()->getNoteForCustomer();
                if (!isEmptyOrNull($t)) {
                    $r .= $t . "\n";
                }
            }

            $this->cachedInvoiceFooter = $this->replacePlaceHolders($r);
        }

        return $this->cachedInvoiceFooter;
    }

    /**
     * Function called from listOfDIDSAsString, that can be customized.
     *
     * @param string $did
     * @return string a version of the DID that can be included in the invoice.
     */
    protected function readableDID($did) {
       return $did;
    }

    /**
     * @param string $sep DID separator
     * @return string the list of DIDS associated to the party
     * as a list, separated by comma.
     */
    protected function listOfDIDSAsString($sep) {
       $fromDate = fromMySQLTimestampToUnixTimestamp($this->getArReport()->getFromDate());
       $toDate = fromMySQLTimestampToUnixTimestamp($this->getArReport()->getToDate());
       $organizationId = $this->getArReport()->getArOrganizationUnitId();

       $info = OrganizationUnitInfo::getInstance();
       $allDids = $info->getAllDIDSAtDate($organizationId, $fromDate, $toDate, null, ExpandExtensions::INTERNAL_NAME_PREFIX);

       $isFirst = true;
       $r = '';
       foreach($allDids as $did) {
           if ($isFirst) {
               $isFirst = false;
           } else {
               $r .= $sep;
           }

           $r .= $this->readableDID($did);
       }

       return $r;
    }

    /**
     * @return float the height of the invoice footer text
     * NOTE: it is approximate, because there can be text more long than one line.
     */
    public function getInvoiceFooterHeight()
    {
        return (countLines($this->getInvoiceFooter()) + 1) * $this->defaultFontHeight;
    }

    /**
     * @param string $str replace invoice placeholders with real content
     * @return string
     */
    protected function replacePlaceHolders($str)
    {
        $params = ArParamsPeer::getDefaultParams();
        $legalDate = fromMySQLTimestampToUnixTimestamp($this->getArReport()->getLegalDate());
        $withinDays = $params->getInvoicePaymentDueInXxDays();
        if (isEmptyOrNull($withinDays)) {
            $withinDays = 0;
        }
        $dueDate = strtotime('+' . $params->getInvoicePaymentDueInXxDays() . ' days', $legalDate);

        $from = array(
          '${pay_due_date}'
        , '${pay_within_days}'
        , '${invoice_number}'
        , '${sepa}'
        , '${iban}'
        , '${bic}'
        );

        $to = array(
            format_invoice_timestamp_according_config($dueDate)
        , $withinDays
        , $this->getArReport()->getCompleteLegalCode()
        , $this->getArParty()->getPaymentSepa()
        , $this->getArParty()->getPaymentIban()
        , $this->getArParty()->getPaymentBic()
        );

        return str_replace($from, $to, $str);
    }

    //
    // Framework Functions
    //

    function __construct()
    {
        parent::__construct('P', 'mm', 'A4');

    }

    public function init()
    {
        $this->SetAutoPageBreak(false);
        $this->AliasNbPages();
        $this->cachedParty = null;
        $this->cachedInvoiceFooter = null;

        $this->seller = ArParamsPeer::getDefaultParams();
        $this->colonnes = array();
        $this->angle = 0;

        $logoFile = $this->getLogoFile();
        if (is_null($logoFile)) {
            $this->logoYXProprtion = 0;
        } else {
            $info = getimagesize($logoFile);
            $pixelWidth = $info[0];
            $pixelHeight = $info[1];
            $this->logoYXProprtion = (1.0 * $pixelHeight) / (1.0 * $pixelWidth);
        }

        $this->tplIdx = null;
        if ($this->backgroundPDFTemplate() != null) {
            $this->tplIdx = $this->setSourceFile($this->backgroundPDFTemplate());
            $this->tplIdx = $this->importPage(1);
        }

        $this->customInit();
    }

    function getLogoFile()
    {
        if ($this->useDefaultLogo()) {
            return $this->seller->getLogoImageInInvoicesWithCompletePath();
        } else {
            return null;
        }
    }

    //
    // Utility Functions
    //

    /**
     * @param mixed $value a string in UTF-8 format
     * @return string in PDF format
     */
    public function convertString($value)
    {
        if ($this->characterSet == "UTF-8") {
            return $value;
        } else {
            return iconv("UTF-8", $this->characterSet, "$value");
        }
    }

    function RoundedRect($x, $y, $w, $h, $r, $style = '')
    {
        $k = $this->k;
        $hp = $this->h;
        if ($style === 'F')
            $op = 'f';
        elseif ($style === 'FD' || $style === 'DF')
            $op = 'B';
        else
            $op = 'S';
        $MyArc = 4 / 3 * (sqrt(2) - 1);
        $this->_out(sprintf('%.2F %.2F m', ($x + $r) * $k, ($hp - $y) * $k));
        $xc = $x + $w - $r;
        $yc = $y + $r;
        $this->_out(sprintf('%.2F %.2F l', $xc * $k, ($hp - $y) * $k));

        $this->_Arc($xc + $r * $MyArc, $yc - $r, $xc + $r, $yc - $r * $MyArc, $xc + $r, $yc);
        $xc = $x + $w - $r;
        $yc = $y + $h - $r;
        $this->_out(sprintf('%.2F %.2F l', ($x + $w) * $k, ($hp - $yc) * $k));
        $this->_Arc($xc + $r, $yc + $r * $MyArc, $xc + $r * $MyArc, $yc + $r, $xc, $yc + $r);
        $xc = $x + $r;
        $yc = $y + $h - $r;
        $this->_out(sprintf('%.2F %.2F l', $xc * $k, ($hp - ($y + $h)) * $k));
        $this->_Arc($xc - $r * $MyArc, $yc + $r, $xc - $r, $yc + $r * $MyArc, $xc - $r, $yc);
        $xc = $x + $r;
        $yc = $y + $r;
        $this->_out(sprintf('%.2F %.2F l', ($x) * $k, ($hp - $yc) * $k));
        $this->_Arc($xc - $r, $yc - $r * $MyArc, $xc - $r * $MyArc, $yc - $r, $xc, $yc - $r);
        $this->_out($op);
    }

    function _Arc($x1, $y1, $x2, $y2, $x3, $y3)
    {
        $h = $this->h;
        $this->_out(sprintf('%.2F %.2F %.2F %.2F %.2F %.2F c ', $x1 * $this->k, ($h - $y1) * $this->k,
            $x2 * $this->k, ($h - $y2) * $this->k, $x3 * $this->k, ($h - $y3) * $this->k));
    }

    function Rotate($angle, $x = -1, $y = -1)
    {
        if ($x == -1)
            $x = $this->x;
        if ($y == -1)
            $y = $this->y;
        if ($this->angle != 0)
            $this->_out('Q');
        $this->angle = $angle;
        if ($angle != 0) {
            $angle *= M_PI / 180;
            $c = cos($angle);
            $s = sin($angle);
            $cx = $x * $this->k;
            $cy = ($this->h - $y) * $this->k;
            $this->_out(sprintf('q %.5F %.5F %.5F %.5F %.2F %.2F cm 1 0 0 1 %.2F %.2F cm', $c, $s, -$s, $c, $cx, $cy, -$cx, -$cy));
        }
    }

    function _endpage()
    {
        if ($this->angle != 0) {
            $this->angle = 0;
            $this->_out('Q');
        }
        parent::_endpage();
    }

    // public functions
    function sizeOfText($texte, $largeur)
    {
        $index = 0;
        $nb_lines = 0;
        if ($largeur > 0) {
            $loop = TRUE;
        } else {
            $loop = FALSE;
        }
        while ($loop) {
            $pos = strpos($texte, "\n");
            if (!$pos) {
                $loop = FALSE;
                $ligne = $texte;
            } else {
                $ligne = substr($texte, $index, $pos);
                $texte = substr($texte, $pos + 1);
            }
            $length = floor($this->GetStringWidth($ligne));
            $res = 1 + floor($length / $largeur);
            $nb_lines += $res;
        }
        return $nb_lines;
    }

    function lineVert($tab)
    {
        reset($this->colonnes);
        $maxSize = 0;
        while (list($lib, $pos) = each($this->colonnes)) {
            $texte = $tab[$lib];
            $longCell = $pos - 2;
            $size = $this->sizeOfText($texte, $longCell);
            if ($size > $maxSize)
                $maxSize = $size;
        }
        return $maxSize;
    }

    // add a watermark (temporary estimate, DUPLICATA...)
    // call this method first
    function temporaire($texte)
    {
        $this->SetFont('Arial', 'B', 50);
        $this->SetTextColor(203, 203, 203);
        $this->Rotate(45, 55, 190);
        $this->Text(55, 190, $texte);
        $this->Rotate(0);
        $this->SetTextColor(0, 0, 0);
    }
}

?>