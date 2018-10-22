<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

define('EURO', chr(128));

include_once(getAsterisellCompleteRootDirectory() . '/ext_libs/fpdf/fpdf.php');

/**
 * Generate a PDF Invoice.
 *
 * Original author: Xavier Nicolay, 2004.
 * Modified from Massimo Zaniboni, 2009.
 */
class PDFInvoiceTemplate01 extends FPDF
{
    protected $colonnes;
    protected $format;
    protected $angle = 0;

    /**
     * @var bool true for a legal invoice, false for a report of calls, similar to an invoice
     */
    protected $isLegalInvoice = true;

    /**
     * @param bool $v
     */
    public function setIsLegalInvoice($v) {
        $this->isLegalInvoice = $v;
    }

    /**
     * @var ArParams|null
     */
    private $seller;

    /**
     * @var ArParty
     */
    private $buyer;

    private $logoYXProportion;

    const TEXT_HEIGTH = 4;
    const BORDER = 10;

    function __construct(ArParty $buyerParty)
    {
        parent::__construct('P', 'mm', 'A4');

        $this->SetAutoPageBreak(false);
        $this->AliasNbPages();

        $this->buyer = $buyerParty;
        $this->seller = ArParamsPeer::getDefaultParams();
        $this->colonnes = array();
        $this->format = array();
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
    }

    function getLogoFile()
    {
        return $this->seller->getLogoImageInInvoicesWithCompletePath();
    }

    // private functions
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
        while ($loop)
        {
            $pos = strpos($texte, "\n");
            if (!$pos) {
                $loop = FALSE;
                $ligne = $texte;
            }
            else
            {
                $ligne = substr($texte, $index, $pos);
                $texte = substr($texte, $pos + 1);
            }
            $length = floor($this->GetStringWidth($ligne));
            $res = 1 + floor($length / $largeur);
            $nb_lines += $res;
        }
        return $nb_lines;
    }

    // Company
    function addCompany($nom, $adresse)
    {
        $border = 10;
        $x1 = $border;
        $y1 = 8;

        $logoFile = $this->getLogoFile();
        if (!is_null($logoFile)) {
            $pageWidth = 210 - $border - $border;
            $width_LS = ($pageWidth / 4);
            $startX_L = $x1;
            $endX_L = $startX_L + $width_LS;
            $startY_L = $y1;
            $height_L = $width_LS * $this->logoYXProprtion;
            $endY_L = $startY_L + $height_L;

            $this->Image($logoFile, $x1, $y1, $width_LS);

            // add a space between logo and header
            $y1 = $endY_L + $border / 2;
        }

        //Positionnement en bas
        $this->SetXY($x1, $y1);
        $this->SetFont('Arial', 'B', 12);
        $length = $this->GetStringWidth($nom);
        $this->Cell($length, 2, $nom);
        $this->SetXY($x1, $y1 + 4);
        $this->SetFont('Arial', '', 10);
        $length = $this->GetStringWidth($adresse);
        //Coordonnées de la société
        $lignes = $this->sizeOfText($adresse, $length);
        $this->MultiCell($length, 4, $adresse);
    }

    // Label and number of invoice/estimate
    function fact_dev($libelle, $num)
    {
        $r1 = $this->w - 80;
        $r2 = $r1 + 68;
        $y1 = 6;
        $y2 = $y1 + 2;
        $mid = ($r1 + $r2) / 2;

        $texte = $libelle . " EN " . EURO . " N° : " . $num;
        $szfont = 12;
        $loop = 0;

        while ($loop == 0)
        {
            $this->SetFont("Arial", "B", $szfont);
            $sz = $this->GetStringWidth($texte);
            if (($r1 + $sz) > $r2)
                $szfont--;
            else
                $loop++;
        }

        $this->SetLineWidth(0.1);
        $this->SetFillColor(192);
        $this->RoundedRect($r1, $y1, ($r2 - $r1), $y2, 2.5, 'DF');
        $this->SetXY($r1 + 1, $y1 + 2);
        $this->Cell($r2 - $r1 - 1, 5, $texte, 0, 0, "C");
    }

    // Estimate
    function addDevis($numdev)
    {
        $string = sprintf("DEV%04d", $numdev);
        $this->fact_dev("Devis", $string);
    }

    // Invoice
    function addFacture($numfact)
    {
        $string = sprintf("FA%04d", $numfact);
        $this->fact_dev("Facture", $string);
    }

    function addDate($date)
    {
        $r1 = $this->w - 61;
        $r2 = $r1 + 30;
        $y1 = 17;
        $y2 = $y1;
        $mid = $y1 + ($y2 / 2);
        $this->RoundedRect($r1, $y1, ($r2 - $r1), $y2, 3.5, 'D');
        $this->Line($r1, $mid, $r2, $mid);
        $this->SetXY($r1 + ($r2 - $r1) / 2 - 5, $y1 + 3);
        $this->SetFont("Arial", "B", 10);
        $this->Cell(10, 5, mytr("DATE"), 0, 0, "C");
        $this->SetXY($r1 + ($r2 - $r1) / 2 - 5, $y1 + 9);
        $this->SetFont("Arial", "", 10);
        $this->Cell(10, 5, $date, 0, 0, "C");
    }

    function addInvoiceNr($ref)
    {
        $r1 = $this->w - 31;
        $r2 = $r1 + 19;
        $y1 = 17;
        $y2 = $y1;
        $mid = $y1 + ($y2 / 2);
        $this->RoundedRect($r1, $y1, ($r2 - $r1), $y2, 3.5, 'D');
        $this->Line($r1, $mid, $r2, $mid);
        $this->SetXY($r1 + ($r2 - $r1) / 2 - 5, $y1 + 3);
        $this->SetFont("Arial", "B", 10);
        $this->Cell(10, 5, mytr("NR."), 0, 0, "C");
        $this->SetXY($r1 + ($r2 - $r1) / 2 - 5, $y1 + 9);
        $this->SetFont("Arial", "", 10);
        $this->Cell(10, 5, $ref, 0, 0, "C");
    }

    function addPageNumber()
    {
        $r1 = $this->w - 80;
        $r2 = $r1 + 19;
        $y1 = 17;
        $y2 = $y1;
        $mid = $y1 + ($y2 / 2);
        $this->RoundedRect($r1, $y1, ($r2 - $r1), $y2, 3.5, 'D');
        $this->Line($r1, $mid, $r2, $mid);
        $this->SetXY($r1 + ($r2 - $r1) / 2 - 5, $y1 + 3);
        $this->SetFont("Arial", "B", 10);
        $this->Cell(10, 5, mytr("PAGE"), 0, 0, "C");
        $this->SetXY($r1 + ($r2 - $r1) / 2 - 5, $y1 + 9);
        $this->SetFont("Arial", "", 10);
        $this->Cell(10, 5, $this->PageNo() . ' ' . mytr('of') . ' {nb}', 0, 0, "C");
    }

    function addCustomer($customer, $address)
    {

        $r1 = $this->w - 80;
        $r2 = $r1 + 68;
        $y1 = 40;
        $this->SetXY($r1, $y1);

        $this->SetFont('Arial', 'B', 12);
        $length = $this->GetStringWidth($customer);
        $this->Cell($length, 2, $customer);

        $this->SetXY($r1, $y1 + 4);
        $this->SetFont('Arial', '', 10);
        $length = $this->GetStringWidth($address);
        $lignes = $this->sizeOfText($address, $length);
        $this->MultiCell($length, 4, $address);
    }

    // Invoice timeframe
    function addTimeframe($s)
    {
        $this->SetFont("Arial", "B", 10);
        $r1 = $this->w - 80;
        $r2 = $r1 + 70;
        $y1 = 80;
        $y2 = $y1 + 10;
        $mid = $y1 + (($y2 - $y1) / 2);
        $this->RoundedRect($r1, $y1, ($r2 - $r1), ($y2 - $y1), 2.5, 'D');
        $this->Line($r1, $mid, $r2, $mid);
        $this->SetXY($r1 + 16, $y1 + 1);
        if ($this->isLegalInvoice) {
            $n = "Invoiced Period";
        }  else {
            $n = "Period";
        }
        $this->Cell(40, 4, mytr($n), '', '', "C");
        $this->SetFont("Arial", "", 10);
        $this->SetXY($r1 + 16, $y1 + 5);
        $this->Cell(40, 5, $s, '', '', "C");
    }

    function addCols($tab)
    {
        $r1 = 10;
        $r2 = $this->w - ($r1 * 2);
        $y1 = 100;
        $y2 = $this->h - 50 - $y1;
        $this->SetXY($r1, $y1);
        $this->Rect($r1, $y1, $r2, $y2, "D");
        $this->Line($r1, $y1 + 6, $r1 + $r2, $y1 + 6);
        $colX = $r1;
        $this->colonnes = $tab;
        while (list($lib, $pos) = each($tab))
        {
            $this->SetXY($colX, $y1 + 2);
            $this->Cell($pos, 1, $lib, 0, 0, "C");
            $colX += $pos;
            $this->Line($colX, $y1, $colX, $y1 + $y2);
        }
    }

    function addLineFormat($tab)
    {
        $this->format = $tab;
    }

    function lineVert($tab)
    {
        reset($this->colonnes);
        $maxSize = 0;
        while (list($lib, $pos) = each($this->colonnes))
        {
            $texte = $tab[$lib];
            $longCell = $pos - 2;
            $size = $this->sizeOfText($texte, $longCell);
            if ($size > $maxSize)
                $maxSize = $size;
        }
        return $maxSize;
    }

    // add a line to the invoice/estimate
    /*    $ligne = array( "REFERENCE"    => $prod["ref"],
     "DESTINATION"  => $libelle
     "QUANTITY"     => sprintf( "%.2F", $prod["qte"]) ,
     "DURATION"      => sprintf( "%.2F", $prod["px_unit"]),
     "COST" => sprintf ( "%.2F", $prod["qte"] * $prod["px_unit"]) ,
    */
    function addLine($ligne, $tab)
    {
        $ordonnee = 10;
        $maxSize = $ligne;

        reset($this->colonnes);
        while (list($lib, $pos) = each($this->colonnes))
        {
            $longCell = $pos - 2;
            $texte = $tab[$lib];
            $length = $this->GetStringWidth($texte);
            $tailleTexte = $this->sizeOfText($texte, $length);
            if (array_key_exists($lib, $this->format)) {
                $formText = $this->format[$lib];
            } else {
                $formText = 'L';
            }
            $this->SetXY($ordonnee, $ligne - 1);
            $this->MultiCell($longCell, self::TEXT_HEIGTH, $texte, 0, $formText);
            if ($maxSize < ($this->GetY()))
                $maxSize = $this->GetY();
            $ordonnee += $pos;
        }
        return ($maxSize - $ligne);
    }

    function addRemarque($remarque)
    {
        $this->SetFont("Arial", "", 10);
        $r1 = 10;
        $y1 = $this->h - 45;
        $this->SetXY($r1, $y1);

        $x2 = $this->w - 70 - self::BORDER;
        $width = $x2 - $r1;
        
        $this->MultiCell($width, self::TEXT_HEIGTH, $remarque, 0, 'L');
    }

    function addTotalsBox()
    {
        $r1 = $this->w - 70;
        $r2 = $r1 + 60;
        $y1 = $this->h - 45;
        $y2 = $y1 + 30;
        $this->RoundedRect($r1, $y1, ($r2 - $r1), ($y2 - $y1), 2.5, 'D');

        $this->SetFont("Arial", "B", 10);

        $this->SetXY($r1, $y1 + 5);
        $this->Cell(20, 4, mytr("Amount"), 0, 0, "C");

        $this->SetXY($r1, $y1 + 10);
        $this->Cell(20, 4, mytr("% VAT"), 0, 0, "C");

        $this->SetXY($r1, $y1 + 15);
        $this->Cell(20, 4, mytr("VAT Tax"), 0, 0, "C");

        $this->SetXY($r1, $y1 + 20);
        $this->Cell(20, 4, mytr("Total"), 0, 0, "C");
    }

    function completeTotals($amount, $percVat, $vat, $total)
    {
        $this->SetFont('Arial', '', 8);

        $re = $this->w - 50;
        $rf = $this->w - 29;
        $y1 = $this->h - 45;

        $this->SetFont("Arial", "", 10);

        $this->SetXY($re, $y1 + 5);
        $this->Cell(17, 4, $amount, '', '', 'R');

        $this->SetXY($re, $y1 + 10);
        $this->Cell(17, 4, $percVat, '', '', 'R');

        $this->SetXY($re, $y1 + 15);
        $this->Cell(17, 4, $vat, '', '', 'R');

        $this->SetXY($re, $y1 + 20);
        $this->Cell(17, 4, $total, '', '', 'R');
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