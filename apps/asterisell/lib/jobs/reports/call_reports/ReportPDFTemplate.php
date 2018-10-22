<?php
// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

include_once(getAsterisellCompleteRootDirectory() . '/ext_libs/fpdf/fpdf.php');

/**
 * A template for PDF generation of Reports.
 */
class ReportPDFTemplate extends FPDF
{
    ////////////
    // PARAMS //
    ////////////

    const SHOW_DRAFT_WATERMARK = false;

    protected $borderMargin;

    public function setDefaultBorderMargin()
    {
        $this->setBorderMargin(10);
    }

    public function setBorderMarginAccordingTableWidth($w)
    {
        $m = intval(($this->getPageWidth() - $w) / 2);
        $this->setBorderMargin($m);
    }

    public function setBorderMargin($m)
    {
        $this->borderMargin = $m;
        $this->SetMargins($m, 20);
    }

    public function getBorderMargin()
    {
        return $this->borderMargin;
    }

    protected function getNormalLineWidht()
    {
        return 0.2;
    }

    protected function getStrongLineWidth()
    {
        return 0.8;
    }

    protected function getPageWidth()
    {
        return 210;
    }

    public function getPageContentWidth()
    {
        return $this->getPageWidth() - ($this->getBorderMargin() * 2);
    }

    protected function getContentWidth()
    {
        return $this->getPageWidth() - $this->getBorderMargin() - $this->getBorderMargin();
    }

    ////////////////////////////
    // REPORT HEADER AND LOGO //
    ////////////////////////////

    /**
     * @var bool
     */
    protected $isFirstPage;

    /**
     * @var string
     */
    protected $companyName;

    /**
     * @var string
     */
    protected $companyAdresse;

    /**
     * @var string|null
     */
    protected $companyLogoFile;

    /**
     * @param string $companyName
     * @param string $companyAdresse
     * @param string|null $companyLogoFile
     */
    function setCompanyInfo($companyName, $companyAdresse, $companyLogoFile)
    {
        $this->companyName = $companyName;
        $this->companyAdresse = $companyAdresse;
        $this->companyLogoFile = $companyLogoFile;
    }

    ///////////
    // TABLE //
    ///////////

    public function getTableWidth()
    {
        return $this->getPageContentWidth();
    }

    //////////////////
    // Report State //
    //////////////////

    protected $isDraft;

    protected $rotationAngle = 0;

    //////////////////
    // Table Header //
    //////////////////

    /**
     * @var string
     */
    protected $reportHeaderLine1 = '';

    /**
     * @var bool
     */
    protected $processingTable;

    //////////////////
    // Table Format //
    //////////////////

    /**
     * @var int
     */
    protected $lineHeight;

    /**
     * @var float[] $widths
     */
    protected $widths;

    /**
     * @var string[] $aligns
     */
    protected $aligns;

    /**
     * @var string[] $alignsVendor
     */
    protected $alignsVendor;

    /**
     * True for rows that must be displayed
     *
     * @var bool[] $displayRow
     */
    protected $displayRow;

    /**
     * @var
     */
    protected $tableHeader;

    public function setTableWidths($w)
    {
        $this->widths = $w;
    }

    public function setTableAligns($v)
    {
        $this->aligns = $v;
    }

    public function setTableAlignsVendor($v)
    {
        $this->alignsVendor = $v;
    }

    public function setTableDisplay($v)
    {
        $this->displayRow = $v;
    }

    public function setTableHeader($v)
    {
        $this->tableHeader = $v;
    }

    public function setLineHeight($v)
    {
        $this->lineHeight = $v;
    }

    //////////////
    // Init PDF //
    //////////////

    function __construct()
    {
        parent::__construct('P', 'mm', 'A4');

        $this->setDefaultBorderMargin();

        $this->setIsDraft(false);
        $this->SetAutoPageBreak(true);

        $this->SetLineWidth($this->getNormalLineWidht());

        $this->AliasNbPages();

        $this->SetFont('Arial', '', 9);
        $this->SetTextColor(0);
        $this->SetFillColor(255);

        $this->lineHeight = 4;

        $this->processingTable = false;

        $this->isFirstPage = true;
    }

    /**
     * Called every time there is a new page.
     */
    function Header()
    {
        $border = $this->getBorderMargin();
        $x1 = $border;
        $y1 = $border;

        // Page number

        $w = 50;

        // Page number

        $this->SetFont('Arial', 'I', 8);

        $this->SetXY(0 - $this->getBorderMargin() - $w, $this->getBorderMargin());
        $this->Cell(0, $this->getBorderMargin(), 'Page ' . $this->PageNo() . '/{nb}', 0, 1, 'R');

        if ($this->isFirstPage) {
            $this->isFirstPage = false;


            // Display LOGO

            $insertLogoImage = true;
            $logoYXProportion = 0;
            if (is_null($this->companyLogoFile)) {
                $insertLogoImage = false;
            } else {
                $info = getimagesize($this->companyLogoFile);
                if ($info === FALSE) {
                    $insertLogoImage = false;
                } else {
                    $pixelWidth = $info[0];
                    $pixelHeight = $info[1];
                    $logoYXProportion = (1.0 * $pixelHeight) / (1.0 * $pixelWidth);
                }
            }

            if ($insertLogoImage) {
                $pageWidth = $this->getPageContentWidth();
                $width_LS = ($pageWidth / 4);
                $startY_L = $y1;
                $height_L = $width_LS * $logoYXProportion;
                $endY_L = $startY_L + $height_L;

                $this->Image($this->companyLogoFile, $x1, $y1, $width_LS);

                // add a space between logo and header
                $y1 = $endY_L + $border / 2;

                // write the company name on the right
                $x1 = $width_LS + $border + $border;
                $y1 = $startY_L;
            }

            $this->SetXY($x1, $y1);
            $this->SetFont('Arial', 'B', 8);
            $length = $this->GetStringWidth($this->companyName);
            $this->Cell($length, $this->lineHeight, $this->companyName);
            $this->SetXY($x1, $y1 + $this->lineHeight);
            $this->SetFont('Arial', '', 8);
            $length = $this->GetStringWidth($this->companyAdresse);

            $this->MultiCell($length, $this->lineHeight, $this->companyAdresse);

            // Draw a Line

            $this->SetXY($this->getBorderMargin(), $this->GetY() + $this->lineHeight);
            $this->Line($this->getBorderMargin(), $this->GetY(), $this->getPageWidth() - $this->getBorderMargin(), $this->GetY());
            $this->SetXY($this->getBorderMargin(), $this->GetY() + $this->lineHeight);

            $y1 = $this->GetY();
        }

        // Report Description

        $this->SetFont('Arial', 'B', 8);
        $this->SetXY($this->getBorderMargin(), $y1);
        $this->MultiCell(self::getContentWidth() - $w - $w, $this->lineHeight, $this->reportHeaderLine1, 0, 'L', false);

        $this->SetXY($this->getBorderMargin(), $this->GetY() + $this->lineHeight);
        $this->Line($this->getBorderMargin(), $this->GetY(), $this->getPageWidth() - $this->getBorderMargin(), $this->GetY());
        $this->SetXY($this->getBorderMargin(), $this->GetY() + $this->lineHeight);
    }

    function setBoldFont() {
        $this->SetFont('Arial', 'B', 8);
    }

    function setNormalFont() {
        $this->SetFont('Arial', '', 8);
    }

    /**
     * @param string $str
     */
    function addErrorInfo($str)
    {
        $this->SetFont('Arial', 'I', 8);

        $this->SetXY($this->getBorderMargin(), $this->GetY());

        $this->MultiCell(self::getContentWidth(), $this->lineHeight, $str, 0, 'L', false);

        $this->SetXY($this->getBorderMargin(), $this->GetY() + $this->lineHeight);
    }

    /**
     * @param string $str
     */
    function addInitialNotesInfo($str)
    {
        $this->addErrorInfo($str);
    }

    function addDebugInfo($str)
    {
        $this->SetFont('Arial', '', 8);

        $this->SetXY($this->getBorderMargin(), $this->GetY());

        $this->MultiCell(self::getContentWidth(), $this->lineHeight, 'DEBUG MESSAGE: ' . $str, 0, 'L', false);

        $this->SetXY($this->getBorderMargin(), $this->GetY() + $this->lineHeight);
    }

    /**
     * Called every time there is a new page.
     */
    function Footer()
    {
        // Put the watermark

        if (self::SHOW_DRAFT_WATERMARK) {
            if ($this->isDraft) {
                $this->SetFont('Arial', 'B', 50);
                $this->SetTextColor(255, 192, 203);
                $this->WatermarkRotatedText(35, 190, 'D r a f t    R e p o r t', 45);
                $this->SetTextColor(0);
            }
        }
    }

    /**
     * @param string $v
     */
    public function setReportHeaderLine1($v)
    {
        $this->reportHeaderLine1 = $v;
    }

    /**
     * @param bool $v
     */
    public function setIsDraft($v)
    {
        $this->isDraft = $v;
    }

    ///////////////////////
    // Watermark Support //
    ///////////////////////

    protected function WatermarkRotatedText($x, $y, $txt, $angle)
    {
        //Text rotated around its origin
        $this->Rotate($angle, $x, $y);
        $this->Text($x, $y, $txt);
        $this->Rotate(0);
    }

    function Rotate($angle, $x = -1, $y = -1)
    {
        if ($x == -1)
            $x = $this->x;
        if ($y == -1)
            $y = $this->y;
        if ($this->rotationAngle != 0)
            $this->_out('Q');
        $this->rotationAngle = $angle;
        if ($angle != 0) {
            $angle *= M_PI / 180;
            $c = cos($angle);
            $s = sin($angle);
            $cx = $x * $this->k;
            $cy = ($this->h - $y) * $this->k;
            $this->_out(sprintf('q %.5F %.5F %.5F %.5F %.2F %.2F cm 1 0 0 1 %.2F %.2F cm', $c, $s, -$s, $c, $cx, $cy, -$cx, -$cy));
        }
    }

    //////////////////
    // Create Table //
    //////////////////

    public function addTableHeader($withSmallText = false)
    {
        $this->processingTable = true;
        $y = $this->GetY();
        $this->SetXY($this->getBorderMargin(), $y);
        $this->addTableData($this->tableHeader, true, false, $withSmallText);
    }


    protected function configureText($isParent, $isHeaderGroupData, $useSmallText = false)
    {
        $black = 0;
        $gray = 230;
        $white = 255;
        $font = 'Arial';

        if ($useSmallText) {
            $size = 6;
        } else {
            $size = 8;
        }

        // Set color
        if ($isParent && !$isHeaderGroupData) {
            // This is the color of the header group data, that is the title of a section data.

            $this->SetFillColor($gray);
            $this->SetTextColor($black);
            $this->SetDrawColor($black);
            $this->SetFont($font, 'B', $size);
        } else {

            if ($isHeaderGroupData) {
                $style = 'B';
            } else {
                $style = '';
            }

            $this->SetFillColor($white);
            $this->SetTextColor($black);
            $this->SetDrawColor($black);
            $this->SetFont($font, $style, $size);
        }

    }

    /**
     * Add a long line, using the complete table width, with the title of the organization.
     *
     * @param string $name
     * @param bool $useSmallText
     * @return void
     */
    public function addTableLongLineName($name, $useSmallText = false)
    {

        $this->configureText(true, false, $useSmallText);

        $y = $this->GetY();
        $this->SetXY($this->getBorderMargin(), $y);
        $this->MultiCell($this->getTableWidth(), $this->lineHeight, $name, 0, 'C', true);

        $y = $this->GetY();
        $this->SetXY($this->getBorderMargin(), $y);
    }

    /**
     * Add a row, formatting according the current table format.
     *
     * @param string[] $row
     * @param bool $isParent true for an header with totals/parent, false for normal data. This is the strongest header, used for each organization to display
     * @param bool $isHeaderGroupData true for header group data, like vendors, channels and so on... This is the weakest header, used for grouping by vendor, by destination, and so on.
     * @param bool $useSmallText
     * @return void
     */
    public function addTableData($row, $isParent, $isHeaderGroupData, $useSmallText = false)
    {
        $this->configureText($isParent, $isHeaderGroupData, $useSmallText);
        if ($isHeaderGroupData) {
            $this->addTableRow($row, $this->displayRow, $this->widths, $this->alignsVendor);
        } else {
            $this->addTableRow($row, $this->displayRow, $this->widths, $this->aligns);
        }
    }

    /**
     * Add a row, formatting according the specified table format.
     *
     * @param string[] $row
     * @param bool[] $displayRow
     * @param float[] $widths
     * @param string[] $aligns
     * @return void
     */
    public function addTableRow($row, $displayRow, $widths, $aligns)
    {
        //Calculate the height of the row
        $nb = 0;
        for ($i = 0; $i < count($row); $i++) {
            if ($displayRow[$i]) {
                $nb = max($nb, $this->NbLines($widths[$i], $row[$i]));
            }
        }
        $h = $this->lineHeight * $nb;

        //Issue a page break first if needed
        $this->CheckPageBreak($h);

        $y = $this->GetY();
        $this->SetXY($this->getBorderMargin(), $y);

        //Draw the cells of the row
        for ($i = 0; $i < count($row); $i++) {
            if ($displayRow[$i]) {
                $w = $widths[$i];
                $a = isset($aligns[$i]) ? $aligns[$i] : 'L';

                //Save the current position
                $x = $this->GetX();
                $y = $this->GetY();

                //Print the text
                $this->MultiCell($w, $this->lineHeight, $row[$i], 0, $a, true);
                //Put the position to the right of the cell
                $this->SetXY($x + $w, $y);
            }
        }

        // draw the bottom line
        $bottomY = $this->GetY() + $h;
        $this->Line($this->getBorderMargin(), $bottomY, $this->getPageWidth() - $this->getBorderMargin(), $bottomY);

        //Go to the next line
        $this->Ln($h);
    }

    /**
     * Add an empty row used as separator
     */
    public function addTableRowSeparator()
    {
        $a = array();
        foreach ($this->displayRow as $v) {
            $a[] = '';
        }
        $this->addTableData($a, false, false);
    }

    function CheckPageBreak($h)
    {
        //If the height h would cause an overflow, add a new page immediately
        if ($this->GetY() + $h > $this->PageBreakTrigger)
            $this->AddPage($this->CurOrientation);
    }

    function NbLines($w, $txt)
    {
        //Computes the number of lines a MultiCell of width w will take
        $cw =& $this->CurrentFont['cw'];
        if ($w == 0)
            $w = $this->w - $this->rMargin - $this->x;
        $wmax = ($w - 2 * $this->cMargin) * 1000 / $this->FontSize;
        $s = str_replace("\r", '', $txt);
        $nb = strlen($s);
        if ($nb > 0 and $s[$nb - 1] == "\n")
            $nb--;
        $sep = -1;
        $i = 0;
        $j = 0;
        $l = 0;
        $nl = 1;
        while ($i < $nb) {
            $c = $s[$i];
            if ($c == "\n") {
                $i++;
                $sep = -1;
                $j = $i;
                $l = 0;
                $nl++;
                continue;
            }
            if ($c == ' ')
                $sep = $i;
            $l += $cw[$c];
            if ($l > $wmax) {
                if ($sep == -1) {
                    if ($i == $j)
                        $i++;
                } else
                    $i = $sep + 1;
                $sep = -1;
                $j = $i;
                $l = 0;
                $nl++;
            } else
                $i++;
        }
        return $nl;
    }

    //////////////////////
    // Bookmark Support //
    //////////////////////
    // from: http://www.fpdf.org/en/script/script1.php

    var $outlines = array();

    var $OutlineRoot;

    function Bookmark($txt, $level = 0, $y = 0)
    {
        if ($y == -1)
            $y = $this->GetY();
        $this->outlines[] = array('t' => $txt, 'l' => $level, 'y' => ($this->h - $y) * $this->k, 'p' => $this->PageNo());
    }

    function BookmarkUTF8($txt, $level = 0, $y = 0)
    {
        $this->Bookmark($this->_UTF8toUTF16($txt), $level, $y);
    }

    function _putbookmarks()
    {
        $nb = count($this->outlines);
        if ($nb == 0)
            return;
        $lru = array();
        $level = 0;
        foreach ($this->outlines as $i => $o) {
            if ($o['l'] > 0) {
                $parent = $lru[$o['l'] - 1];
                //Set parent and last pointers
                $this->outlines[$i]['parent'] = $parent;
                $this->outlines[$parent]['last'] = $i;
                if ($o['l'] > $level) {
                    //Level increasing: set first pointer
                    $this->outlines[$parent]['first'] = $i;
                }
            } else
                $this->outlines[$i]['parent'] = $nb;
            if ($o['l'] <= $level and $i > 0) {
                //Set prev and next pointers
                $prev = $lru[$o['l']];
                $this->outlines[$prev]['next'] = $i;
                $this->outlines[$i]['prev'] = $prev;
            }
            $lru[$o['l']] = $i;
            $level = $o['l'];
        }
        //Outline items
        $n = $this->n + 1;
        foreach ($this->outlines as $o) {
            $this->_newobj();
            $this->_out('<</Title ' . $this->_textstring($o['t']));
            $this->_out('/Parent ' . ($n + $o['parent']) . ' 0 R');
            if (isset($o['prev']))
                $this->_out('/Prev ' . ($n + $o['prev']) . ' 0 R');
            if (isset($o['next']))
                $this->_out('/Next ' . ($n + $o['next']) . ' 0 R');
            if (isset($o['first']))
                $this->_out('/First ' . ($n + $o['first']) . ' 0 R');
            if (isset($o['last']))
                $this->_out('/Last ' . ($n + $o['last']) . ' 0 R');
            $this->_out(sprintf('/Dest [%d 0 R /XYZ 0 %.2F null]', 1 + 2 * $o['p'], $o['y']));
            $this->_out('/Count 0>>');
            $this->_out('endobj');
        }
        //Outline root
        $this->_newobj();
        $this->OutlineRoot = $this->n;
        $this->_out('<</Type /Outlines /First ' . $n . ' 0 R');
        $this->_out('/Last ' . ($n + $lru[0]) . ' 0 R>>');
        $this->_out('endobj');
    }

    function _putresources()
    {
        parent::_putresources();
        $this->_putbookmarks();
    }

    function _putcatalog()
    {
        parent::_putcatalog();
        if (count($this->outlines) > 0) {
            $this->_out('/Outlines ' . $this->OutlineRoot . ' 0 R');
            $this->_out('/PageMode /UseOutlines');
        }
    }
}
