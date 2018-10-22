<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Generate a Call Report.
 */
class PDFCallReportTemplate extends FPDF
{
  const BORDER_MARGIN = 10;

  const SEP_MARGIN = 2.5;

  /**
   * the current height of a line.
   */
  private $lineHeight;

  private $invoice;
  private $seller;
  private $buyer;
  private $logoYXProportion;

  function __construct(ArInvoice $invoice) { 
    parent::__construct('P', 'mm', 'A4');

    $this->SetMargins(10, 20);

    $this->SetAutoPageBreak(true);
    $this->setToDefaultFont();

    $this->AliasNbPages();
    $this->setInvoice($invoice);
  }

  function getLogoFile() {
    $f = $this->seller->getLogoImageInInvoices();
    if (is_null($f)) {
      return null;
    } 

    if (strlen(trim($f)) == 0) {
      return null;
    }

    $r = sfConfig::get('sf_web_dir') . '/images/' . $f;

    return $r;
  }

  function setInvoice(ArInvoice $invoice) {
    $this->invoice = $invoice;
    $this->buyer = $invoice->getArParty();
    $this->seller = $this->buyer->getArParams();

    $logoFile = $this->getLogoFile();
    if (is_null($logoFile)) {
      $this->logoYXProportion = 0;
    } else {
      $info = getimagesize($logoFile);
      $pixelWidth = $info[0];
      $pixelHeight = $info[1];
      $this->logoYXProportion = (1.0 * $pixelHeight) / (1.0 * $pixelWidth);
    }
  }

  function getPageWidth() {
    return 210 - (PDFCallReportTemplate::BORDER_MARGIN * 2);
  } 

  function setToDefaultFont() {
    $this->SetFont('Arial', '', 8);
    $this->SetTextColor(0);
    $this->SetFillColor(255);

    $this->lineHeight = 4;
  }

  function setToTableCaptionFont() {
    $this->SetFont('Arial', 'B', 8);
    $this->SetTextColor(0);
    $this->SetFillColor(255);

    $this->lineHeight = 4;
  }

  function setToInvoiceHeaderFont() {
    $this->SetFont('Arial', '', 9);
    $this->SetTextColor(0);
    $this->SetFillColor(255);

    $this->lineHeight = 4;
  }

  function setToInvoiceHeaderBoldFont() {
    $this->SetFont('Arial', 'B', 9);
    $this->SetTextColor(0);
    $this->SetFillColor(255);

    $this->lineHeight = 4;
  }


  function setToTableHeaderFont() {
    $this->SetFont('Arial', 'B', 12);
    $this->SetTextColor(255);
    $this->SetFillColor(180);

    $this->lineHeight = 6;
  }

  function Header() {
    // FPDP bug: use SetXY instead of SetX and SetY

    $startX = PDFInvoiceCenteredTemplate::BORDER_MARGIN;
    $startY = PDFInvoiceCenteredTemplate::BORDER_MARGIN;

    $this->SetXY($startX, PDFInvoiceCenteredTemplate::BORDER_MARGIN / 3);
    $this->SetFont('Arial','I',9);
    $this->Cell($this->getPageWidth(), 4,'Page '.$this->PageNo(). ' of {nb}', '',0,'R');

    $this->SetXY($startX, $startY);

    $this->setToInvoiceHeaderFont();

    $border = PDFCallReportTemplate::SEP_MARGIN;
    $pageWidth = $this->getPageWidth();
 
    // Header layout is:
    //
    // +---------+-----------------------+--------------------------------------+
    // | (L)     | (S)                   | (B)                                  |
    // |  LOGO   | Service Provider Info | Customer Info                        |
    // |         |                       |                                      |
    // +---------+-----------------------+--------------------------------------+
    // | (I) Invoice Info                                                       |
    // +---------+-----------------------+--------------------------------------+
    //

    // (L) section
    //
    $width_LSB = ($pageWidth / 3) - $border - $border;
    $startX_L = $startX + $border;
    $endX_L = $startX_L + $width_LSB + $border;
    $startY_L = $startY + $border;
    $height_L = $width_LSB * $this->logoYXProportion;
    $endY_L = $startY_L + $height_L + $border;

    // (S) section
    //
    $this->setToInvoiceHeaderFont();
    $s= $this->seller->getLegalName() 
      . "\n" . $this->seller->getLegalAddress() 
      . "\n" . format_zip_city_address($this->seller->getLegalZipcode(), $this->seller->getLegalCity(), $this->seller->getLegalStateProvince(), $this->seller->getLegalCountry()) 
      . maybeAddIfExistsCentral("\nemail: ", $this->seller->getLegalEmail(), "")
      . maybeAddIfExistsCentral("\n" . mytr("tel:"), $this->seller->getLegalPhone(), "")
      . maybeAddIfExistsCentral("\nfax: ", $this->seller->getLegalFax(), "")
      . maybeAddIfExistsCentral("\n" . mytr("VAT") . ": ", $this->seller->getVat(), "");

    $startX_S = $endX_L + $border;
    $startY_S = $startY_L;
    $endX_S = $startX_S + $width_LSB + $border;
    $height_S = $this->lineHeight * (number_of_lines($s) + 1);
    $endY_S = $startY_L + $height_S;

    $this->setToInvoiceHeaderBoldFont();
    $this->SetXY($startX_S, $startY_S);
    $this->Cell($width_LSB, $this->lineHeight, mytr("Service Provider:"), 0, 'L');

    $this->setToInvoiceHeaderFont();
    $this->SetXY($startX_S, $startY_S + $this->lineHeight);
    $this->MultiCell($width_LSB, $this->lineHeight, $s, 0, 'L');

    // (B) section
    //    
    $this->setToInvoiceHeaderFont();
    $s= $this->buyer->getName() . "\n" . $this->buyer->getLegalAddress() . "\n" . format_zip_city_address($this->buyer->getLegalZipcode(), $this->buyer->getLegalCity(), $this->buyer->getLegalStateProvince(), $this->buyer->getLegalCountry()) . maybeAddIfExistsCentral("\n" . mytr("VAT:"),  $this->buyer->getVat(), "");

    $startX_B = $endX_S + $border;
    $endX_B = $startX_B + $width_LSB + $border;
    $startY_B = $startY_S;
    $height_B = $this->lineHeight * (number_of_lines($s) + 1);
    $endY_B = $startY_B + $height_B;

    $this->setToInvoiceHeaderBoldFont();
    $this->SetXY($startX_B, $startY_B);
    $this->Cell($width_LSB, $this->lineHeight, mytr("Customer:"), 0, 'L');

    $this->setToInvoiceHeaderFont();
    $this->SetXY($startX_B, $startY_B + $this->lineHeight);
    $this->MultiCell($width_LSB, $this->lineHeight, $s, 0, 'L');

    // Draw the (L) logo centered vertically according the height of (S) and (B) sections
    //
    $endY_LSB = $endY_L;
    if ($endY_LSB < $endY_S) {
      $endY_LSB = $endY_S;
    }

    if ($endY_LSB < $endY_B) {
      $endY_LSB = $endY_B;
    }

    $startY_centeredL = $startY_L;
    if ($endY_L < $endY_LSB) {
      $d = $endY_LSB - $endY_L;
      $startY_centeredL += ($d / 2);
    }

    $logoFile = $this->getLogoFile();
    if (! is_null($logoFile)) {
      $this->Image($logoFile, $startX_L, $startY_centeredL, $width_LSB);
    }

    // (I) section
    // 
    $startX_I = $startX_L;
    $startY_I = $endY_LSB + $border;
    $width_I =  $pageWidth - $border - $border;
    $height_I = $this->lineHeight * 2;
    $endY_I = $startY_I + $height_I + $border;

    $this->setToInvoiceHeaderBoldFont();
    $this->SetXY($startX_I, $startY_I);
    $this->Cell($width_I, $this->lineHeight, mytr("Call Details for:"), 0, 'L');

    $s= mytr("Invoice Number:") . " " . $this->invoice->getNr() . ", " . mytr("Date: ") . format_invoice_date_according_config($this->invoice->getInvoiceDate())
      . ", " . mytr("Billing Period from") . " " . format_invoice_date_according_config($this->invoice->getArCdrFrom()) . " " . mytr("to") . " " . format_invoice_date_according_config($this->invoice->getArCdrTo());

    $this->setToInvoiceHeaderFont();
    $this->SetXY($startX_I, $startY_I + $this->lineHeight);
    $this->MultiCell($width_I, $this->lineHeight, $s, 0, 'L');

    // Display frames
    //
    $headerHeight = $endY_I - $startY;
    $this->SetLineWidth(0.4);
    $this->RoundedRect($startX, $startY, $pageWidth, $headerHeight, 2.5, 'D');

    $this->SetLineWidth(0.2);
    $this->Line($endX_L, $startY, $endX_L, $endY_LSB);
    $this->Line($endX_S, $startY, $endX_S, $endY_LSB);
    $this->Line($startX, $endY_LSB, $endX_B, $endY_LSB);

    $this->SetXY($startX, $endY_I + $border);

    $this->setToDefaultFont();
    $this->Ln();

  }

  /**
   * Add a table in the current position.
   */
  function addCallTable($title, $colsNames, $colsNamesAlign, $colsAlign, $colsDim, $lines) {
    $tableWidth = $this->getPageWidth();

    $startY = $this->GetY();
    $startX = PDFCallReportTemplate::BORDER_MARGIN;

    // Display table header
    //
    $this->setToTableHeaderFont();
    $this->SetXY($startX, $startY);
    $this->Cell($tableWidth, $this->lineHeight, $title, 0, 1, 'L', true);

    $this->Ln();

    // Display table caption
    //
    $this->setToTableCaptionFont();
    
    $nrOfCols = count($colsNames);

    for ($i = 0; $i < $nrOfCols; $i++) {
      $this->Cell($colsDim[$i], $this->lineHeight, $colsNames[$i], 'B', 0, $colsNamesAlign[$i], 'L', false);
    }
    $this->Ln();

    // Display table content
    //
    $this->setToDefaultFont();

    foreach ($lines as $line) {
      $i = 0;
      foreach ($line as $cell) {
        $this->Cell($colsDim[$i], $this->lineHeight, $cell, '', 0, $colsAlign[$i], 'L', false);
        $i++;
      }
      $this->Ln();
    }
   
    $this->Ln();
  }

  // private functions
  function RoundedRect($x, $y, $w, $h, $r, $style = '')
  {
    $k = $this->k;
    $hp = $this->h;
    if($style=='F')
      $op='f';
    elseif($style=='FD' || $style=='DF')
      $op='B';
    else
      $op='S';
    $MyArc = 4/3 * (sqrt(2) - 1);
    $this->_out(sprintf('%.2F %.2F m',($x+$r)*$k,($hp-$y)*$k ));
    $xc = $x+$w-$r ;
    $yc = $y+$r;
    $this->_out(sprintf('%.2F %.2F l', $xc*$k,($hp-$y)*$k ));

    $this->_Arc($xc + $r*$MyArc, $yc - $r, $xc + $r, $yc - $r*$MyArc, $xc + $r, $yc);
    $xc = $x+$w-$r ;
    $yc = $y+$h-$r;
    $this->_out(sprintf('%.2F %.2F l',($x+$w)*$k,($hp-$yc)*$k));
    $this->_Arc($xc + $r, $yc + $r*$MyArc, $xc + $r*$MyArc, $yc + $r, $xc, $yc + $r);
    $xc = $x+$r ;
    $yc = $y+$h-$r;
    $this->_out(sprintf('%.2F %.2F l',$xc*$k,($hp-($y+$h))*$k));
    $this->_Arc($xc - $r*$MyArc, $yc + $r, $xc - $r, $yc + $r*$MyArc, $xc - $r, $yc);
    $xc = $x+$r ;
    $yc = $y+$r;
    $this->_out(sprintf('%.2F %.2F l',($x)*$k,($hp-$yc)*$k ));
    $this->_Arc($xc - $r, $yc - $r*$MyArc, $xc - $r*$MyArc, $yc - $r, $xc, $yc - $r);
    $this->_out($op);
  }

  function _Arc($x1, $y1, $x2, $y2, $x3, $y3)
  {
    $h = $this->h;
    $this->_out(sprintf('%.2F %.2F %.2F %.2F %.2F %.2F c ', $x1*$this->k, ($h-$y1)*$this->k,
			$x2*$this->k, ($h-$y2)*$this->k, $x3*$this->k, ($h-$y3)*$this->k));
  }


}
?>