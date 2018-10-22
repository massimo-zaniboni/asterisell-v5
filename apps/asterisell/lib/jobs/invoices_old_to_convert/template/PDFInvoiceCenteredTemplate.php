<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Generate an Invoice using a centered template, with legal terms 
 * and that supports multiple pages.
 */
class PDFInvoiceCenteredTemplate extends FPDF
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

    $this->setAutoPageBreak(true);
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
      $this->logoYXProprtion = 0;
    } else {
      $info = getimagesize($logoFile);
      $pixelWidth = $info[0];
      $pixelHeight = $info[1];
      $this->logoYXProprtion = (1.0 * $pixelHeight) / (1.0 * $pixelWidth);
    }
  }

  function getPageWidth() {
    return 210 - (PDFInvoiceCenteredTemplate::BORDER_MARGIN * 2);
  } 

  function setToDefaultFont() {
    $this->SetFont('Arial', '', 10);
    $this->SetTextColor(0);
    $this->SetFillColor(255);

    $this->lineHeight = 5.5;
  }

  function setToInvoiceHeaderFont() {
    $this->SetFont('Arial', '', 10);
    $this->SetTextColor(0);
    $this->SetFillColor(255);

    $this->lineHeight = 5.5;
  }

  function setToInvoiceHeaderBoldFont() {
    $this->SetFont('Arial', 'B', 10);
    $this->SetTextColor(0);
    $this->SetFillColor(255);

    $this->lineHeight = 5.5;
  }

  function setToFontInvoiceSectionTitle() {
    $this->SetFont('Arial', 'B', 10);
    $this->SetTextColor(0);
    $this->SetFillColor(255);

    $this->lineHeight = 5.5;
  }

  function draw($lines) {
    // FPDP bug: use SetXY instead of SetX and SetY

    $startX = PDFInvoiceCenteredTemplate::BORDER_MARGIN;
    $startY = PDFInvoiceCenteredTemplate::BORDER_MARGIN;

    $this->SetXY($startX, $startY);

    $this->setToInvoiceHeaderFont();

    $border = PDFInvoiceCenteredTemplate::SEP_MARGIN;
    $pageWidth = $this->getPageWidth();
 
    // Header layout is:
    //
    // +---------+--------------------------------+
    // | (L)     | (S)                            |
    // |  LOGO   | Service Provider Info          |
    // |         |                                |
    // +---------+--------------------------------+
    //
    //   (B) Buyer info          (I) Invoice Info
    //
    //        (D) Invoice Details
    //
    //        Mobile Line          1203.33
    //        Fixed Line           1234.23
    //
    //        (T) Invoice Totals
    //
    //        Total Call Charges   13123.33
    //        Total VAT (19.5%)      123.33
    //
    //        Totals               55345.43 
    //                                                            
    // 
    //             (N) Invoice Notes
    //             (P) Payment Terms
    

    // (L) section
    //
    $width_LS = ($pageWidth / 3) - $border - $border;
    $startX_L = $startX + $border;
    $endX_L = $startX_L + $width_LS + $border;
    $width_L = $endX_L - $startX_L;
    $startY_L = $startY + $border;
    $height_L = $width_LS * $this->logoYXProprtion;
    $endY_L = $startY_L + $height_L + $border;

    // (S) section
    //
    $s= $this->seller->getLegalName() 
      . "\n" . $this->seller->getLegalAddress() 
      . "\n" . format_zip_city_address($this->seller->getLegalZipcode(), $this->seller->getLegalCity(), $this->seller->getLegalStateProvince(), $this->seller->getLegalCountry()) 
      . maybeAddIfExistsCentral("\n" . mytr("email:"), $this->seller->getLegalEmail(), "")
      . maybeAddIfExistsCentral("\n" . mytr("tel:"), $this->seller->getLegalPhone(), "")
      . maybeAddIfExistsCentral("\n" . mytr("fax:"), $this->seller->getLegalFax(), "")
      . maybeAddIfExistsCentral("\n" . mytr("VAT:"), $this->seller->getVat(), "");

    $startX_S = $this->getPageWidth() / 2;
    $startY_S = $startY_L;
    $endX_S = $startX_S + $width_LS + $border;
    $height_S = $this->lineHeight * number_of_lines($s);
    $endY_S = $startY_S + $height_S + $border;

    $this->setToInvoiceHeaderBoldFont();
    $this->SetXY($startX_S, $startY_S);
    $this->MultiCell($width_LS, $this->lineHeight, $s, 0, 'L');

    // Draw the (L) logo centered vertically according the height of (S) and (B) sections
    //
    $endY_LS = $endY_L;
    if ($endY_LS < $endY_S) {
      $endY_LS = $endY_S;
    }

    $startY_centeredL = $startY_L;
    if ($endY_L < $endY_LS) {
      $d = $endY_LS - $endY_L;
      $startY_centeredL += ($d / 2);
    }

    $logoFile = $this->getLogoFile();
    if (! is_null($logoFile)) {
      $this->Image($logoFile, $startX_L, $startY_centeredL, $width_LS);
    }

    // Display frame
    //
    $headerHeight = $endY_LS - $startY;
    $this->SetLineWidth(0.4);
    $this->RoundedRect($startX, $startY, $pageWidth, $headerHeight, 2.5, 'D');

    // (B) section
    //    
    $startX_B = $startX + $border;
    $endX_B = $this->getPageWidth() / 2 - $border;
    $width_B = $endX_B + $startX_B;
    $startY_B = $endY_LS + $border + $border;

    $this->SetXY($startX_B, $startY_B);
    $this->setToFontInvoiceSectionTitle();
    $this->Cell($width_B, $this->lineHeight, 'Bill To:', 0, 2, 'L');

    $y = $this->GetY();    
    
    $s= $this->buyer->getName() . "\n" . $this->buyer->getLegalAddress() . "\n" . format_zip_city_address($this->buyer->getLegalZipcode(), $this->buyer->getLegalCity(), $this->buyer->getLegalStateProvince(), $this->buyer->getLegalCountry()) . "\n" . mytr("VAT:") . " " . $this->buyer->getVat();

    $this->setToDefaultFont();
    $this->SetXY($startX_B, $y);
    $this->MultiCell($width_B, $this->lineHeight, $s, 0, 'L');

    $endY_B = $this->GetY();
    $heightY_B = $endY_B - $startY_B;

    // (I) section
    // 
    $startX_I_label = $endX_B + $border;
    $startY_I_label = $startY_B;
    $width_I_label =  $this->getPageWidth() / 4;

    $startX_I_data = $startX_I_label + $width_I_label;
    $startY_I_data = $startY_I_label;
    $width_I_data = $width_I_label;

    $this->setToFontInvoiceSectionTitle();
    $this->SetXY($startX_I_label, $startY_I_label);
    $this->Cell($width_I_label, $this->lineHeight, mytr('Invoice:'), 0, 2, 'L');

    $y = $this->GetY();    
    $this->setToDefaultFont();
    $this->SetXY($startX_I_label, $y);
    $this->Cell($width_I_label, $this->lineHeight, mytr('Number:'), 0, 0, 'L');
    $this->SetXY($startX_I_data, $y);
    $this->Cell($width_I_data, $this->lineHeight, $this->invoice->getNr(), 0, 2, 'R');

    $y = $this->GetY();    
    $this->SetXY($startX_I_label, $y);
    $this->Cell($width_I_label, $this->lineHeight, mytr('Date (And Tax Point):'), 0, 0, 'L');
    $this->SetXY($startX_I_data, $y);
    $this->Cell($width_I_data, $this->lineHeight, format_invoice_date_according_config($this->invoice->getInvoiceDate()), 0, 2, 'R');

    $y = $this->GetY();    
    $this->SetXY($startX_I_label, $y);
    $this->Cell($width_I_label, $this->lineHeight, mytr('Billing Period:'), 0, 0, 'L');
    $this->SetXY($startX_I_data, $y);
    $this->Cell($width_I_data, $this->lineHeight, mytr("from") . " " . format_invoice_date_according_config($this->invoice->getArCdrFrom()), 0, 2, 'R');

    $y = $this->GetY();    
    $this->SetXY($startX_I_label, $y);
    $this->Cell($width_I_label, $this->lineHeight, 'Billing Period:', 0, 0, 'L');
    $this->SetXY($startX_I_data, $y);
    $this->Cell($width_I_data, $this->lineHeight, mytr("to") . "  " . format_invoice_date_according_config($this->invoice->getArCdrTo()), 0, 2, 'R');

    $crmCode = $this->buyer->getExternalCrmCode();
    if (! is_null($crmCode)) {
      if (strlen(trim($crmCode)) > 0) {
	$y = $this->GetY();    
	$this->SetXY($startX_I_label, $y);
	$this->Cell($width_I_label, $this->lineHeight, mytr('Customer Account Number:'), 0, 0, 'L');
	$this->SetXY($startX_I_data, $y);
	$this->Cell($width_I_data, $this->lineHeight, $crmCode, 0, 2, 'R');
      }
    }

    $endY_I = $this->GetY();
    $height_I = $endY_I - $startY_I_label;

    $endY_BI = $endY_B;
    if ($endY_BI < $endY_I) {
      $endY_BI = $endY_I;
    }
    
    $height_BI = $endY_BI - $endY_I;

    // Blank lines after (B-I) section.
    //
    $this->SetXY($startX, $endY_BI);
    $this->Ln();

    // (D) Invoice details
    //
    $this->setToDefaultFont();
    $width_D_label = $this->getPageWidth() / 2;
    $width_D_data = $this->getMaxStringWidth($lines);

    $startY_D = $this->GetY();

    $startX_D_label = $startX_B;
    $startX_D_data = $startX_I_label;

    $this->setToFontInvoiceSectionTitle();
    $y = $this->GetY();
    $this->setXY($startX_D_label, $y);
    $this->Cell($width_D_label, $this->lineHeight, mytr('Invoice Details:'), '', 2, 'L');

    $this->setToDefaultFont();
    foreach ($lines as $line) {
      $descr = $line[0];
      $cost = $line[1];

      $y = $this->GetY();
      $this->SetXY($startX_D_label, $y);
      $this->Cell($width_D_label, $this->lineHeight, $descr, 0, 0, 'L');

      $this->SetXY($startX_D_data, $y);
      $this->Cell($width_D_data, $this->lineHeight, $cost, 0, 0, 'R');
      $this->Ln();
    }
   
    $this->Ln();

    $this->setToFontInvoiceSectionTitle();
    $y = $this->GetY();
    $this->setXY($startX_D_label, $y);
    $this->Cell($width_D_label, $this->lineHeight, mytr('Invoice Totals:'), '', 2, 'L');

    $this->setToDefaultFont();
    $y = $this->GetY();
    $invoice = $this->invoice;

    $this->setToDefaultFont();
    $this->SetXY($startX_D_label, $y);
    $this->Cell($width_D_label, $this->lineHeight, mytr("Total Call Charges"), 0, 0, 'L');
    $this->SetXY($startX_D_data, $y);
    $this->Cell($width_D_data, $this->lineHeight, from_db_decimal_to_pdf_txt_decimal($invoice->getTotalWithoutTax()), 0, 0, 'R');
    $this->Ln();

    $y = $this->GetY();
    $this->SetXY($startX_D_label, $y);
    $this->Cell($width_D_label, $this->lineHeight, mytr("VAT") . " (" . from_db_decimal_to_vat_perc_according_culture($invoice->getVatPerc()) . '%' . ")", 0, 0, 'L');
    $this->SetXY($startX_D_data, $y);
    $this->Cell($width_D_data, $this->lineHeight, from_db_decimal_to_pdf_txt_decimal($invoice->getTotalVat()), 0, 0, 'R');
    $this->Ln();

    $this->setToFontInvoiceSectionTitle();
    $y = $this->GetY();
    $this->SetXY($startX_D_label, $y);
    $this->Cell($width_D_label, $this->lineHeight, mytr("Total"), 0, 0, 'L');
    $this->SetXY($startX_D_data, $y);
    $this->Cell($width_D_data, $this->lineHeight, from_db_decimal_to_pdf_txt_decimal($invoice->getTotal()), 0, 0, 'R');
    $this->Ln();

    $this->Ln();

    // Footer
    //
    $this->Ln();
    $this->Ln();

    $this->setToInvoiceHeaderBoldFont();

    $this->SetX($startX_B);
    $this->MultiCell($this->getPageWidth(), 
		     $this->lineHeight, 
		     from_user_string_to_php_string($this->seller->getInvoiceNotes() . "\n\n" . $this->seller->getInvoicePaymentTerms())
		     , 0, 'L');

  }

  function getMaxStringWidth($lines) {
    $r = 0;
    foreach ($lines as $line) {
      $t = $this->GetStringWidth($line[1]);
      if ($r < $t) {
        $r = $t;
      }
    }
    return $r;
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