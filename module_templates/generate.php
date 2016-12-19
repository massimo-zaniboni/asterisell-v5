<?php

$appsDirectory = realpath("../apps/asterisell");

$destinationDirectory = $appsDirectory . '/modules';

require_once($appsDirectory . '/lib/FieldsToShow.php');

// Call Report - Template

copyModule("call_report_template", FieldsToShow::getModuleName(true));
copyModule("call_report_template", FieldsToShow::getModuleName(false));

processAllCallReportTemplate("config/generator.yml");
processAllCallReportTemplate("config/security.yml");
processAllCallReportTemplate("templates/_list_header.php");
processAllCallReportTemplate("templates/_list.php");
processAllCallReportTemplate("templates/listSuccess.php");
processAllCallReportTemplate("actions/actions.class.php");
processAllCallReportTemplate("templates/_filter_on_destination_type.php");
processAllCallReportTemplate("templates/_filter_on_vendor_cost.php");
processAllCallReportTemplate("templates/exportToCsvSuccess.php");

// Support Functions

function processAllCallReportTemplate($srcFile) {
  processCallReportTemplate($srcFile, "admin");
  processCallReportTemplate($srcFile, "customer");
}

function processCallReportTemplate($srcFile, $procType) {
  processTemplate("call_report_template", $srcFile, "$procType ", $procType . "_call_report");
}

function processTemplate($srcModule, $srcFile, $processingParams, $dstModule) {
  global $destinationDirectory;

  my_shell_exec("cd $srcModule; php $srcFile $processingParams > $destinationDirectory/$dstModule/$srcFile");
  @unlink("$destinationDirectory/$dstModule/generator_header.php");
}

function copyModule($srcName, $dstName) {
  global $destinationDirectory;

  copyDirectoryContent($srcName, $destinationDirectory . "/" . $dstName);
}

function copyDirectoryContent($src, $dst) {
  my_shell_exec("mkdir $dst");

  $src1 = realpath($src);
  $dst1 = realpath($dst);

  if (!is_null($dst1) && strlen(trim($dst1)) > 0) {
    if (!is_null($src1) && strlen(trim($src1)) > 0) {
      my_shell_exec("rm -r -f $dst1/*");
      my_shell_exec("cp -r $src1/* $dst1/.");
    } else {
      echo "\nWARNING: Directory $src does not exists!";
    }
  } else {
    echo "\nWARNING: Directory $dst does not exists!";
  }
}

function my_shell_exec($cmd) {
  echo "$cmd\n";
  shell_exec($cmd);
}
