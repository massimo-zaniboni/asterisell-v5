<?php
/**
 * @var ArReport $ArReport
 */

// Complete the list of available reports

$phpClassOptions = ReportGenerator::getAllReportsClassNameAndDescription();

$options = array('' => '');
$defaultOption = '';

foreach ($phpClassOptions as $className => $description) {
    $options[$className] = $description;
}

// Select in the list the current report

/**
 * @var string $reportName
 */
$reportName = $ArReport->getPhpClassName();

if (is_null($reportName)) {
    $defaultOption = '';
} else {
    // Is the current report method available?
    //
    $defaultOption = $reportName;
    if (!array_key_exists($reportName, $options)) {
        // use the class name as description
        //
        $options[$reportName] = $reportName;
    }
}

// Add selection and related buttons

echo select_tag('select_premade_report_generator', options_for_select($options, $defaultOption));

