<?php

/**
 * @var ArReportSet $ar_report_set
 */

$conn = Propel::getConnection();
$stm = $conn->prepare('
SELECT
id,
produced_report_short_description,
produced_report_additional_description,
produced_report_already_reviewed,
produced_report_is_draft
FROM ar_report
WHERE about_ar_report_set_id = ?
AND produced_report_already_reviewed = ?
ORDER BY produced_report_short_description
');

$params = array(
    array('Summary reports to review and confirm', false),
    array('Summary reports already reviewed and confirmed (visible to users)', true)
);

$reportSetId = $ar_report_set->getId();

foreach ($params as $param) {

    list($title, $toReview) = $param;

    $stm->execute(array($reportSetId, $toReview));

    echo htmlspecialchars($title) . ': <ul>';

    while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
        $descr = $rs[1] . "\n" . $rs[2];
        echo '<li>' . link_to(htmlspecialchars($descr), 'report/edit?id=' . $rs[0]) . '</li>' ;
    }
    echo '</ul><br/><br/>';

    $stm->closeCursor();
}
