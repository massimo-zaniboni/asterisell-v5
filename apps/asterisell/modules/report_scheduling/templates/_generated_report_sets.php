<?php

/**
 * @var ArReportScheduler $ArReportScheduler
 */

$conn = Propel::getConnection();
$stm = $conn->prepare('
SELECT
id,
from_date,
must_be_reviewed
FROM ar_report_set
WHERE ar_report_scheduler_id = ?
AND must_be_reviewed = ?
ORDER BY to_date DESC
LIMIT 5
');

$params = array(
    array('Last Reports Sets to Review', true),
    array('Last Reports Sets Already Reviewed', false)
);

$reportSchedulerId = $ArReportScheduler->getId();

foreach ($params as $param) {

    list($title, $toReview) = $param;

    $stm->execute(array($reportSchedulerId, (int)$toReview));

    echo htmlspecialchars($title) . ': <ul>';

    while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
        $descr = 'from date ' . fromUnixTimestampToSymfonyStrTimestamp(fromMySQLTimestampToUnixTimestamp($rs[1]));
        echo '<li>' . link_to(htmlspecialchars($descr), 'report_set/edit?id=' . $rs[0]) . '</li>';
    }
    echo '</ul><br/><br/>';

    $stm->closeCursor();
}
