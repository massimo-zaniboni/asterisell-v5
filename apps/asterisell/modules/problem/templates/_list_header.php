<?php
use_helper('I18N', 'Form');

echo '<table>';

echo '<tr>';
echo '<td>';
echo form_tag('problem/refreshView');
echo submit_tag(__('Refresh View'));
echo "</form>";
echo '</td>';

echo '<td>';
echo form_tag('problem/exportToFile');
echo submit_tag(__('Export to File'));
echo "</form>";
echo '</td>';

echo '<td>';
echo form_tag('problem/seeJobQueue');
echo submit_tag(__('See Job Log'));
echo '</form>';
echo '</td>';

echo '<td>';
echo form_tag('problem/deleteProblems');
echo submit_tag(__('Delete all Problems'));
echo "</form>";
echo '</td>';

echo '</tr>';

echo '</table>';

//
// Show if Problem List will be deleted.
//

if (FixedJobProcessor::getCleanErrorTable() != 0) {
  echo '<div id="allertMessage">Next execution of rating processor, will generate only new error messages, and old error messages will be not displayed again. This is useful for removing some informative messages that are not anymore important, and for removing already solved errors that sometime can be still in the error list.</div>';
}

//
// Show a table with the summary of problems
//

$problemSummary = ArCurrentProblemPeer::getProblemSummaryByType();

echo '<table cellspacing="0" class="sf_admin_list"><thead><tr>';

echo '<th>Overview</th></tr></thead>';

echo '<tbody>';

foreach($problemSummary as $summary) {
    list($severityLevelName, $numberOfProblems, $responsibleName, $problemDomainName, $severityLevelId, $responsibleId, $domainId) = $summary;

    if ($severityLevelId != ArProblemType::TYPE_INFO) {
    echo '<tr class="sf_admin_row_0">'
            . '<td>' . $numberOfProblems
            . ' '
            . htmlspecialchars($problemDomainName, ENT_QUOTES, 'UTF-8')
            . ' '
            . htmlspecialchars($severityLevelName, ENT_QUOTES, 'UTF-8') . '(s)'
            . ', with responsible '
            . htmlspecialchars($responsibleName, ENT_QUOTES, 'UTF-8')
            . '</td></tr>';
    }
}

echo '</tbody></table>';
echo '<br/>';

