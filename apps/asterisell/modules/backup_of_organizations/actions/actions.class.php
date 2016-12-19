<?php

/**
 * backup_of_organizations actions.
 *
 * @package    asterisell
 * @subpackage backup_of_organizations
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class backup_of_organizationsActions extends autoBackup_of_organizationsActions
{

    public function executeDownloadsql()
    {
        $id = $this->getRequest()->getParameter('id');
        return $this->performDownload(false, $id);
    }

    public function executeDownloadyaml()
    {
        $id = $this->getRequest()->getParameter('id');
        return $this->performDownload(true, $id);
    }

    /**
     * @param bool $isYAML
     * @param int|null $id
     */
    protected function performDownload($isYAML, $id)
    {
        if (!is_null($id)) {
            $subject = ArOrganizationBackupOfChangesPeer::retrieveByPK($id);

            if (!is_null($subject)) {
                if ($isYAML) {
                    $resultFileName = "backup_of_organizations_" . $id . ".yaml";
                    $content = $subject->getYamlExportAtDateInPlainText();
                } else {
                    $resultFileName = "backup_of_organizations_" . $id . ".sql";
                    $content = $subject->getSqlTablesInPlainText();
                }

                if (!is_null($content)) {
                    $this->setLayout(false);
                    sfConfig::set('sf_web_debug', false);
                    $this->getResponse()->clearHttpHeaders();
                    $this->getResponse()->setHttpHeader('Pragma: private', true);
                    $this->getResponse()->setHttpHeader('Content-Disposition', 'attachment; filename="' . $resultFileName . '"');
                    $this->getResponse()->setContentType('text/plain; charset=utf-8');
                    $this->getResponse()->sendHttpHeaders();
                    $this->getResponse()->setContent($content);

                    return sfView::NONE;
                } else {
                    $this->redirect('backup_of_organizations/edit?id=' . $id);
                }
            }
        }
        return sfView::SUCCESS;
    }
}
