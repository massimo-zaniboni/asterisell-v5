<?php

sfLoader::loadHelpers(array('I18N', 'Debug', 'Asterisell'));

class documentActions extends autodocumentActions
{

    public function updateArDocumentFromRequest()
    {
        $document = $this->ar_document;

        $fileName = $this->getRequest()->getFileName('file');

        if (! is_null($fileName) && strlen($fileName) > 0) {
            // XXX they are not written in web directory
            // XXX they are not deleted from the directory
            $destFile = getAsterisellRootDirectory() . '/' . sfConfig::get('app_sfMediaLibrary_upload_dir') . '/' . 'temp-' . time();

            // put file in temporary directory

            $fileMimeType = $this->getRequest()->getFileType('file');
            $this->getRequest()->moveFile('file', $destFile);

            // put file content in blob object
            $document->setFileName($fileName);
            $document->setDocumentContent(file_get_contents($destFile));
            $document->setMimeType($fileMimeType);
            $document->setAlreadyOpened(false);

            // delete temporary file

            unlink($destFile);

        }

        return parent::updateArDocumentFromRequest();
    }

    public function executeDownload() {
         // being sure no other content wil be output

        $id = $this->getRequest()->getParameter('id');
        if (!is_null($id)) {
            $document = ArDocumentPeer::retrieveByPK($id);
            if (! is_null($document)) {
                if ($document->isThereAttachedDocument()) {
                    $this->setLayout(false);
                    sfConfig::set('sf_web_debug', false);

                    // Adding the file to the Response object
                    $this->getResponse()->clearHttpHeaders();
                    $this->getResponse()->setHttpHeader('Pragma: private', true);
                    $this->getResponse()->setHttpHeader('Content-Disposition', 'attachment; filename="'. $document->getFileName() .'"');
                    $this->getResponse()->setContentType($document->getMimeType() . '; charset=utf-8');
                    $this->getResponse()->sendHttpHeaders();
                    $this->getResponse()->setContent($document->getDocumentContent());

                    return sfView::NONE;
                }
            }
        }

        return $this->forward('document', 'list');
    }
}
