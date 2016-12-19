<?php

/**
 * Skeleton subclass for representing a row from the 'ar_report' table.
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArReport extends BaseArReport
{

    /**
     * Initializes internal state of ArReport object.
     * @see        parent::__construct()
     */
    public function __construct()
    {
        // Make sure that parent constructor is always invoked, since that
        // is where any default values for this object are set.
        parent::__construct();
    }

    public function __toString()
    {
        return $this->getName();
    }

    public function getName()
    {
        return $this->getProducedReportShortDescription();
    }

    /**
     * @param      string $d new value
     * @return     ArReport The current object (for fluent API support)
     */
    public function setDocumentContent($d)
    {
        $this->setProducedReportGenerationDate(time());

        // use a ascii friendly format for MySQL dump
        $this->setProducedReportDocument(base64_encode(gzcompress($d)));
    }

    /**
     * @param string $s "text/plain" for inserting a text directly in the email,
     * "application/pdf" for adding a PDF attachment, and so on.
     * @return ArReport|void
     */
    public function setProducedReportMimeType($s)
    {
        return parent::setProducedReportMimeType($s);
    }

    /**
     * @return string|null
     */
    public function getProducedReportFileName()
    {
        if ($this->isThereDocument()) {

            $isDefaultReportFileName = false;
            $reportFileName = trim($this->getReportAttachmentFileName());

            if (isEmptyOrNull($reportFileName)) {
                $reportFileName = 'report';
                $isDefaultReportFileName = true;
            }

            if ($this->getReportAttachmentFileNameAddReportDate()) {
                $t1 = fromMySQLTimestampToUnixTimestamp($this->getFromDate());
                $t2 = date('Y', $t1) . '-' . date('m', $t1) . '-' . date('d', $t1);
                if ($isDefaultReportFileName) {
                    $reportFileName .= "_";
                }
                $reportFileName .= $t2;
            }

            $reportFileName .= '.' . $this->getProducedReportFileTypeSuffix();

            return $reportFileName;
        } else {
            return null;
        }
    }

    /**
     * @return bool
     */
    public function isThereDocument()
    {
        if (is_null($this->getProducedReportDocument())) {
            return FALSE;
        }
        return TRUE;
    }

    public function getDocumentContent()
    {
        $d = $this->getProducedReportDocument();
        if (is_null($d)) {
            return null;
        }

        $s = stream_get_contents($d);
        if (strlen($s) == 0) {
            return null;
        }

        return gzuncompress(base64_decode($s));
    }

    /**
     * @return ReportGenerator|null
     */
    public function getReportGenerator()
    {
        $className = $this->getPhpClassName();
        if (!isEmptyOrNull($className)) {
            $reportGenerator = new $className();
            if ($reportGenerator === false) {
                return null;
            } else if ($reportGenerator instanceof ReportGenerator) {
                $reportGenerator->setArReport($this);
                return $reportGenerator;
            } else {
                return null;
            }
        }

        return null;
    }

    /**
     * @param PropelPDO $conn
     * @param ReportCalcStore|null $store non null for reusing the store used in a similar report
     * @return ReportCalcStore|null
     * @throws ArProblemException
     * precondition ! is_null($this->getId())
     */
    public function generateDocument(PropelPDO $conn, $store = null)
    {
        $generator = $this->getReportGenerator();
        if (!is_null($generator)) {
            if (!is_null($store)) {
                $generator->setStore($store);
            }
            $generator->generateReport($this, $conn);
            return $generator->getStore();
        } else {
            return null;
        }
    }

    public function save(PropelPDO $conn = null)
    {
        if (is_null($this->getArReportOrderOfChildrenId())) {
            $this->setArReportOrderOfChildrenId(ArReportOrderOfChildren::getDefaultOrder());
        }

        $generator = $this->getReportGenerator();
        if (!is_null($generator)) {
            $generator->generateOnlyNames();
        } else {
            if (isEmptyOrNull($this->getProducedReportShortDescription())) {
                $this->setProducedReportShortDescription('');
            }

            if (isEmptyOrNull($this->getProducedReportAdditionalDescription())) {
                $this->setProducedReportAdditionalDescription('');
            }
        }

        return parent::save($conn);
    }

} // ArReport
