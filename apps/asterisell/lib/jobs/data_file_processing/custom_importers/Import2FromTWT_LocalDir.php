<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Similar to Import2FromTWT_FTP_Server but work on local directory.
 */
abstract class Import2FromTWT_LocalDir extends ImportCSVFilesFromLocalServer
{

    /**
     * @return Import2FromTWT_FTP_Server job to use knowing what to do.
     */
    abstract public function getWrappedJob();

    protected $cachedJob = null;
    
    /**
     * @return Import2FromTWT_FTP_Server
     */
    public function getCachedWrappedJob() {
      if (is_null($this->cachedJob)) {
          $this->cachedJob = $this->getWrappedJob();
      }
      
      return $this->cachedJob;
    }     
        
    public function getSourceCharacterEncoding() {
        return $this->getCachedWrappedJob()->getSourceCharacterEncoding();
    }

    public function getTWTAccount() {
        return $this->getCachedWrappedJob()->getTWTAccount();
    }

    public function canAcceptFileName($n) {
        return $this->getCachedWrappedJob()->canAcceptFileName($n);
    }
    
    public function getDefaultLogicalType() {
        return $this->getCachedWrappedJob()->getDefaultLogicalType();
    }

    public function getPhysicalType($n) {
        return $this->getCachedWrappedJob()->getPhysicalType($n);
    }
    
    public function normalizeFileContent($remoteFileName, $sourceFileName, $destFileName) {
        return $this->getCachedWrappedJob()->normalizeFileContent($remoteFileName, $sourceFileName, $destFileName);
    }

}
