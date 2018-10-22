<?php

class debug_cdrActions extends autoDebug_cdrActions
{

    protected function getArCdrOrCreate($calldate = 'calldate', $is_service_cdr = 'is_service_cdr', $id = 'id')
    {
        $p_callDate = intval($this->getRequestParameter($calldate));
        $p_is_service_cdr = intval($this->getRequestParameter($is_service_cdr));
        $p_id = intval($this->getRequestParameter($id));
        $ar_cdr = ArCdrPeer::retrieveByPk1($p_callDate, $p_is_service_cdr, $p_id);

        if (is_null($ar_cdr)) {
          $this->forward404();
        } else {
            return $ar_cdr;
        }
    }
}
