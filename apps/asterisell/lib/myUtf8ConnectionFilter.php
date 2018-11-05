<?php
class myUtf8ConnectionFilter extends sfFilter
{
    /**
     * @param PDOStatement $filterChain
     */
    public function execute($filterChain)
  {
    $con = Propel::getConnection();
    if ($con){
        $con->exec("SET NAMES 'utf8mb4' COLLATE 'utf8mb4_bin'");
    }
    $filterChain->execute();
  }
}
