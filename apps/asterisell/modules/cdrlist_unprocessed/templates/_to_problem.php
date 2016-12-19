<?php
/**
 * @var ArCdr $ar_cdr
 */
$t = $ar_cdr->getId();
echo link_to('go to error details', 'debug_cdr/edit?id=' . $t);
