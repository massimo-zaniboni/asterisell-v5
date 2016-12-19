<?php

  // Symfony in current version does not manage this, so...
  //
if ($sf_flash->has('error')) {
  echo '<div class="form-errors">';
  echo '<h2>' . $sf_flash->get('error') . '</h2>';
  echo '</div>';
}

?>
