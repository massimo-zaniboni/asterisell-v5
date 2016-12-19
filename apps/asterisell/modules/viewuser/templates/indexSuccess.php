<?php
use_helper('Number');
use_helper('I18N');
use_helper('Date');

//
// Display error messages
//

$message = '';
if ($sf_request->hasErrors()) {
  $errorNames = $sf_request->getErrorNames();
  foreach($errorNames as $errorName) {
    $errorDescr = $sf_request->getError($errorName);
    $message .= htmlentities($errorDescr) . '</br>';
  }
}

if (strlen($message) > 0) {
  echo '<div class="form-errors">';
  echo "<h2>$message</h2>";
  echo '</div>';
}

//
// Display notification messages
//

/*
if ($sf_user->hasFlash('notice')) {
  echo '<div class="save-ok">';
  echo '<h2>' . __($sf_user->getFlash('notice')) . '</h2>';
  echo '</div>';
}
*/

echo '<div id="loginHelp"> </div>';

// Display login form
//
echo '<div id="asterisellLogin">';
echo form_tag('viewuser/change');
echo '<table cellpadding="15" cellspacing="15">';
echo '<tr>' . '<td>' . __('Current Password:') . '</td>' . '<td>' . input_password_tag('current_password', null, array('class' => 'text')) . '</td>' . '</tr>';
echo '<tr>' . '<td>' . __('New Password:') . '</td>' . '<td>' . input_password_tag('new_password1', null, array('class' => 'text')) . '</td>' . '</tr>';
echo '<tr>' . '<td>' . __('Repeat Password:') . '</td>' . '<td>' . input_password_tag('new_password2', null, array('class' => 'text')) . '</td>' . '</tr>';
echo '<tr>' . '<td> </td>' . '<td><input type="submit" name="change" value="' . __("Change Password") . '" class="login"/></td></tr>';
echo '<tr>' . '<td> </td>' . '<td><input type="submit" name="return" value="' . __("Return to Call Report") . '" class="login"/></td></tr>';
echo '</table>';
echo '</form>';
echo '</div>';
