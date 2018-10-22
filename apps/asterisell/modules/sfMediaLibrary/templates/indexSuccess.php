<?php use_helper('Javascript', 'I18N') ?>

<div id="sf_asset_container">
  <h1><?php echo __('Media library (%1%)', array('%1%' => $current_dir_slash), 'sfMediaLibrary') ?></h1>
  <div id="sf_admin_footer">
  <div id="asterisellHelp">

  <h2>Overview</h2>
    <p>Use this module to upload images and files to be used as part of the website interface and/or generated documents.</p>

  <h2>File Location</h2>
    <p>The directory where images and files are store in: "web/<?php echo sfConfig::get('app_sfMediaLibrary_upload_dir')?>". Images and files may also be uploaded directly to this location on the server. Note: In case images / files are directly uploaded to the server, ensure that the web server process can read and write the images / files.</p>

  <h2>Supported Formats</h2>
      <p> JPEG images </p>
      <p> PNG images without alpha channel (transparent layer)</p>

  <h2>Security</h2>
    <p>Note that all users will have access to uploaded images / files. Do not upload private content.</p>

  </div></div>
  <br/>
  
  <div id="sf_asset_content">

    <div id="sf_asset_controls">

      <?php echo form_tag('sfMediaLibrary/upload', 'class=float-left id=sf_asset_upload_form name=sf_asset_upload_form multipart=true') ?>
      <?php echo input_hidden_tag('current_dir', $currentDir) ?>
      <fieldset>
        <div class="form-row">
          <?php echo label_for('file', __('Add a file:', array(), 'sfMediaLibrary'), '') ?>
          <div class="content"><?php echo input_file_tag('file') ?></div>
        </div>
      </fieldset>

      <ul class="sf_asset_actions">
        <li><?php echo submit_tag(__('Add', array(), 'sfMediaLibrary'), array (
          'name'    => 'add',
          'class'   => 'sf_asset_action_add_file',
          'onclick' => "if($('file').value=='') { alert('".__('Please choose a file first', array(), 'sfMediaLibrary')."');return false; }",
        )) ?></li>
      </ul>

      </form>

      <?php echo form_tag('sfMediaLibrary/mkdir', 'class=float-left id=sf_asset_mkdir_form name=sf_asset_mkdir_form') ?>
      <?php echo input_hidden_tag('current_dir', $currentDir) ?>
      <fieldset>
        <div class="form-row">
          <?php echo label_for('dir', __('Create a dir:', array(), 'sfMediaLibrary'), '') ?>
          <div class="content"><?php echo input_tag('name', null, 'size=15 id=dir') ?></div>
        </div>
      </fieldset>

      <ul class="sf_asset_actions">
        <li><?php echo submit_tag(__('Create', array(), 'sfMediaLibrary'), array (
          'name'    => 'create',
          'class'   => 'sf_asset_action_add_folder',
          'onclick' => "if($('dir').value=='') { alert('".__('Please enter a directory name first', array(), 'sfMediaLibrary')."');return false; }",
        )) ?></li>
      </ul>

      </form>

    </div>

    <div id="sf_asset_assets">

      <?php include_partial('sfMediaLibrary/dirs', array('dirs' => $dirs, 'currentDir' => $currentDir, 'parentDir' => $parentDir, 'is_file' => (count($files) > 0))) ?>
      <?php include_partial('sfMediaLibrary/files', array('files' => $files, 'currentDir' => $currentDir, 'webAbsCurrentDir' => $webAbsCurrentDir, 'count' => count($dirs))) ?>

    </div>

  </div>

</div>
