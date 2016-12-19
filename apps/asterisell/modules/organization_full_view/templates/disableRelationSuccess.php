<h1>Disable Permission</h1>

<form action="<?php echo $sf_data->getRaw('disableRelationUrl'); ?>" method="post">
<table>
<?php echo $form ?>
<tr>
<td colspan="2">
<input type="submit" name="cancel" value="Cancel"/>
<input type="submit" name="save" value="Save"/>
</td>
</tr>
</table>
</form>
