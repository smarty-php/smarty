<?php /* Smarty version 3.1.24-dev/7, created on 2015-05-23 18:11:15
         compiled from "./templates/php_line_comment.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:139405560a6a3985781_34109568%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'f14bc0bc443795e95eef36e1f9f126b927b218e9' => 
    array (
      0 => './templates/php_line_comment.tpl',
      1 => 1432387784,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '139405560a6a3985781_34109568',
  'has_nocache_code' => false,
  'version' => '3.1.24-dev/7',
  'unifunc' => 'content_5560a6a3998006_97311186',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_5560a6a3998006_97311186')) {
function content_5560a6a3998006_97311186 ($_smarty_tpl) {

$_smarty_tpl->properties['nocache_hash'] = '139405560a6a3985781_34109568';
?>
--><?php echo '<?php ';?>echo ' hello world ';
// comment <?php echo '<?php ';?>is okay
//
// comment <?php echo '<?php ';?>once again

echo '<?php echo '<?php ';?>';

// other comment <?php echo '<%';?> foo

echo '<?php echo '?>';?> ';
$foo = 3;
<?php echo '?>';?><--<?php }
}
?>