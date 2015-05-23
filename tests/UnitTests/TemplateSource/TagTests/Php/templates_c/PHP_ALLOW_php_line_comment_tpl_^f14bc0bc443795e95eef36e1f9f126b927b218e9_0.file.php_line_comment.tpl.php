<?php /* Smarty version 3.1.24-dev/7, created on 2015-05-23 18:11:15
         compiled from "./templates/php_line_comment.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:133305560a6a3a3a704_42353753%%*/
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
  'nocache_hash' => '133305560a6a3a3a704_42353753',
  'has_nocache_code' => false,
  'version' => '3.1.24-dev/7',
  'unifunc' => 'content_5560a6a3a405e0_30236030',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_5560a6a3a405e0_30236030')) {
function content_5560a6a3a405e0_30236030 ($_smarty_tpl) {

$_smarty_tpl->properties['nocache_hash'] = '133305560a6a3a3a704_42353753';
?>
--><?php echo ' hello world ';
// comment <?php is okay
//
// comment <?php once again

echo '<?php ';

// other comment <% foo

echo '?> ';
$foo = 3;
?><--<?php }
}
?>