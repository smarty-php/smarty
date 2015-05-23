<?php /* Smarty version 3.1.24-dev/7, created on 2015-05-23 18:11:15
         compiled from "./templates/php_block_comment.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:326065560a6a3b94eb7_16031521%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'f35234d65e91ddc6d8e8d0b2a04722231b0ec710' => 
    array (
      0 => './templates/php_block_comment.tpl',
      1 => 1432387784,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '326065560a6a3b94eb7_16031521',
  'has_nocache_code' => false,
  'version' => '3.1.24-dev/7',
  'unifunc' => 'content_5560a6a3b9aff9_10675968',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_5560a6a3b9aff9_10675968')) {
function content_5560a6a3b9aff9_10675968 ($_smarty_tpl) {

$_smarty_tpl->properties['nocache_hash'] = '326065560a6a3b94eb7_16031521';
?>
--><?php echo ' hello world ';
/*
* comment <?php is okay
*
* comment <?php once again
*/

echo '<?php ';

/*
* other comment <% foo
*/

echo '?> ';
$foo = 3;
?><--<?php }
}
?>