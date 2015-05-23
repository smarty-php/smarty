<?php /* Smarty version 3.1.24-dev/7, created on 2015-05-23 18:11:15
         compiled from "./templates/php_block_comment.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:178885560a6a3ad32d5_10964447%%*/
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
  'nocache_hash' => '178885560a6a3ad32d5_10964447',
  'has_nocache_code' => false,
  'version' => '3.1.24-dev/7',
  'unifunc' => 'content_5560a6a3ae6518_67021582',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_5560a6a3ae6518_67021582')) {
function content_5560a6a3ae6518_67021582 ($_smarty_tpl) {

$_smarty_tpl->properties['nocache_hash'] = '178885560a6a3ad32d5_10964447';
?>
--><?php echo '<?php ';?>echo ' hello world ';
/*
* comment <?php echo '<?php ';?>is okay
*
* comment <?php echo '<?php ';?>once again
*/

echo '<?php echo '<?php ';?>';

/*
* other comment <?php echo '<%';?> foo
*/

echo '<?php echo '?>';?> ';
$foo = 3;
<?php echo '?>';?><--<?php }
}
?>