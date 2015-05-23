<?php /* Smarty version 3.1.24-dev/7, created on 2015-05-23 18:11:16
         compiled from "./templates/phptag_block_comment.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:291935560a6a412b574_81106260%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '3f5a13ae129f09269eeb53e4d841f312f05f99d0' => 
    array (
      0 => './templates/phptag_block_comment.tpl',
      1 => 1432393087,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '291935560a6a412b574_81106260',
  'has_nocache_code' => false,
  'version' => '3.1.24-dev/7',
  'unifunc' => 'content_5560a6a4132537_37257195',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_5560a6a4132537_37257195')) {
function content_5560a6a4132537_37257195 ($_smarty_tpl) {

$_smarty_tpl->properties['nocache_hash'] = '291935560a6a412b574_81106260';
?>
--><?php  echo ' hello world ';
/*
*    comment {/php} is okay
*
* comment {/php} once again {/php} foo
*/

echo '{php} ';

/*
* other comment <% foo
*/

echo '{/php} ';
$foo = 3;
?><--<?php }
}
?>