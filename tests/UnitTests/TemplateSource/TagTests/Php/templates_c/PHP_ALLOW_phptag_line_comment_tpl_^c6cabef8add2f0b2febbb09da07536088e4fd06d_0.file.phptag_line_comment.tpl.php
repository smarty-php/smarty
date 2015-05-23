<?php /* Smarty version 3.1.24-dev/7, created on 2015-05-23 18:11:16
         compiled from "./templates/phptag_line_comment.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:263845560a6a40d3f16_81532548%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'c6cabef8add2f0b2febbb09da07536088e4fd06d' => 
    array (
      0 => './templates/phptag_line_comment.tpl',
      1 => 1432392915,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '263845560a6a40d3f16_81532548',
  'has_nocache_code' => false,
  'version' => '3.1.24-dev/7',
  'unifunc' => 'content_5560a6a40daa69_91942870',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_5560a6a40daa69_91942870')) {
function content_5560a6a40daa69_91942870 ($_smarty_tpl) {

$_smarty_tpl->properties['nocache_hash'] = '263845560a6a40d3f16_81532548';
?>
--><?php  echo ' hello world ';
// comment {/php} is okay
//
// comment {/php} once again {/php} foo

echo '{php} ';

// other comment <% foo

echo '{/php} ';
$foo = 3;
?><--<?php }
}
?>