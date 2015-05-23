<?php /* Smarty version 3.1.24-dev/7, created on 2015-05-23 18:11:15
         compiled from "./templates/script.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:93805560a6a3ec26c9_68047538%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'f286914a7c21fa1f5fb8070f4da6115a337a26f4' => 
    array (
      0 => './templates/script.tpl',
      1 => 1432343903,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '93805560a6a3ec26c9_68047538',
  'has_nocache_code' => false,
  'version' => '3.1.24-dev/7',
  'unifunc' => 'content_5560a6a3ec79f1_10305020',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_5560a6a3ec79f1_10305020')) {
function content_5560a6a3ec79f1_10305020 ($_smarty_tpl) {

$_smarty_tpl->properties['nocache_hash'] = '93805560a6a3ec26c9_68047538';
?>
--><?php echo '<script language=\'php\'>
echo \' hello world \';
echo \'<script language=\\\'php\\\'> \';
echo \'</script> \';
</script>';?><--<?php }
}
?>