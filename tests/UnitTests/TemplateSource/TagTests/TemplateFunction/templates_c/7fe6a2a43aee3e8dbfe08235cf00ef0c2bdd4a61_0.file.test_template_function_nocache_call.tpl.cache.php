<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:03
         compiled from "./templates/test_template_function_nocache_call.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:9227554f33a309a449_19754931%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '7fe6a2a43aee3e8dbfe08235cf00ef0c2bdd4a61' => 
    array (
      0 => './templates/test_template_function_nocache_call.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '9227554f33a309a449_19754931',
  'has_nocache_code' => true,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a30a7084_73390220',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a30a7084_73390220')) {
function content_554f33a30a7084_73390220 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '9227554f33a309a449_19754931';
echo $_smarty_tpl->getSubTemplate ('template_function_lib.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 9999, $_smarty_tpl->cache_lifetime, array(), 0);
echo '/*%%SmartyNocache:9227554f33a309a449_19754931%%*/<?php $_smarty_tpl->callTemplateFunction (\'template_func1\', $_smarty_tpl, array(), true);?>
/*/%%SmartyNocache:9227554f33a309a449_19754931%%*/';?>
<?php }
}
?>