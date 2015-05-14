<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:02
         compiled from "./templates/test_template_function.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:23758554f33a24e2527_45120061%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '9d9c162f1a2d1af75355a54fa37e85b832d9728a' => 
    array (
      0 => './templates/test_template_function.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '23758554f33a24e2527_45120061',
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a24ed0e0_77310981',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a24ed0e0_77310981')) {
function content_554f33a24ed0e0_77310981 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '23758554f33a24e2527_45120061';
echo $_smarty_tpl->getSubTemplate ('template_function_lib.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 9999, $_smarty_tpl->cache_lifetime, array(), 0);
$_smarty_tpl->callTemplateFunction ('template_func1', $_smarty_tpl, array(), false);?>
<?php }
}
?>