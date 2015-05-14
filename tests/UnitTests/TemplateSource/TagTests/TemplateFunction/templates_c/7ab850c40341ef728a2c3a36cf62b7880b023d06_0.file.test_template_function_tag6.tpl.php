<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:01
         compiled from "./templates/test_template_function_tag6.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:9462554f33a1d2ad82_45742097%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '7ab850c40341ef728a2c3a36cf62b7880b023d06' => 
    array (
      0 => './templates/test_template_function_tag6.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '9462554f33a1d2ad82_45742097',
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a1d35a35_68706146',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a1d35a35_68706146')) {
function content_554f33a1d35a35_68706146 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '9462554f33a1d2ad82_45742097';
echo $_smarty_tpl->getSubTemplate ('test_define_function_tag.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, $_smarty_tpl->cache_lifetime, array(), 0);
echo $_smarty_tpl->getSubTemplate ('test_inherit_function_tag6.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, $_smarty_tpl->cache_lifetime, array(), 0);
?>
<?php }
}
?>