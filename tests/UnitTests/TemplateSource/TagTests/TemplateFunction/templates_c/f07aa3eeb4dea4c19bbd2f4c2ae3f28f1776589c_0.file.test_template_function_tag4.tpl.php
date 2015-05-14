<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:01
         compiled from "./templates/test_template_function_tag4.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:24268554f33a14620b8_97298192%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'f07aa3eeb4dea4c19bbd2f4c2ae3f28f1776589c' => 
    array (
      0 => './templates/test_template_function_tag4.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '24268554f33a14620b8_97298192',
  'variables' => 
  array (
    'loop' => 0,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a148afb4_17621827',
  'tpl_function' => 
  array (
    'functest4' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/f07aa3eeb4dea4c19bbd2f4c2ae3f28f1776589c_0.file.test_template_function_tag4.tpl.php',
      'uid' => 'f07aa3eeb4dea4c19bbd2f4c2ae3f28f1776589c',
      'call_name' => 'smarty_template_function_functest4_24268554f33a14620b8_97298192',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a148afb4_17621827')) {
function content_554f33a148afb4_17621827 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '24268554f33a14620b8_97298192';
$_smarty_tpl->callTemplateFunction ('functest4', $_smarty_tpl, array(), true);?>
<?php }
}
?><?php
/* smarty_template_function_functest4_24268554f33a14620b8_97298192 */
if (!function_exists('smarty_template_function_functest4_24268554f33a14620b8_97298192')) {
function smarty_template_function_functest4_24268554f33a14620b8_97298192($_smarty_tpl,$params) {
$saved_tpl_vars = $_smarty_tpl->tpl_vars;
$params = array_merge(array('loop'=>0), $params);
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}
echo $_smarty_tpl->tpl_vars['loop']->value;
if ($_smarty_tpl->tpl_vars['loop']->value<5) {
$_smarty_tpl->callTemplateFunction ('functest4', $_smarty_tpl, array('loop'=>$_smarty_tpl->tpl_vars['loop']->value+1), false);
}
foreach (Smarty::$global_tpl_vars as $key => $value){
if ($_smarty_tpl->tpl_vars[$key] === $value) $saved_tpl_vars[$key] = $value;
}
$_smarty_tpl->tpl_vars = $saved_tpl_vars;
}
}
/*/ smarty_template_function_functest4_24268554f33a14620b8_97298192 */

?>
