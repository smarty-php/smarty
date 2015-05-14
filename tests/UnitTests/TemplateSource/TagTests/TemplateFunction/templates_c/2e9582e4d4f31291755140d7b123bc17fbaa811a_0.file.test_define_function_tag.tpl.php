<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:01
         compiled from "./templates/test_define_function_tag.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:24823554f33a1d764f9_03909341%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '2e9582e4d4f31291755140d7b123bc17fbaa811a' => 
    array (
      0 => './templates/test_define_function_tag.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '24823554f33a1d764f9_03909341',
  'variables' => 
  array (
    'loop' => 0,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a1d9b877_15698903',
  'tpl_function' => 
  array (
    'functest6i' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/2e9582e4d4f31291755140d7b123bc17fbaa811a_0.file.test_define_function_tag.tpl.php',
      'uid' => '2e9582e4d4f31291755140d7b123bc17fbaa811a',
      'call_name' => 'smarty_template_function_functest6i_24823554f33a1d764f9_03909341',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a1d9b877_15698903')) {
function content_554f33a1d9b877_15698903 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '24823554f33a1d764f9_03909341';
?>
<?php }
}
?><?php
/* smarty_template_function_functest6i_24823554f33a1d764f9_03909341 */
if (!function_exists('smarty_template_function_functest6i_24823554f33a1d764f9_03909341')) {
function smarty_template_function_functest6i_24823554f33a1d764f9_03909341($_smarty_tpl,$params) {
$saved_tpl_vars = $_smarty_tpl->tpl_vars;
$params = array_merge(array('loop'=>0), $params);
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}
echo $_smarty_tpl->tpl_vars['loop']->value;
if ($_smarty_tpl->tpl_vars['loop']->value<5) {
$_smarty_tpl->callTemplateFunction ('functest6i', $_smarty_tpl, array('loop'=>$_smarty_tpl->tpl_vars['loop']->value+1), false);
}
foreach (Smarty::$global_tpl_vars as $key => $value){
if ($_smarty_tpl->tpl_vars[$key] === $value) $saved_tpl_vars[$key] = $value;
}
$_smarty_tpl->tpl_vars = $saved_tpl_vars;
}
}
/*/ smarty_template_function_functest6i_24823554f33a1d764f9_03909341 */

?>
