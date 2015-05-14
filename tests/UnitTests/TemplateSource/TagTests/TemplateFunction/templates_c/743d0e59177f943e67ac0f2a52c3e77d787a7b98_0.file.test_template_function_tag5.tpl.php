<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:01
         compiled from "./templates/test_template_function_tag5.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:13589554f33a1739657_70054575%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '743d0e59177f943e67ac0f2a52c3e77d787a7b98' => 
    array (
      0 => './templates/test_template_function_tag5.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '13589554f33a1739657_70054575',
  'variables' => 
  array (
    'loop' => 0,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a17645a9_04761730',
  'tpl_function' => 
  array (
    'functest4' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/743d0e59177f943e67ac0f2a52c3e77d787a7b98_0.file.test_template_function_tag5.tpl.php',
      'uid' => '743d0e59177f943e67ac0f2a52c3e77d787a7b98',
      'call_name' => 'smarty_template_function_functest4_13589554f33a1739657_70054575',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a17645a9_04761730')) {
function content_554f33a17645a9_04761730 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '13589554f33a1739657_70054575';
echo $_smarty_tpl->getSubTemplate ('test_inherit_function_tag.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, $_smarty_tpl->cache_lifetime, array(), 0);
?>
<?php }
}
?><?php
/* smarty_template_function_functest4_13589554f33a1739657_70054575 */
if (!function_exists('smarty_template_function_functest4_13589554f33a1739657_70054575')) {
function smarty_template_function_functest4_13589554f33a1739657_70054575($_smarty_tpl,$params) {
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
/*/ smarty_template_function_functest4_13589554f33a1739657_70054575 */

?>
