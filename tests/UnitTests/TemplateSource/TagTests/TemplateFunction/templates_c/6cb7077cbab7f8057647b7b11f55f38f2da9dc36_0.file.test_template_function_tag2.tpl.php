<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:01
         compiled from "./templates/test_template_function_tag2.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:20735554f33a1198368_16340485%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '6cb7077cbab7f8057647b7b11f55f38f2da9dc36' => 
    array (
      0 => './templates/test_template_function_tag2.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '20735554f33a1198368_16340485',
  'variables' => 
  array (
    'default' => 0,
    'param' => 0,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a11cbff7_75513815',
  'tpl_function' => 
  array (
    'functest2' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/6cb7077cbab7f8057647b7b11f55f38f2da9dc36_0.file.test_template_function_tag2.tpl.php',
      'uid' => '6cb7077cbab7f8057647b7b11f55f38f2da9dc36',
      'call_name' => 'smarty_template_function_functest2_20735554f33a1198368_16340485',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a11cbff7_75513815')) {
function content_554f33a11cbff7_75513815 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '20735554f33a1198368_16340485';
$_smarty_tpl->callTemplateFunction ('functest2', $_smarty_tpl, array('param'=>'param'), true);?>
 <?php $_smarty_tpl->callTemplateFunction ('functest2', $_smarty_tpl, array('param'=>'param2'), true);?>
 <?php $_smarty_tpl->callTemplateFunction ('functest2', $_smarty_tpl, array('param'=>'param2','default'=>'passed'), true);?>
 <?php $_smarty_tpl->callTemplateFunction ('functest2', $_smarty_tpl, array('param'=>'param'), true);?>
<?php }
}
?><?php
/* smarty_template_function_functest2_20735554f33a1198368_16340485 */
if (!function_exists('smarty_template_function_functest2_20735554f33a1198368_16340485')) {
function smarty_template_function_functest2_20735554f33a1198368_16340485($_smarty_tpl,$params) {
$saved_tpl_vars = $_smarty_tpl->tpl_vars;
$params = array_merge(array('default'=>'default'), $params);
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}
echo $_smarty_tpl->tpl_vars['default']->value;?>
 <?php echo $_smarty_tpl->tpl_vars['param']->value;
foreach (Smarty::$global_tpl_vars as $key => $value){
if ($_smarty_tpl->tpl_vars[$key] === $value) $saved_tpl_vars[$key] = $value;
}
$_smarty_tpl->tpl_vars = $saved_tpl_vars;
}
}
/*/ smarty_template_function_functest2_20735554f33a1198368_16340485 */

?>
