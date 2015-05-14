<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:01
         compiled from "./templates/test_template_function_tag5.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:32283554f33a1a4e286_12401978%%*/
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
    'd0ce3d3da7d8c86b14875fc36c9fccde5dc190ba' => 
    array (
      0 => './templates/test_inherit_function_tag.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '32283554f33a1a4e286_12401978',
  'variables' => 
  array (
    'loop' => 0,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a1a80068_05590904',
  'tpl_function' => 
  array (
    'functest4' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/743d0e59177f943e67ac0f2a52c3e77d787a7b98_1.file.test_template_function_tag5.tpl.php',
      'uid' => '743d0e59177f943e67ac0f2a52c3e77d787a7b98',
      'call_name' => 'smarty_template_function_functest4_32283554f33a1a4e286_12401978',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a1a80068_05590904')) {
function content_554f33a1a80068_05590904 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '32283554f33a1a4e286_12401978';
/*  Call merged included template "test_inherit_function_tag.tpl" */
echo $_smarty_tpl->getInlineSubTemplate('test_inherit_function_tag.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, $_smarty_tpl->cache_lifetime, array(), 0, '3473554f33a1a78440_81337556', 'content_554f33a1a77345_73752731');
/*  End of included template "test_inherit_function_tag.tpl" */?>
<?php }
}
?><?php
/*%%SmartyHeaderCode:3473554f33a1a78440_81337556%%*/
if ($_valid && !is_callable('content_554f33a1a77345_73752731')) {
function content_554f33a1a77345_73752731 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '3473554f33a1a78440_81337556';
$_smarty_tpl->callTemplateFunction ('functest4', $_smarty_tpl, array(), true);?>
<?php
/*/%%SmartyNocache:3473554f33a1a78440_81337556%%*/
}
}
?><?php
/* smarty_template_function_functest4_32283554f33a1a4e286_12401978 */
if (!function_exists('smarty_template_function_functest4_32283554f33a1a4e286_12401978')) {
function smarty_template_function_functest4_32283554f33a1a4e286_12401978($_smarty_tpl,$params) {
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
/*/ smarty_template_function_functest4_32283554f33a1a4e286_12401978 */

?>
