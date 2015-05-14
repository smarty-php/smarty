<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:02
         compiled from "./templates/test_template_function_tag6.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:29133554f33a2200012_48386613%%*/
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
    '2e9582e4d4f31291755140d7b123bc17fbaa811a' => 
    array (
      0 => './templates/test_define_function_tag.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
    'c43995ee759c6c7f171e915aace16ee91ee059a9' => 
    array (
      0 => './templates/test_inherit_function_tag6.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '29133554f33a2200012_48386613',
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a223ad46_61545179',
  'tpl_function' => 
  array (
    'functest6i' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/7ab850c40341ef728a2c3a36cf62b7880b023d06_1.file.test_template_function_tag6.tpl.php',
      'uid' => '2e9582e4d4f31291755140d7b123bc17fbaa811a',
      'call_name' => 'smarty_template_function_functest6i_5523554f33a22066a1_70467432',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a223ad46_61545179')) {
function content_554f33a223ad46_61545179 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '29133554f33a2200012_48386613';
/*  Call merged included template "test_define_function_tag.tpl" */
echo $_smarty_tpl->getInlineSubTemplate('test_define_function_tag.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, $_smarty_tpl->cache_lifetime, array(), 0, '5523554f33a22066a1_70467432', 'content_554f33a2205710_05709812');
/*  End of included template "test_define_function_tag.tpl" */
/*  Call merged included template "test_inherit_function_tag6.tpl" */
echo $_smarty_tpl->getInlineSubTemplate('test_inherit_function_tag6.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 0, $_smarty_tpl->cache_lifetime, array(), 0, '24009554f33a2232320_32744838', 'content_554f33a2231111_07613734');
/*  End of included template "test_inherit_function_tag6.tpl" */?>
<?php }
}
?><?php
/*%%SmartyHeaderCode:5523554f33a22066a1_70467432%%*/
if ($_valid && !is_callable('content_554f33a2205710_05709812')) {
function content_554f33a2205710_05709812 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '5523554f33a22066a1_70467432';
?>
<?php
/*/%%SmartyNocache:5523554f33a22066a1_70467432%%*/
}
}
?><?php
/*%%SmartyHeaderCode:24009554f33a2232320_32744838%%*/
if ($_valid && !is_callable('content_554f33a2231111_07613734')) {
function content_554f33a2231111_07613734 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '24009554f33a2232320_32744838';
$_smarty_tpl->callTemplateFunction ('functest6i', $_smarty_tpl, array(), true);?>
<?php
/*/%%SmartyNocache:24009554f33a2232320_32744838%%*/
}
}
?><?php
/* smarty_template_function_functest6i_5523554f33a22066a1_70467432 */
if (!function_exists('smarty_template_function_functest6i_5523554f33a22066a1_70467432')) {
function smarty_template_function_functest6i_5523554f33a22066a1_70467432($_smarty_tpl,$params) {
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
/*/ smarty_template_function_functest6i_5523554f33a22066a1_70467432 */

?>
