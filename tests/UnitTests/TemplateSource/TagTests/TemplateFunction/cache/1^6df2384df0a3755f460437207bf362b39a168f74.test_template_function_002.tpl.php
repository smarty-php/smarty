<?php
/*%%SmartyHeaderCode:22688554f33a06da492_37365358%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '6df2384df0a3755f460437207bf362b39a168f74' => 
    array (
      0 => './templates/test_template_function_002.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '22688554f33a06da492_37365358',
  'tpl_function' => 
  array (
    'functest' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/1^6df2384df0a3755f460437207bf362b39a168f74_0.file.test_template_function_002.tpl.cache.php',
      'uid' => '6df2384df0a3755f460437207bf362b39a168f74',
      'call_name_caching' => 'smarty_template_function_functest_22688554f33a06da492_37365358_nocache',
      'call_name' => 'smarty_template_function_functest_22688554f33a06da492_37365358',
    ),
  ),
  'variables' => 
  array (
    'default' => 1,
    'param' => 0,
  ),
  'has_nocache_code' => true,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a0754e03_36911292',
  'saved_tpl_vars' => 
  array (
  ),
  'cache_lifetime' => 3600,
),true);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a0754e03_36911292')) {
function content_554f33a0754e03_36911292 ($_smarty_tpl) {
?>
<?php $saved_tpl_vars = $_smarty_tpl->tpl_vars;
foreach (array (
  'default' => 'default',
  'param' => 'param',
) as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}
?>
<?php echo $_smarty_tpl->tpl_vars['default']->value;?>
 <?php echo $_smarty_tpl->tpl_vars['param']->value;?>
<?php foreach (Smarty::$global_tpl_vars as $key => $value){
if ($_smarty_tpl->tpl_vars[$key] === $value) $saved_tpl_vars[$key] = $value;
}
$_smarty_tpl->tpl_vars = $saved_tpl_vars;?>
 <?php $saved_tpl_vars = $_smarty_tpl->tpl_vars;
foreach (array (
  'default' => 'default',
  'param' => 1,
) as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}
?>
<?php echo $_smarty_tpl->tpl_vars['default']->value;?>
 <?php echo $_smarty_tpl->tpl_vars['param']->value;?>
<?php foreach (Smarty::$global_tpl_vars as $key => $value){
if ($_smarty_tpl->tpl_vars[$key] === $value) $saved_tpl_vars[$key] = $value;
}
$_smarty_tpl->tpl_vars = $saved_tpl_vars;?>
 <?php $_smarty_tpl->callTemplateFunction ('functest', $_smarty_tpl, array('param'=>$_smarty_tpl->tpl_vars['param']->value,'default'=>$_smarty_tpl->tpl_vars['default']->value), true);?>
<?php }
}
?><?php /* smarty_template_function_functest_22688554f33a06da492_37365358 */
if (!function_exists('smarty_template_function_functest_22688554f33a06da492_37365358')) {
function smarty_template_function_functest_22688554f33a06da492_37365358($_smarty_tpl,$params) {
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
/*/ smarty_template_function_functest_22688554f33a06da492_37365358 */?>
