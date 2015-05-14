<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:00
         compiled from "./templates/test_template_function_002.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:32163554f33a03b9d17_30936283%%*/
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
  'nocache_hash' => '32163554f33a03b9d17_30936283',
  'variables' => 
  array (
    'default' => 0,
    'param' => 0,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a03f2984_15922471',
  'tpl_function' => 
  array (
    'functest' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/6df2384df0a3755f460437207bf362b39a168f74_0.file.test_template_function_002.tpl.cache.php',
      'uid' => '6df2384df0a3755f460437207bf362b39a168f74',
      'call_name_caching' => 'smarty_template_function_functest_32163554f33a03b9d17_30936283_nocache',
      'call_name' => 'smarty_template_function_functest_32163554f33a03b9d17_30936283',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a03f2984_15922471')) {
function content_554f33a03f2984_15922471 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '32163554f33a03b9d17_30936283';
$_smarty_tpl->callTemplateFunction ('functest', $_smarty_tpl, array('param'=>'param'), false);?>
 <?php $_smarty_tpl->callTemplateFunction ('functest', $_smarty_tpl, array('param'=>$_smarty_tpl->tpl_vars['param']->value), false);?>
 <?php $_smarty_tpl->callTemplateFunction ('functest', $_smarty_tpl, array('param'=>$_smarty_tpl->tpl_vars['param']->value,'default'=>$_smarty_tpl->tpl_vars['default']->value), false);?>
<?php }
}
?><?php
/* smarty_template_function_functest_32163554f33a03b9d17_30936283_nocache */
if (!function_exists('smarty_template_function_functest_32163554f33a03b9d17_30936283_nocache')) {
function smarty_template_function_functest_32163554f33a03b9d17_30936283_nocache ($_smarty_tpl,$params) {
ob_start();
$params = array_merge(array('default'=>'default'), $params);
$_smarty_tpl->properties['saved_tpl_vars'][] = $_smarty_tpl->tpl_vars;
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}$params = var_export($params, true);
echo "/*%%SmartyNocache:32163554f33a03b9d17_30936283%%*/<?php \$saved_tpl_vars = \$_smarty_tpl->tpl_vars;
foreach ($params as \$key => \$value) {
\$_smarty_tpl->tpl_vars[\$key] = new Smarty_Variable(\$value);
}
?>/*/%%SmartyNocache:32163554f33a03b9d17_30936283%%*/
";
echo $_smarty_tpl->tpl_vars['default']->value;?>
 <?php echo '/*%%SmartyNocache:32163554f33a03b9d17_30936283%%*/<?php echo $_smarty_tpl->tpl_vars[\'param\']->value;?>
/*/%%SmartyNocache:32163554f33a03b9d17_30936283%%*/';
echo "/*%%SmartyNocache:32163554f33a03b9d17_30936283%%*/<?php foreach (Smarty::\$global_tpl_vars as \$key => \$value){
if (\$_smarty_tpl->tpl_vars[\$key] === \$value) \$saved_tpl_vars[\$key] = \$value;
}
\$_smarty_tpl->tpl_vars = \$saved_tpl_vars;?>
/*/%%SmartyNocache:32163554f33a03b9d17_30936283%%*/";
?><?php echo str_replace('32163554f33a03b9d17_30936283', $_smarty_tpl->properties['nocache_hash'], ob_get_clean());
$_smarty_tpl->tpl_vars = array_pop($_smarty_tpl->properties['saved_tpl_vars']);
}
}
/*/ smarty_template_function_functest_32163554f33a03b9d17_30936283_nocache */
/* smarty_template_function_functest_32163554f33a03b9d17_30936283 */
if (!function_exists('smarty_template_function_functest_32163554f33a03b9d17_30936283')) {
function smarty_template_function_functest_32163554f33a03b9d17_30936283($_smarty_tpl,$params) {
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
/*/ smarty_template_function_functest_32163554f33a03b9d17_30936283 */

?>
