<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:02
         compiled from "./templates/test_template_function.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:4860554f33a2834c32_19980616%%*/
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
    'dfb143ec1fbcb556e13ac1f174c3b8394191bd18' => 
    array (
      0 => './templates/template_function_lib.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '4860554f33a2834c32_19980616',
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a28651d5_62401584',
  'tpl_function' => 
  array (
    'template_func1' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/9d9c162f1a2d1af75355a54fa37e85b832d9728a_1.file.test_template_function.tpl.cache.php',
      'uid' => 'dfb143ec1fbcb556e13ac1f174c3b8394191bd18',
      'call_name_caching' => 'smarty_template_function_template_func1_30682554f33a283b093_35547451_nocache',
      'call_name' => 'smarty_template_function_template_func1_30682554f33a283b093_35547451',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a28651d5_62401584')) {
function content_554f33a28651d5_62401584 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '4860554f33a2834c32_19980616';
/*  Call merged included template "template_function_lib.tpl" */
echo $_smarty_tpl->getInlineSubTemplate('template_function_lib.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 9999, $_smarty_tpl->cache_lifetime, array(), 0, '30682554f33a283b093_35547451', 'content_554f33a283a179_89560857');
/*  End of included template "template_function_lib.tpl" */
$_smarty_tpl->callTemplateFunction ('template_func1', $_smarty_tpl, array(), false);?>
<?php }
}
?><?php
/*%%SmartyHeaderCode:30682554f33a283b093_35547451%%*/
if ($_valid && !is_callable('content_554f33a283a179_89560857')) {
function content_554f33a283a179_89560857 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '30682554f33a283b093_35547451';
?>

<?php
/*/%%SmartyNocache:30682554f33a283b093_35547451%%*/
}
}
?><?php
/* smarty_template_function_template_func1_30682554f33a283b093_35547451_nocache */
if (!function_exists('smarty_template_function_template_func1_30682554f33a283b093_35547451_nocache')) {
function smarty_template_function_template_func1_30682554f33a283b093_35547451_nocache ($_smarty_tpl,$params) {
ob_start();
$params = array_merge(array('default'=>'d1'), $params);
$_smarty_tpl->properties['saved_tpl_vars'][] = $_smarty_tpl->tpl_vars;
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}$params = var_export($params, true);
echo "/*%%SmartyNocache:30682554f33a283b093_35547451%%*/<?php \$saved_tpl_vars = \$_smarty_tpl->tpl_vars;
foreach ($params as \$key => \$value) {
\$_smarty_tpl->tpl_vars[\$key] = new Smarty_Variable(\$value);
}
?>/*/%%SmartyNocache:30682554f33a283b093_35547451%%*/
";
echo htmlspecialchars($_smarty_tpl->tpl_vars['foo']->value, ENT_QUOTES, 'UTF-8', true);?>
 <?php echo '/*%%SmartyNocache:30682554f33a283b093_35547451%%*/<?php echo htmlspecialchars($_smarty_tpl->tpl_vars[\'foo\']->value, ENT_QUOTES, \'UTF-8\', true);?>
/*/%%SmartyNocache:30682554f33a283b093_35547451%%*/';?>
 <?php echo "/*%%SmartyNocache:30682554f33a283b093_35547451%%*/<?php foreach (Smarty::\$global_tpl_vars as \$key => \$value){
if (\$_smarty_tpl->tpl_vars[\$key] === \$value) \$saved_tpl_vars[\$key] = \$value;
}
\$_smarty_tpl->tpl_vars = \$saved_tpl_vars;?>
/*/%%SmartyNocache:30682554f33a283b093_35547451%%*/";
?><?php echo str_replace('30682554f33a283b093_35547451', $_smarty_tpl->properties['nocache_hash'], ob_get_clean());
$_smarty_tpl->tpl_vars = array_pop($_smarty_tpl->properties['saved_tpl_vars']);
}
}
/*/ smarty_template_function_template_func1_30682554f33a283b093_35547451_nocache */
/* smarty_template_function_template_func1_30682554f33a283b093_35547451 */
if (!function_exists('smarty_template_function_template_func1_30682554f33a283b093_35547451')) {
function smarty_template_function_template_func1_30682554f33a283b093_35547451($_smarty_tpl,$params) {
$saved_tpl_vars = $_smarty_tpl->tpl_vars;
$params = array_merge(array('default'=>'d1'), $params);
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}
echo htmlspecialchars($_smarty_tpl->tpl_vars['foo']->value, ENT_QUOTES, 'UTF-8', true);?>
 <?php echo htmlspecialchars($_smarty_tpl->tpl_vars['foo']->value, ENT_QUOTES, 'UTF-8', true);
foreach (Smarty::$global_tpl_vars as $key => $value){
if ($_smarty_tpl->tpl_vars[$key] === $value) $saved_tpl_vars[$key] = $value;
}
$_smarty_tpl->tpl_vars = $saved_tpl_vars;
}
}
/*/ smarty_template_function_template_func1_30682554f33a283b093_35547451 */

?>
