<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:02
         compiled from "./templates/template_function_lib.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:30703554f33a2534bd6_18319841%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'dfb143ec1fbcb556e13ac1f174c3b8394191bd18' => 
    array (
      0 => './templates/template_function_lib.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '30703554f33a2534bd6_18319841',
  'variables' => 
  array (
    'foo' => 1,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a255bc18_48320158',
  'tpl_function' => 
  array (
    'template_func1' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/dfb143ec1fbcb556e13ac1f174c3b8394191bd18_0.file.template_function_lib.tpl.cache.php',
      'uid' => 'dfb143ec1fbcb556e13ac1f174c3b8394191bd18',
      'call_name_caching' => 'smarty_template_function_template_func1_30703554f33a2534bd6_18319841_nocache',
      'call_name' => 'smarty_template_function_template_func1_30703554f33a2534bd6_18319841',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a255bc18_48320158')) {
function content_554f33a255bc18_48320158 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '30703554f33a2534bd6_18319841';
?>

<?php }
}
?><?php
/* smarty_template_function_template_func1_30703554f33a2534bd6_18319841_nocache */
if (!function_exists('smarty_template_function_template_func1_30703554f33a2534bd6_18319841_nocache')) {
function smarty_template_function_template_func1_30703554f33a2534bd6_18319841_nocache ($_smarty_tpl,$params) {
ob_start();
$params = array_merge(array('default'=>'d1'), $params);
$_smarty_tpl->properties['saved_tpl_vars'][] = $_smarty_tpl->tpl_vars;
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}$params = var_export($params, true);
echo "/*%%SmartyNocache:30703554f33a2534bd6_18319841%%*/<?php \$saved_tpl_vars = \$_smarty_tpl->tpl_vars;
foreach ($params as \$key => \$value) {
\$_smarty_tpl->tpl_vars[\$key] = new Smarty_Variable(\$value);
}
?>/*/%%SmartyNocache:30703554f33a2534bd6_18319841%%*/
";
echo htmlspecialchars($_smarty_tpl->tpl_vars['foo']->value, ENT_QUOTES, 'UTF-8', true);?>
 <?php echo '/*%%SmartyNocache:30703554f33a2534bd6_18319841%%*/<?php echo htmlspecialchars($_smarty_tpl->tpl_vars[\'foo\']->value, ENT_QUOTES, \'UTF-8\', true);?>
/*/%%SmartyNocache:30703554f33a2534bd6_18319841%%*/';?>
 <?php echo "/*%%SmartyNocache:30703554f33a2534bd6_18319841%%*/<?php foreach (Smarty::\$global_tpl_vars as \$key => \$value){
if (\$_smarty_tpl->tpl_vars[\$key] === \$value) \$saved_tpl_vars[\$key] = \$value;
}
\$_smarty_tpl->tpl_vars = \$saved_tpl_vars;?>
/*/%%SmartyNocache:30703554f33a2534bd6_18319841%%*/";
?><?php echo str_replace('30703554f33a2534bd6_18319841', $_smarty_tpl->properties['nocache_hash'], ob_get_clean());
$_smarty_tpl->tpl_vars = array_pop($_smarty_tpl->properties['saved_tpl_vars']);
}
}
/*/ smarty_template_function_template_func1_30703554f33a2534bd6_18319841_nocache */
/* smarty_template_function_template_func1_30703554f33a2534bd6_18319841 */
if (!function_exists('smarty_template_function_template_func1_30703554f33a2534bd6_18319841')) {
function smarty_template_function_template_func1_30703554f33a2534bd6_18319841($_smarty_tpl,$params) {
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
/*/ smarty_template_function_template_func1_30703554f33a2534bd6_18319841 */

?>
