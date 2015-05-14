<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:03
         compiled from "./templates/test_template_function_nocache_call.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:25320554f33a336e935_85019970%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '7fe6a2a43aee3e8dbfe08235cf00ef0c2bdd4a61' => 
    array (
      0 => './templates/test_template_function_nocache_call.tpl',
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
  'nocache_hash' => '25320554f33a336e935_85019970',
  'has_nocache_code' => true,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a33a5381_95773343',
  'tpl_function' => 
  array (
    'template_func1' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/7fe6a2a43aee3e8dbfe08235cf00ef0c2bdd4a61_1.file.test_template_function_nocache_call.tpl.cache.php',
      'uid' => 'dfb143ec1fbcb556e13ac1f174c3b8394191bd18',
      'call_name_caching' => 'smarty_template_function_template_func1_14246554f33a33751a3_10584105_nocache',
      'call_name' => 'smarty_template_function_template_func1_14246554f33a33751a3_10584105',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a33a5381_95773343')) {
function content_554f33a33a5381_95773343 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '25320554f33a336e935_85019970';
/*  Call merged included template "template_function_lib.tpl" */
echo $_smarty_tpl->getInlineSubTemplate('template_function_lib.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 9999, $_smarty_tpl->cache_lifetime, array(), 0, '14246554f33a33751a3_10584105', 'content_554f33a3374104_61765255');
/*  End of included template "template_function_lib.tpl" */
echo '/*%%SmartyNocache:25320554f33a336e935_85019970%%*/<?php $_smarty_tpl->callTemplateFunction (\'template_func1\', $_smarty_tpl, array(), true);?>
/*/%%SmartyNocache:25320554f33a336e935_85019970%%*/';?>
<?php }
}
?><?php
/*%%SmartyHeaderCode:14246554f33a33751a3_10584105%%*/
if ($_valid && !is_callable('content_554f33a3374104_61765255')) {
function content_554f33a3374104_61765255 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '14246554f33a33751a3_10584105';
?>

<?php
/*/%%SmartyNocache:14246554f33a33751a3_10584105%%*/
}
}
?><?php
/* smarty_template_function_template_func1_14246554f33a33751a3_10584105_nocache */
if (!function_exists('smarty_template_function_template_func1_14246554f33a33751a3_10584105_nocache')) {
function smarty_template_function_template_func1_14246554f33a33751a3_10584105_nocache ($_smarty_tpl,$params) {
ob_start();
$params = array_merge(array('default'=>'d1'), $params);
$_smarty_tpl->properties['saved_tpl_vars'][] = $_smarty_tpl->tpl_vars;
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}$params = var_export($params, true);
echo "/*%%SmartyNocache:14246554f33a33751a3_10584105%%*/<?php \$saved_tpl_vars = \$_smarty_tpl->tpl_vars;
foreach ($params as \$key => \$value) {
\$_smarty_tpl->tpl_vars[\$key] = new Smarty_Variable(\$value);
}
?>/*/%%SmartyNocache:14246554f33a33751a3_10584105%%*/
";
echo htmlspecialchars($_smarty_tpl->tpl_vars['foo']->value, ENT_QUOTES, 'UTF-8', true);?>
 <?php echo '/*%%SmartyNocache:14246554f33a33751a3_10584105%%*/<?php echo htmlspecialchars($_smarty_tpl->tpl_vars[\'foo\']->value, ENT_QUOTES, \'UTF-8\', true);?>
/*/%%SmartyNocache:14246554f33a33751a3_10584105%%*/';?>
 <?php echo "/*%%SmartyNocache:14246554f33a33751a3_10584105%%*/<?php foreach (Smarty::\$global_tpl_vars as \$key => \$value){
if (\$_smarty_tpl->tpl_vars[\$key] === \$value) \$saved_tpl_vars[\$key] = \$value;
}
\$_smarty_tpl->tpl_vars = \$saved_tpl_vars;?>
/*/%%SmartyNocache:14246554f33a33751a3_10584105%%*/";
?><?php echo str_replace('14246554f33a33751a3_10584105', $_smarty_tpl->properties['nocache_hash'], ob_get_clean());
$_smarty_tpl->tpl_vars = array_pop($_smarty_tpl->properties['saved_tpl_vars']);
}
}
/*/ smarty_template_function_template_func1_14246554f33a33751a3_10584105_nocache */
/* smarty_template_function_template_func1_14246554f33a33751a3_10584105 */
if (!function_exists('smarty_template_function_template_func1_14246554f33a33751a3_10584105')) {
function smarty_template_function_template_func1_14246554f33a33751a3_10584105($_smarty_tpl,$params) {
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
/*/ smarty_template_function_template_func1_14246554f33a33751a3_10584105 */

?>
