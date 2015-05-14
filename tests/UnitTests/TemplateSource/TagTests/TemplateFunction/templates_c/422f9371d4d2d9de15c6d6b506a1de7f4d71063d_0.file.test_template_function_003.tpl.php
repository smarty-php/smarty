<?php /* Smarty version 3.1.22-dev/32, created on 2015-05-10 10:32:00
         compiled from "./templates/test_template_function_003.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:11353554f33a0b12c39_38927265%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '422f9371d4d2d9de15c6d6b506a1de7f4d71063d' => 
    array (
      0 => './templates/test_template_function_003.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '11353554f33a0b12c39_38927265',
  'variables' => 
  array (
    'default' => 1,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a0b34e64_35437774',
  'tpl_function' => 
  array (
    'functest' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/422f9371d4d2d9de15c6d6b506a1de7f4d71063d_0.file.test_template_function_003.tpl.php',
      'uid' => '422f9371d4d2d9de15c6d6b506a1de7f4d71063d',
      'call_name_caching' => 'smarty_template_function_functest_11353554f33a0b12c39_38927265_nocache',
      'call_name' => 'smarty_template_function_functest_11353554f33a0b12c39_38927265',
    ),
  ),
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a0b34e64_35437774')) {
function content_554f33a0b34e64_35437774 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '11353554f33a0b12c39_38927265';
$_smarty_tpl->callTemplateFunction ('functest', $_smarty_tpl, array(), true);?>
<?php }
}
?><?php
/* smarty_template_function_functest_11353554f33a0b12c39_38927265_nocache */
if (!function_exists('smarty_template_function_functest_11353554f33a0b12c39_38927265_nocache')) {
function smarty_template_function_functest_11353554f33a0b12c39_38927265_nocache ($_smarty_tpl,$params) {
echo '/*%%SmartyNocache:11353554f33a0b12c39_38927265%%*/<?php if (!is_callable(\'smarty_function_counter\')) require_once \'C:/wamp/www/Smarty3.1-test-release/vendor/smarty/smarty/libs/plugins/function.counter.php\';
?>/*/%%SmartyNocache:11353554f33a0b12c39_38927265%%*/';
ob_start();
$params = array_merge(array('default'=>'default'), $params);
$_smarty_tpl->properties['saved_tpl_vars'][] = $_smarty_tpl->tpl_vars;
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}$params = var_export($params, true);
echo "/*%%SmartyNocache:11353554f33a0b12c39_38927265%%*/<?php \$saved_tpl_vars = \$_smarty_tpl->tpl_vars;
foreach ($params as \$key => \$value) {
\$_smarty_tpl->tpl_vars[\$key] = new Smarty_Variable(\$value);
}
?>/*/%%SmartyNocache:11353554f33a0b12c39_38927265%%*/
";
echo '/*%%SmartyNocache:11353554f33a0b12c39_38927265%%*/<?php echo $_smarty_tpl->tpl_vars[\'default\']->value;?>
/*/%%SmartyNocache:11353554f33a0b12c39_38927265%%*/';?>
 <?php echo '/*%%SmartyNocache:11353554f33a0b12c39_38927265%%*/<?php echo smarty_function_counter(array(\'start\'=>1),$_smarty_tpl);?>
/*/%%SmartyNocache:11353554f33a0b12c39_38927265%%*/';
echo "/*%%SmartyNocache:11353554f33a0b12c39_38927265%%*/<?php foreach (Smarty::\$global_tpl_vars as \$key => \$value){
if (\$_smarty_tpl->tpl_vars[\$key] === \$value) \$saved_tpl_vars[\$key] = \$value;
}
\$_smarty_tpl->tpl_vars = \$saved_tpl_vars;?>
/*/%%SmartyNocache:11353554f33a0b12c39_38927265%%*/";
?><?php echo str_replace('11353554f33a0b12c39_38927265', $_smarty_tpl->properties['nocache_hash'], ob_get_clean());
$_smarty_tpl->tpl_vars = array_pop($_smarty_tpl->properties['saved_tpl_vars']);
}
}
/*/ smarty_template_function_functest_11353554f33a0b12c39_38927265_nocache */
/* smarty_template_function_functest_11353554f33a0b12c39_38927265 */
if (!function_exists('smarty_template_function_functest_11353554f33a0b12c39_38927265')) {
function smarty_template_function_functest_11353554f33a0b12c39_38927265($_smarty_tpl,$params) {
if (!is_callable('smarty_function_counter')) require_once 'C:/wamp/www/Smarty3.1-test-release/vendor/smarty/smarty/libs/plugins/function.counter.php';
$saved_tpl_vars = $_smarty_tpl->tpl_vars;
$params = array_merge(array('default'=>'default'), $params);
foreach ($params as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}
echo $_smarty_tpl->tpl_vars['default']->value;?>
 <?php echo smarty_function_counter(array('start'=>1),$_smarty_tpl);
foreach (Smarty::$global_tpl_vars as $key => $value){
if ($_smarty_tpl->tpl_vars[$key] === $value) $saved_tpl_vars[$key] = $value;
}
$_smarty_tpl->tpl_vars = $saved_tpl_vars;
}
}
/*/ smarty_template_function_functest_11353554f33a0b12c39_38927265 */

?>
