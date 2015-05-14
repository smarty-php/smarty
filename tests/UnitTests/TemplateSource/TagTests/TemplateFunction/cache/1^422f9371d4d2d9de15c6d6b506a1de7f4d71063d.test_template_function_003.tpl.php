<?php
/*%%SmartyHeaderCode:12318554f33a0dd75e8_81553017%%*/
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
  'nocache_hash' => '12318554f33a0dd75e8_81553017',
  'tpl_function' => 
  array (
    'functest' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/1^422f9371d4d2d9de15c6d6b506a1de7f4d71063d_0.file.test_template_function_003.tpl.cache.php',
      'uid' => '422f9371d4d2d9de15c6d6b506a1de7f4d71063d',
      'call_name_caching' => 'smarty_template_function_functest_12318554f33a0dd75e8_81553017_nocache',
      'call_name' => 'smarty_template_function_functest_12318554f33a0dd75e8_81553017',
    ),
  ),
  'variables' => 
  array (
    'default' => 1,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a0e467b9_89359945',
  'saved_tpl_vars' => 
  array (
  ),
  'cache_lifetime' => 3600,
),true);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a0e467b9_89359945')) {
function content_554f33a0e467b9_89359945 ($_smarty_tpl) {
?>
<?php if (!is_callable('smarty_function_counter')) require_once 'C:/wamp/www/Smarty3.1-test-release/vendor/smarty/smarty/libs/plugins/function.counter.php';
?><?php $saved_tpl_vars = $_smarty_tpl->tpl_vars;
foreach (array (
  'default' => 'default',
) as $key => $value) {
$_smarty_tpl->tpl_vars[$key] = new Smarty_Variable($value);
}
?>
<?php echo $_smarty_tpl->tpl_vars['default']->value;?>
 <?php echo smarty_function_counter(array('start'=>1),$_smarty_tpl);?>
<?php foreach (Smarty::$global_tpl_vars as $key => $value){
if ($_smarty_tpl->tpl_vars[$key] === $value) $saved_tpl_vars[$key] = $value;
}
$_smarty_tpl->tpl_vars = $saved_tpl_vars;?>
<?php }
}
?><?php /* smarty_template_function_functest_12318554f33a0dd75e8_81553017 */
if (!function_exists('smarty_template_function_functest_12318554f33a0dd75e8_81553017')) {
function smarty_template_function_functest_12318554f33a0dd75e8_81553017($_smarty_tpl,$params) {
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
/*/ smarty_template_function_functest_12318554f33a0dd75e8_81553017 */?>
