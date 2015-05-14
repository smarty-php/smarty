<?php
/*%%SmartyHeaderCode:22738554f33a00aae61_12933950%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'd80ab133a083b4a7b15480731cf13d1c48e9f54a' => 
    array (
      0 => './templates/test_template_function_001.tpl',
      1 => 1430802407,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '22738554f33a00aae61_12933950',
  'tpl_function' => 
  array (
    'functest' => 
    array (
      'called_functions' => 
      array (
      ),
      'compiled_filepath' => './templates_c/d80ab133a083b4a7b15480731cf13d1c48e9f54a_0.file.test_template_function_001.tpl.cache.php',
      'uid' => 'd80ab133a083b4a7b15480731cf13d1c48e9f54a',
      'call_name' => 'smarty_template_function_functest_22738554f33a00aae61_12933950',
    ),
  ),
  'variables' => 
  array (
    'default' => 0,
    'param' => 0,
  ),
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a01276d2_11243022',
  'cache_lifetime' => 3600,
),true);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a01276d2_11243022')) {
function content_554f33a01276d2_11243022 ($_smarty_tpl) {
?>
default param default 1 2 1<?php }
}
?><?php /* smarty_template_function_functest_22738554f33a00aae61_12933950 */
if (!function_exists('smarty_template_function_functest_22738554f33a00aae61_12933950')) {
function smarty_template_function_functest_22738554f33a00aae61_12933950($_smarty_tpl,$params) {
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
/*/ smarty_template_function_functest_22738554f33a00aae61_12933950 */?>
