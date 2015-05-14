<?php
/*%%SmartyHeaderCode:9227554f33a309a449_19754931%%*/
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
  'nocache_hash' => '9227554f33a309a449_19754931',
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
  'has_nocache_code' => true,
  'version' => '3.1.22-dev/32',
  'unifunc' => 'content_554f33a30ebdc0_07849220',
  'cache_lifetime' => 1000,
),true);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_554f33a30ebdc0_07849220')) {
function content_554f33a30ebdc0_07849220 ($_smarty_tpl) {
?>

<?php $_smarty_tpl->callTemplateFunction ('template_func1', $_smarty_tpl, array(), true);?>
<?php }
}
?><?php /* smarty_template_function_template_func1_30703554f33a2534bd6_18319841 */
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
/*/ smarty_template_function_template_func1_30703554f33a2534bd6_18319841 */?>
