<?php /*%%SmartyHeaderCode:223115546ac0c8b9a57-27675462%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'fbd66664f02a689fe68ff07aab3c3e08ff032450' => 
    array (
      0 => '.\\templates\\test_template_function.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
    'f570b722030e66e236a8779621afd8864a68a35c' => 
    array (
      0 => '.\\templates\\template_function_lib.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '223115546ac0c8b9a57-27675462',
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac0cbf0929_62753899',
  'has_nocache_code' => true,
  'cache_lifetime' => 1000,
),true); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac0cbf0929_62753899')) {function content_5546ac0cbf0929_62753899($_smarty_tpl) {?>foo <?php echo htmlspecialchars($_smarty_tpl->tpl_vars['foo']->value, ENT_QUOTES, 'UTF-8', true);?>
 <?php }} ?>