<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:22
         compiled from ".\templates\test_template_function_001.tpl" */ ?>
<?php /*%%SmartyHeaderCode:258655546ac0ae803f3-64675413%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '09386ad9fcdb61979b97289c8fd29438338a503d' => 
    array (
      0 => '.\\templates\\test_template_function_001.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '258655546ac0ae803f3-64675413',
  'function' => 
  array (
    'functest' => 
    array (
      'parameter' => 
      array (
        'default' => 'default',
      ),
      'compiled' => '',
    ),
  ),
  'variables' => 
  array (
    'default' => 0,
    'param' => 0,
  ),
  'has_nocache_code' => 0,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac0aecb732_27695636',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac0aecb732_27695636')) {function content_5546ac0aecb732_27695636($_smarty_tpl) {?><?php if (!function_exists('smarty_template_function_functest')) {
    function smarty_template_function_functest($_smarty_tpl,$params) {
    $saved_tpl_vars = $_smarty_tpl->tpl_vars;
    foreach ($_smarty_tpl->smarty->template_functions['functest']['parameter'] as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);};
    foreach ($params as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);}?><?php echo $_smarty_tpl->tpl_vars['default']->value;?>
 <?php echo $_smarty_tpl->tpl_vars['param']->value;?>
<?php $_smarty_tpl->tpl_vars = $saved_tpl_vars;}}?>
<?php smarty_template_function_functest($_smarty_tpl,array('param'=>'param'));?>
 <?php smarty_template_function_functest($_smarty_tpl,array('param'=>$_smarty_tpl->tpl_vars['param']->value));?>
 <?php smarty_template_function_functest($_smarty_tpl,array('param'=>$_smarty_tpl->tpl_vars['param']->value,'default'=>$_smarty_tpl->tpl_vars['default']->value));?>
<?php }} ?>