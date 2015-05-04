<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:23
         compiled from ".\templates\test_template_function_tag2.tpl" */ ?>
<?php /*%%SmartyHeaderCode:131165546ac0b7b36e2-87647107%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'ae15c0395cbdb671d3e0b5c64665fb72a031cc55' => 
    array (
      0 => '.\\templates\\test_template_function_tag2.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '131165546ac0b7b36e2-87647107',
  'function' => 
  array (
    'functest2' => 
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
  'unifunc' => 'content_5546ac0b7f6786_68320817',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac0b7f6786_68320817')) {function content_5546ac0b7f6786_68320817($_smarty_tpl) {?><?php if (!function_exists('smarty_template_function_functest2')) {
    function smarty_template_function_functest2($_smarty_tpl,$params) {
    $saved_tpl_vars = $_smarty_tpl->tpl_vars;
    foreach ($_smarty_tpl->smarty->template_functions['functest2']['parameter'] as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);};
    foreach ($params as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);}?><?php echo $_smarty_tpl->tpl_vars['default']->value;?>
 <?php echo $_smarty_tpl->tpl_vars['param']->value;?>
<?php $_smarty_tpl->tpl_vars = $saved_tpl_vars;}}?>
<?php smarty_template_function_functest2($_smarty_tpl,array('param'=>'param'));?>
 <?php smarty_template_function_functest2($_smarty_tpl,array('param'=>'param2'));?>
 <?php smarty_template_function_functest2($_smarty_tpl,array('param'=>'param2','default'=>'passed'));?>
 <?php smarty_template_function_functest2($_smarty_tpl,array('param'=>'param'));?>
<?php }} ?>