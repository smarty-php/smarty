<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:23
         compiled from ".\templates\test_template_function_003.tpl" */ ?>
<?php /*%%SmartyHeaderCode:289755546ac0b50aa97-38818815%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '4b7a617b140b8137b05d7c3a0feba6173ed81881' => 
    array (
      0 => '.\\templates\\test_template_function_003.tpl',
      1 => 1430554653,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '289755546ac0b50aa97-38818815',
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
    'default' => 1,
  ),
  'has_nocache_code' => 0,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac0b532262_30695619',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac0b532262_30695619')) {function content_5546ac0b532262_30695619($_smarty_tpl) {?><?php if (!is_callable('smarty_function_counter')) include 'C:\wamp\www\Smarty3.1-test-2 - 3.1.11\vendor\smarty\smarty\libs\plugins\function.counter.php';
?><?php if (!function_exists('smarty_template_function_functest')) {
    function smarty_template_function_functest($_smarty_tpl,$params) {
    $saved_tpl_vars = $_smarty_tpl->tpl_vars;
    foreach ($_smarty_tpl->smarty->template_functions['functest']['parameter'] as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);};
    foreach ($params as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);}?><?php echo $_smarty_tpl->tpl_vars['default']->value;?>
 <?php echo smarty_function_counter(array('start'=>1),$_smarty_tpl);?>
<?php $_smarty_tpl->tpl_vars = $saved_tpl_vars;}}?>
<?php smarty_template_function_functest($_smarty_tpl,array());?>
<?php }} ?>