<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:24
         compiled from ".\templates\test_define_function_tag.tpl" */ ?>
<?php /*%%SmartyHeaderCode:113295546ac0c36a694-95034632%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '9e4c8ecce04be29a439fc8423aac8b0a5b2fe621' => 
    array (
      0 => '.\\templates\\test_define_function_tag.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '113295546ac0c36a694-95034632',
  'function' => 
  array (
    'functest6i' => 
    array (
      'parameter' => 
      array (
        'loop' => 0,
      ),
      'compiled' => '',
    ),
  ),
  'variables' => 
  array (
    'loop' => 0,
  ),
  'has_nocache_code' => 0,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac0c39d5d8_31968659',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac0c39d5d8_31968659')) {function content_5546ac0c39d5d8_31968659($_smarty_tpl) {?><?php if (!function_exists('smarty_template_function_functest6i')) {
    function smarty_template_function_functest6i($_smarty_tpl,$params) {
    $saved_tpl_vars = $_smarty_tpl->tpl_vars;
    foreach ($_smarty_tpl->smarty->template_functions['functest6i']['parameter'] as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);};
    foreach ($params as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);}?><?php echo $_smarty_tpl->tpl_vars['loop']->value;?>
<?php if ($_smarty_tpl->tpl_vars['loop']->value<5){?><?php smarty_template_function_functest6i($_smarty_tpl,array('loop'=>$_smarty_tpl->tpl_vars['loop']->value+1));?>
<?php }?><?php $_smarty_tpl->tpl_vars = $saved_tpl_vars;}}?>
<?php }} ?>