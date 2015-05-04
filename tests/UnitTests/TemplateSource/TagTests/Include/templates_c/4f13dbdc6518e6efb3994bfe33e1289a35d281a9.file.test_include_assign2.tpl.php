<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:14:59
         compiled from ".\templates\test_include_assign2.tpl" */ ?>
<?php /*%%SmartyHeaderCode:277315546abf3c84961-34181024%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '4f13dbdc6518e6efb3994bfe33e1289a35d281a9' => 
    array (
      0 => '.\\templates\\test_include_assign2.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '277315546abf3c84961-34181024',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf3ca1b31_19763171',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf3ca1b31_19763171')) {function content_5546abf3ca1b31_19763171($_smarty_tpl) {?><?php $_smarty_tpl->tpl_vars['foo'] = new Smarty_variable('bar', null, 0);?><?php $_smarty_tpl->tpl_vars['foo'] = new Smarty_variable($_smarty_tpl->getSubTemplate ("helloworld.tpl", $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array(), 0));?>
<?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
<?php }} ?>