<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:00
         compiled from ".\templates\test_include_local_scope.tpl" */ ?>
<?php /*%%SmartyHeaderCode:314075546abf453aee6-59929998%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'cd498d03e9337b319009ebf19cde60042d42efc8' => 
    array (
      0 => '.\\templates\\test_include_local_scope.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '314075546abf453aee6-59929998',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf4553890_36472160',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf4553890_36472160')) {function content_5546abf4553890_36472160($_smarty_tpl) {?>before include <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
 <?php echo $_smarty_tpl->getSubTemplate ('test_include_local_scope_sub.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array(), 0);?>
 after include <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
<?php }} ?>