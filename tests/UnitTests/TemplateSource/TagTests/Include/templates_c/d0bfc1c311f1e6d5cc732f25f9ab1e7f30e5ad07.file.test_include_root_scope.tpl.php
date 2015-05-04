<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:00
         compiled from ".\templates\test_include_root_scope.tpl" */ ?>
<?php /*%%SmartyHeaderCode:27285546abf4abda51-53988248%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'd0bfc1c311f1e6d5cc732f25f9ab1e7f30e5ad07' => 
    array (
      0 => '.\\templates\\test_include_root_scope.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '27285546abf4abda51-53988248',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf4ad8eb0_41586032',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf4ad8eb0_41586032')) {function content_5546abf4ad8eb0_41586032($_smarty_tpl) {?>before include <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
 <?php echo $_smarty_tpl->getSubTemplate ('test_include_local_scope_sub.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array(), 2);?>
 after include <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
<?php }} ?>