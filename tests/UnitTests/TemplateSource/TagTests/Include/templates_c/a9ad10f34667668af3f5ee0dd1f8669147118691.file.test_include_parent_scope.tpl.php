<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:00
         compiled from ".\templates\test_include_parent_scope.tpl" */ ?>
<?php /*%%SmartyHeaderCode:118455546abf48192d9-40850486%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'a9ad10f34667668af3f5ee0dd1f8669147118691' => 
    array (
      0 => '.\\templates\\test_include_parent_scope.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '118455546abf48192d9-40850486',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf4835735_68149898',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf4835735_68149898')) {function content_5546abf4835735_68149898($_smarty_tpl) {?>before include <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
 <?php echo $_smarty_tpl->getSubTemplate ('test_include_local_scope_sub.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array(), 1);?>
 after include <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
<?php }} ?>