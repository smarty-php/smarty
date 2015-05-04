<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:01
         compiled from ".\templates\test_recursive_includes2.tpl" */ ?>
<?php /*%%SmartyHeaderCode:45945546abf5322c33-78968439%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'b79bd66a7cb5c896ceb34034c86a36f2334cea4d' => 
    array (
      0 => '.\\templates\\test_recursive_includes2.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '45945546abf5322c33-78968439',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 0,
    'bar' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf5361a15_01563267',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf5361a15_01563267')) {function content_5546abf5361a15_01563267($_smarty_tpl) {?>before <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
 <?php echo $_smarty_tpl->tpl_vars['bar']->value;?>
<br>
<?php if ($_smarty_tpl->tpl_vars['foo']->value<4){?><?php echo $_smarty_tpl->getSubTemplate ('test_recursive_includes_pass.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array('foo'=>$_smarty_tpl->tpl_vars['foo']->value+1), 0);?>
<?php }?>after <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
 <?php echo $_smarty_tpl->tpl_vars['bar']->value;?>
<br>
<?php }} ?>