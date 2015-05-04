<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:34
         compiled from ".\templates\assign.root.tpl" */ ?>
<?php /*%%SmartyHeaderCode:150675546ac16c5fdf5-69654877%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '8d0e795f16cf7dca37a30a29c4e07176ee9c6d20' => 
    array (
      0 => '.\\templates\\assign.root.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '150675546ac16c5fdf5-69654877',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'local' => 0,
    'parent' => 0,
    'root' => 0,
    'global' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac16cbdd75_77550766',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac16cbdd75_77550766')) {function content_5546ac16cbdd75_77550766($_smarty_tpl) {?><?php $_smarty_tpl->tpl_vars["root"] = new Smarty_variable("root", null, 2);
$_ptr = $_smarty_tpl->parent; while ($_ptr != null) {$_ptr->tpl_vars["root"] = clone $_smarty_tpl->tpl_vars["root"]; $_ptr = $_ptr->parent; }?> <?php echo (($tmp = @$_smarty_tpl->tpl_vars['local']->value)===null||$tmp==='' ? "no-local" : $tmp);?>
 <?php echo $_smarty_tpl->getSubTemplate ("assign.global.tpl", $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array(), 0);?>
 <?php echo (($tmp = @$_smarty_tpl->tpl_vars['parent']->value)===null||$tmp==='' ? "no-parent" : $tmp);?>
 <?php echo (($tmp = @$_smarty_tpl->tpl_vars['root']->value)===null||$tmp==='' ? "no-root" : $tmp);?>
 <?php echo (($tmp = @$_smarty_tpl->tpl_vars['global']->value)===null||$tmp==='' ? "no-global" : $tmp);?>
<?php }} ?>