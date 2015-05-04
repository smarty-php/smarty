<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:34
         compiled from ".\templates\assign.parent.tpl" */ ?>
<?php /*%%SmartyHeaderCode:266025546ac16bc0694-05642480%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '84e26627682853100dd26c373bda360efe3dd4cb' => 
    array (
      0 => '.\\templates\\assign.parent.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '266025546ac16bc0694-05642480',
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
  'unifunc' => 'content_5546ac16c1f423_68418718',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac16c1f423_68418718')) {function content_5546ac16c1f423_68418718($_smarty_tpl) {?><?php $_smarty_tpl->tpl_vars["parent"] = new Smarty_variable("parent", null, 1);
if ($_smarty_tpl->parent != null) $_smarty_tpl->parent->tpl_vars["parent"] = clone $_smarty_tpl->tpl_vars["parent"];?> <?php echo (($tmp = @$_smarty_tpl->tpl_vars['local']->value)===null||$tmp==='' ? "no-local" : $tmp);?>
 <?php echo $_smarty_tpl->getSubTemplate ("assign.root.tpl", $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array(), 0);?>
 <?php echo (($tmp = @$_smarty_tpl->tpl_vars['parent']->value)===null||$tmp==='' ? "no-parent" : $tmp);?>
 <?php echo (($tmp = @$_smarty_tpl->tpl_vars['root']->value)===null||$tmp==='' ? "no-root" : $tmp);?>
 <?php echo (($tmp = @$_smarty_tpl->tpl_vars['global']->value)===null||$tmp==='' ? "no-global" : $tmp);?>
<?php }} ?>