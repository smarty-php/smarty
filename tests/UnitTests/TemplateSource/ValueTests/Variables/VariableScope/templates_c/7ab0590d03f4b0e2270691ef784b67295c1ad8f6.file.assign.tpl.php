<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:34
         compiled from ".\templates\assign.tpl" */ ?>
<?php /*%%SmartyHeaderCode:296535546ac16b2bab3-79176586%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '7ab0590d03f4b0e2270691ef784b67295c1ad8f6' => 
    array (
      0 => '.\\templates\\assign.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '296535546ac16b2bab3-79176586',
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
  'unifunc' => 'content_5546ac16b81ca8_56096469',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac16b81ca8_56096469')) {function content_5546ac16b81ca8_56096469($_smarty_tpl) {?><?php $_smarty_tpl->tpl_vars["local"] = new Smarty_variable("local", null, 0);?> <?php echo (($tmp = @$_smarty_tpl->tpl_vars['local']->value)===null||$tmp==='' ? "no-local" : $tmp);?>
 <?php echo $_smarty_tpl->getSubTemplate ("assign.parent.tpl", $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array(), 0);?>
 <?php echo (($tmp = @$_smarty_tpl->tpl_vars['parent']->value)===null||$tmp==='' ? "no-parent" : $tmp);?>
 <?php echo (($tmp = @$_smarty_tpl->tpl_vars['root']->value)===null||$tmp==='' ? "no-root" : $tmp);?>
 <?php echo (($tmp = @$_smarty_tpl->tpl_vars['global']->value)===null||$tmp==='' ? "no-global" : $tmp);?>
<?php }} ?>