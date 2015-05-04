<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:14:51
         compiled from "41e81a693e81b8a27035625eda9e5ea921b0473e" */ ?>
<?php /*%%SmartyHeaderCode:162935546abeb452321-91499336%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '41e81a693e81b8a27035625eda9e5ea921b0473e' => 
    array (
      0 => '41e81a693e81b8a27035625eda9e5ea921b0473e',
      1 => 0,
      2 => 'string',
    ),
  ),
  'nocache_hash' => '162935546abeb452321-91499336',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 0,
    'x' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abeb46e2b2_81242670',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abeb46e2b2_81242670')) {function content_5546abeb46e2b2_81242670($_smarty_tpl) {?><?php  $_smarty_tpl->tpl_vars['x'] = new Smarty_Variable; $_smarty_tpl->tpl_vars['x']->_loop = false;
 $_from = $_smarty_tpl->tpl_vars['foo']->value; if (!is_array($_from) && !is_object($_from)) { settype($_from, 'array');}
foreach ($_from as $_smarty_tpl->tpl_vars['x']->key => $_smarty_tpl->tpl_vars['x']->value){
$_smarty_tpl->tpl_vars['x']->_loop = true;
?><?php echo $_smarty_tpl->tpl_vars['x']->value;?>
 <?php } ?><?php }} ?>