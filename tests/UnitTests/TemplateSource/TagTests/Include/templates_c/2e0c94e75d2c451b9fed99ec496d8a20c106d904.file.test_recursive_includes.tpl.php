<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:01
         compiled from ".\templates\test_recursive_includes.tpl" */ ?>
<?php /*%%SmartyHeaderCode:248295546abf50730f5-67841353%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '2e0c94e75d2c451b9fed99ec496d8a20c106d904' => 
    array (
      0 => '.\\templates\\test_recursive_includes.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '248295546abf50730f5-67841353',
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
  'unifunc' => 'content_5546abf50b26e6_83439332',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf50b26e6_83439332')) {function content_5546abf50b26e6_83439332($_smarty_tpl) {?>before <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
 <?php echo $_smarty_tpl->tpl_vars['bar']->value;?>
<br>
<?php if ($_smarty_tpl->tpl_vars['foo']->value<3){?><?php echo $_smarty_tpl->getSubTemplate ('test_recursive_includes.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array('foo'=>$_smarty_tpl->tpl_vars['foo']->value+1), 0);?>
<?php }?>after <?php echo $_smarty_tpl->tpl_vars['foo']->value;?>
 <?php echo $_smarty_tpl->tpl_vars['bar']->value;?>
<br>
<?php }} ?>