<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:03
         compiled from ".\templates\test_nocache_tag_include.tpl" */ ?>
<?php /*%%SmartyHeaderCode:40785546abf75076c5-21842760%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '7808fdcf13d881fce8827d9aef3d8e1b54e56fd6' => 
    array (
      0 => '.\\templates\\test_nocache_tag_include.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '40785546abf75076c5-21842760',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 1,
    'bar' => 0,
  ),
  'has_nocache_code' => true,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf7522cb6_25368928',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf7522cb6_25368928')) {function content_5546abf7522cb6_25368928($_smarty_tpl) {?><br>include <?php echo '/*%%SmartyNocache:40785546abf75076c5-21842760%%*/<?php echo $_smarty_tpl->tpl_vars[\'foo\']->value+4;?>
/*/%%SmartyNocache:40785546abf75076c5-21842760%%*/';?>
<?php echo $_smarty_tpl->tpl_vars['bar']->value;?>
<?php }} ?>