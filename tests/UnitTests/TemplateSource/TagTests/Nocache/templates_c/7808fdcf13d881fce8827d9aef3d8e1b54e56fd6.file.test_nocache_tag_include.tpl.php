<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:03
         compiled from ".\templates\test_nocache_tag_include.tpl" */ ?>
<?php /*%%SmartyHeaderCode:81765546abf7331323-10434051%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
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
  'nocache_hash' => '81765546abf7331323-10434051',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 1,
    'bar' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf734c7c8_01675818',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf734c7c8_01675818')) {function content_5546abf734c7c8_01675818($_smarty_tpl) {?><br>include <?php echo $_smarty_tpl->tpl_vars['foo']->value+4;?>
<?php echo $_smarty_tpl->tpl_vars['bar']->value;?>
<?php }} ?>