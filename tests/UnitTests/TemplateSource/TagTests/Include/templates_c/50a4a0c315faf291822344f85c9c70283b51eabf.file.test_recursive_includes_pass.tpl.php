<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:01
         compiled from ".\templates\test_recursive_includes_pass.tpl" */ ?>
<?php /*%%SmartyHeaderCode:41635546abf53ab6c5-66114962%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '50a4a0c315faf291822344f85c9c70283b51eabf' => 
    array (
      0 => '.\\templates\\test_recursive_includes_pass.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '41635546abf53ab6c5-66114962',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf53bcfd4_86663681',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf53bcfd4_86663681')) {function content_5546abf53bcfd4_86663681($_smarty_tpl) {?><?php echo $_smarty_tpl->getSubTemplate ('test_recursive_includes2.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array('foo'=>$_smarty_tpl->tpl_vars['foo']->value+1), 0);?>
<?php }} ?>