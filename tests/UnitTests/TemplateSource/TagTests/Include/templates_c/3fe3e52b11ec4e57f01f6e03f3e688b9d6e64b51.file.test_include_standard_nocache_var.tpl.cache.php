<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:14:59
         compiled from ".\templates\test_include_standard_nocache_var.tpl" */ ?>
<?php /*%%SmartyHeaderCode:120685546abf36ebc04-08331572%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '3fe3e52b11ec4e57f01f6e03f3e688b9d6e64b51' => 
    array (
      0 => '.\\templates\\test_include_standard_nocache_var.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '120685546abf36ebc04-08331572',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 1,
  ),
  'has_nocache_code' => true,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf36fdab6_85308088',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf36fdab6_85308088')) {function content_5546abf36fdab6_85308088($_smarty_tpl) {?><?php echo '/*%%SmartyNocache:120685546abf36ebc04-08331572%%*/<?php echo $_smarty_tpl->tpl_vars[\'foo\']->value;?>
/*/%%SmartyNocache:120685546abf36ebc04-08331572%%*/';?>


<?php echo $_smarty_tpl->getSubTemplate ("helloworld.tpl", $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, 9999, null, array(), 0);?>
<?php }} ?>