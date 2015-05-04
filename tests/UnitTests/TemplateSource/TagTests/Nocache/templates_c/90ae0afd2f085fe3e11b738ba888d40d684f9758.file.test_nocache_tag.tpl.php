<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:03
         compiled from ".\templates\test_nocache_tag.tpl" */ ?>
<?php /*%%SmartyHeaderCode:41475546abf72cca17-05140380%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '90ae0afd2f085fe3e11b738ba888d40d684f9758' => 
    array (
      0 => '.\\templates\\test_nocache_tag.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '41475546abf72cca17-05140380',
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
  'unifunc' => 'content_5546abf72f0e77_89950017',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf72f0e77_89950017')) {function content_5546abf72f0e77_89950017($_smarty_tpl) {?><br>root <?php echo $_smarty_tpl->tpl_vars['foo']->value+2;?>
<?php echo $_smarty_tpl->tpl_vars['bar']->value;?>

<?php echo $_smarty_tpl->getSubTemplate ('test_nocache_tag_include.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array(), 0);?>
<?php }} ?>