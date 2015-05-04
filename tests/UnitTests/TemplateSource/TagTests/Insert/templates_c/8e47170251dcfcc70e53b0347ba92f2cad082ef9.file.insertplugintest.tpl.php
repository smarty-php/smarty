<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:01
         compiled from ".\templates\insertplugintest.tpl" */ ?>
<?php /*%%SmartyHeaderCode:280635546abf5af6517-75169572%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '8e47170251dcfcc70e53b0347ba92f2cad082ef9' => 
    array (
      0 => '.\\templates\\insertplugintest.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '280635546abf5af6517-75169572',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf5b07a13_44489434',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf5b07a13_44489434')) {function content_5546abf5b07a13_44489434($_smarty_tpl) {?><?php if (!is_callable('smarty_insert_insertplugintest')) include 'C:\\wamp\\www\\Smarty3.1-test-2 - 3.1.11\\vendor\\smarty\\smarty-phpunit-base\\UnitTests\\TemplateSource\\TagTests\\Insert/PHPunitplugins\\insert.insertplugintest.php';
?><?php echo smarty_insert_insertplugintest(array('foo' => $_smarty_tpl->tpl_vars['foo']->value),$_smarty_tpl);?>
<?php }} ?>