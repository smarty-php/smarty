<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:14:45
         compiled from ".\templates\test_capture_nocache.tpl" */ ?>
<?php /*%%SmartyHeaderCode:97015546abe5d169e9-51539608%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '692667b951dabf79e19973dd2c2aab5e64052fa8' => 
    array (
      0 => '.\\templates\\test_capture_nocache.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '97015546abe5d169e9-51539608',
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
  'unifunc' => 'content_5546abe5d37ce4_98467702',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abe5d37ce4_98467702')) {function content_5546abe5d37ce4_98467702($_smarty_tpl) {?><?php echo '/*%%SmartyNocache:97015546abe5d169e9-51539608%%*/<?php $_smarty_tpl->_capture_stack[0][] = array(\'default\', \'bar\', null); ob_start(); ?>/*/%%SmartyNocache:97015546abe5d169e9-51539608%%*/';?>
foo <?php echo '/*%%SmartyNocache:97015546abe5d169e9-51539608%%*/<?php echo $_smarty_tpl->tpl_vars[\'foo\']->value;?>
/*/%%SmartyNocache:97015546abe5d169e9-51539608%%*/';?>
<?php echo '/*%%SmartyNocache:97015546abe5d169e9-51539608%%*/<?php list($_capture_buffer, $_capture_assign, $_capture_append) = array_pop($_smarty_tpl->_capture_stack[0]);
if (!empty($_capture_buffer)) {
 if (isset($_capture_assign)) $_smarty_tpl->assign($_capture_assign, ob_get_contents());
 if (isset( $_capture_append)) $_smarty_tpl->append( $_capture_append, ob_get_contents());
 Smarty::$_smarty_vars[\'capture\'][$_capture_buffer]=ob_get_clean();
} else $_smarty_tpl->capture_error();?>/*/%%SmartyNocache:97015546abe5d169e9-51539608%%*/';?>

<?php echo '/*%%SmartyNocache:97015546abe5d169e9-51539608%%*/<?php echo $_smarty_tpl->tpl_vars[\'bar\']->value;?>
/*/%%SmartyNocache:97015546abe5d169e9-51539608%%*/';?>
<?php }} ?>