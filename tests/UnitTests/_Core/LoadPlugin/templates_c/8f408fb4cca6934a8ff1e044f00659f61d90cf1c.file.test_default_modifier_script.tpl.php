<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:39
         compiled from ".\templates\test_default_modifier_script.tpl" */ ?>
<?php /*%%SmartyHeaderCode:53565546ac1b065f95-24814248%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '8f408fb4cca6934a8ff1e044f00659f61d90cf1c' => 
    array (
      0 => '.\\templates\\test_default_modifier_script.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '53565546ac1b065f95-24814248',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'foo' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac1b079b50_80055228',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac1b079b50_80055228')) {function content_5546ac1b079b50_80055228($_smarty_tpl) {?><?php if (!is_callable('default_script_modifier')) include './scripts/script_modifier.php';
?><?php echo default_script_modifier($_smarty_tpl->tpl_vars['foo']->value,'scriptmodifier default ');?>
<?php }} ?>