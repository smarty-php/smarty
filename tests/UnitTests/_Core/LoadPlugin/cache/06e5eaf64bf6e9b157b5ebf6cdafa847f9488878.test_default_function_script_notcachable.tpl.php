<?php /*%%SmartyHeaderCode:278405546ac1a9bc3d3-93600921%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '06e5eaf64bf6e9b157b5ebf6cdafa847f9488878' => 
    array (
      0 => '.\\templates\\test_default_function_script_notcachable.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '278405546ac1a9bc3d3-93600921',
  'variables' => 
  array (
    'foo' => 0,
  ),
  'has_nocache_code' => true,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac1aa0b4c9_78695907',
  'cache_lifetime' => 3600,
),true); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac1aa0b4c9_78695907')) {function content_5546ac1aa0b4c9_78695907($_smarty_tpl) {?><?php $_smarty = $_smarty_tpl->smarty; if (!is_callable('default_script_function_tag')) include './scripts/script_function_tag.php';
?><?php echo default_script_function_tag(array('value'=>$_smarty_tpl->tpl_vars['foo']->value),$_smarty_tpl);?>
<?php }} ?>