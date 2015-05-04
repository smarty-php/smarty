<?php /*%%SmartyHeaderCode:269855546abf74a6b06-66211226%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '90ae0afd2f085fe3e11b738ba888d40d684f9758' => 
    array (
      0 => '.\\templates\\test_nocache_tag.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
    '7808fdcf13d881fce8827d9aef3d8e1b54e56fd6' => 
    array (
      0 => '.\\templates\\test_nocache_tag_include.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '269855546abf74a6b06-66211226',
  'variables' => 
  array (
    'foo' => 1,
    'bar' => 0,
  ),
  'has_nocache_code' => true,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf75631f4_68282432',
  'cache_lifetime' => 3600,
),true); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf75631f4_68282432')) {function content_5546abf75631f4_68282432($_smarty_tpl) {?><br>root <?php echo $_smarty_tpl->tpl_vars['foo']->value+2;?>
A
<br>include <?php echo $_smarty_tpl->tpl_vars['foo']->value+4;?>
A<?php }} ?>