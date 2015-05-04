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
    '4bf4a289c4129184fbd543f317e3a064a0574e1c' => 
    array (
      0 => '.\\templates\\helloworld.tpl',
      1 => 1430683106,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '120685546abf36ebc04-08331572',
  'variables' => 
  array (
    'foo' => 1,
  ),
  'has_nocache_code' => true,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abf3785443_00267794',
  'cache_lifetime' => 3600,
),true); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abf3785443_00267794')) {function content_5546abf3785443_00267794($_smarty_tpl) {?><?php echo $_smarty_tpl->tpl_vars['foo']->value;?>


hello world<?php }} ?>