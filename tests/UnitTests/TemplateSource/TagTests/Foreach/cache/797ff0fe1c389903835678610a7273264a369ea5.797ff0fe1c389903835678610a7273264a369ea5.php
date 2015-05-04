<?php /*%%SmartyHeaderCode:147485546abeb1866d1-85904268%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '797ff0fe1c389903835678610a7273264a369ea5' => 
    array (
      0 => '797ff0fe1c389903835678610a7273264a369ea5',
      1 => 0,
      2 => 'string',
    ),
  ),
  'nocache_hash' => '147485546abeb1866d1-85904268',
  'variables' => 
  array (
    'foo' => 0,
    'x' => 1,
  ),
  'has_nocache_code' => true,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abeb1dd7c9_59538066',
  'cache_lifetime' => 3600,
),true); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abeb1dd7c9_59538066')) {function content_5546abeb1dd7c9_59538066($_smarty_tpl) {?><?php  $_smarty_tpl->tpl_vars['x'] = new Smarty_Variable; $_smarty_tpl->tpl_vars['x']->_loop = false;
 $_from = $_smarty_tpl->tpl_vars['foo']->value; if (!is_array($_from) && !is_object($_from)) { settype($_from, 'array');}
foreach ($_from as $_smarty_tpl->tpl_vars['x']->key => $_smarty_tpl->tpl_vars['x']->value){
$_smarty_tpl->tpl_vars['x']->_loop = true;
?><?php echo $_smarty_tpl->tpl_vars['x']->value;?>
 <?php } ?><?php }} ?>