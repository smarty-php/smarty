<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:23
         compiled from ".\templates\test_template_function_tag5.tpl" */ ?>
<?php /*%%SmartyHeaderCode:128265546ac0bd283c0-64174904%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '88236acb653de15fd9f15562e49e8731e26406cf' => 
    array (
      0 => '.\\templates\\test_template_function_tag5.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '128265546ac0bd283c0-64174904',
  'function' => 
  array (
    'functest4' => 
    array (
      'parameter' => 
      array (
        'loop' => 0,
      ),
      'compiled' => '',
    ),
  ),
  'variables' => 
  array (
    'loop' => 0,
  ),
  'has_nocache_code' => 0,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac0bd61a03_57831587',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac0bd61a03_57831587')) {function content_5546ac0bd61a03_57831587($_smarty_tpl) {?><?php if (!function_exists('smarty_template_function_functest4')) {
    function smarty_template_function_functest4($_smarty_tpl,$params) {
    $saved_tpl_vars = $_smarty_tpl->tpl_vars;
    foreach ($_smarty_tpl->smarty->template_functions['functest4']['parameter'] as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);};
    foreach ($params as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);}?><?php echo $_smarty_tpl->tpl_vars['loop']->value;?>
<?php if ($_smarty_tpl->tpl_vars['loop']->value<5){?><?php smarty_template_function_functest4($_smarty_tpl,array('loop'=>$_smarty_tpl->tpl_vars['loop']->value+1));?>
<?php }?><?php $_smarty_tpl->tpl_vars = $saved_tpl_vars;}}?>
<?php echo $_smarty_tpl->getSubTemplate ('test_inherit_function_tag.tpl', $_smarty_tpl->cache_id, $_smarty_tpl->compile_id, null, null, array(), 0);?>
<?php }} ?>