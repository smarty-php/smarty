<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:23
         compiled from ".\templates\test_template_function_tag4.tpl" */ ?>
<?php /*%%SmartyHeaderCode:186295546ac0ba71d00-42219566%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '143afe5305cbff3d07c285f36025ab4c2bec1c17' => 
    array (
      0 => '.\\templates\\test_template_function_tag4.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '186295546ac0ba71d00-42219566',
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
  'unifunc' => 'content_5546ac0baac193_02240671',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac0baac193_02240671')) {function content_5546ac0baac193_02240671($_smarty_tpl) {?><?php if (!function_exists('smarty_template_function_functest4')) {
    function smarty_template_function_functest4($_smarty_tpl,$params) {
    $saved_tpl_vars = $_smarty_tpl->tpl_vars;
    foreach ($_smarty_tpl->smarty->template_functions['functest4']['parameter'] as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);};
    foreach ($params as $key => $value) {$_smarty_tpl->tpl_vars[$key] = new Smarty_variable($value);}?><?php echo $_smarty_tpl->tpl_vars['loop']->value;?>
<?php if ($_smarty_tpl->tpl_vars['loop']->value<5){?><?php smarty_template_function_functest4($_smarty_tpl,array('loop'=>$_smarty_tpl->tpl_vars['loop']->value+1));?>
<?php }?><?php $_smarty_tpl->tpl_vars = $saved_tpl_vars;}}?>
<?php smarty_template_function_functest4($_smarty_tpl,array());?>
<?php }} ?>