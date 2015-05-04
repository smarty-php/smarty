<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:23
         compiled from ".\templates\test_template_function_001.tpl" */ ?>
<?php /*%%SmartyHeaderCode:294545546ac0b2091b4-63695769%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '09386ad9fcdb61979b97289c8fd29438338a503d' => 
    array (
      0 => '.\\templates\\test_template_function_001.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '294545546ac0b2091b4-63695769',
  'function' => 
  array (
    'functest' => 
    array (
      'parameter' => 
      array (
        'default' => 'default',
      ),
      'compiled' => '<?php echo $_smarty_tpl->tpl_vars[\'default\']->value;?>
 <?php echo $_smarty_tpl->tpl_vars[\'param\']->value;?>
',
      'nocache_hash' => '294545546ac0b2091b4-63695769',
      'has_nocache_code' => false,
      'called_functions' => 
      array (
      ),
    ),
  ),
  'variables' => 
  array (
    'default' => 0,
    'param' => 0,
  ),
  'has_nocache_code' => 0,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac0b24c3f5_91440315',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac0b24c3f5_91440315')) {function content_5546ac0b24c3f5_91440315($_smarty_tpl) {?><?php Smarty_Internal_Function_Call_Handler::call ('functest',$_smarty_tpl,array('param'=>'param'),'294545546ac0b2091b4_63695769',false);?>
 <?php Smarty_Internal_Function_Call_Handler::call ('functest',$_smarty_tpl,array('param'=>$_smarty_tpl->tpl_vars['param']->value),'294545546ac0b2091b4_63695769',false);?>
 <?php Smarty_Internal_Function_Call_Handler::call ('functest',$_smarty_tpl,array('param'=>$_smarty_tpl->tpl_vars['param']->value,'default'=>$_smarty_tpl->tpl_vars['default']->value),'294545546ac0b2091b4_63695769',false);?>
<?php }} ?>