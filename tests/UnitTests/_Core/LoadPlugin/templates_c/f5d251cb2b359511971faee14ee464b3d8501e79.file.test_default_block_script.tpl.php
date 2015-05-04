<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:15:38
         compiled from ".\templates\test_default_block_script.tpl" */ ?>
<?php /*%%SmartyHeaderCode:29755546ac1ae3d276-15980111%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'f5d251cb2b359511971faee14ee464b3d8501e79' => 
    array (
      0 => '.\\templates\\test_default_block_script.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '29755546ac1ae3d276-15980111',
  'function' => 
  array (
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546ac1ae4f4f5_34974144',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546ac1ae4f4f5_34974144')) {function content_5546ac1ae4f4f5_34974144($_smarty_tpl) {?><?php if (!is_callable('default_script_block_tag')) include './scripts/script_block_tag.php';
?><?php $_smarty_tpl->smarty->_tag_stack[] = array('scriptblock', array()); $_block_repeat=true; echo default_script_block_tag(array(), null, $_smarty_tpl, $_block_repeat);while ($_block_repeat) { ob_start();?>
foo bar<?php $_block_content = ob_get_clean(); $_block_repeat=false; echo default_script_block_tag(array(), $_block_content, $_smarty_tpl, $_block_repeat); } array_pop($_smarty_tpl->smarty->_tag_stack);?>
<?php }} ?>