<?php /* Smarty version Smarty-3.1-DEV, created on 2015-05-03 23:14:35
         compiled from ".\templates\blockplugintest.tpl" */ ?>
<?php /*%%SmartyHeaderCode:29975546abdb6e5673-31251443%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'bf885b92d81e6085a4764ce2804f32b613d7d0e1' => 
    array (
      0 => '.\\templates\\blockplugintest.tpl',
      1 => 1430530503,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '29975546abdb6e5673-31251443',
  'function' => 
  array (
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abdb6f48d5_42669250',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abdb6f48d5_42669250')) {function content_5546abdb6f48d5_42669250($_smarty_tpl) {?><?php if (!is_callable('smarty_block_textformat')) include 'C:\\wamp\\www\\Smarty3.1-test-2 - 3.1.11\\vendor\\smarty\\smarty\\libs\\plugins\\block.textformat.php';
?><?php $_smarty_tpl->smarty->_tag_stack[] = array('textformat', array()); $_block_repeat=true; echo smarty_block_textformat(array(), null, $_smarty_tpl, $_block_repeat);while ($_block_repeat) { ob_start();?>
abc<?php $_block_content = ob_get_clean(); $_block_repeat=false; echo smarty_block_textformat(array(), $_block_content, $_smarty_tpl, $_block_repeat);  } array_pop($_smarty_tpl->smarty->_tag_stack);?>
<?php }} ?>