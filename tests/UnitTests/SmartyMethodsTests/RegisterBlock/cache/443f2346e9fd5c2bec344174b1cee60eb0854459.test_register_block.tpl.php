<?php /*%%SmartyHeaderCode:237585546abd38cd322-25517081%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '443f2346e9fd5c2bec344174b1cee60eb0854459' => 
    array (
      0 => '.\\templates\\test_register_block.tpl',
      1 => 1430530503,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '237585546abd38cd322-25517081',
  'variables' => 
  array (
    'x' => 0,
    'y' => 1,
    'z' => 0,
  ),
  'has_nocache_code' => true,
  'version' => 'Smarty-3.1-DEV',
  'unifunc' => 'content_5546abd3937466_66820586',
  'cache_lifetime' => 1000,
),true); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5546abd3937466_66820586')) {function content_5546abd3937466_66820586($_smarty_tpl) {?>3 <?php $_smarty_tpl->smarty->_tag_stack[] = array('testblock', array()); $_block_repeat=true; echo myblockcache(array(), null, $_smarty_tpl, $_block_repeat);while ($_block_repeat) { ob_start();?>
<?php echo $_smarty_tpl->tpl_vars['y']->value;?>
<?php $_block_content = ob_get_clean(); $_block_repeat=false; echo myblockcache(array(), $_block_content, $_smarty_tpl, $_block_repeat); } array_pop($_smarty_tpl->smarty->_tag_stack);?>
 300<?php }} ?>