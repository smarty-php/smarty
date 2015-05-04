<?php /* Smarty version 3.1.22-dev/21, created on 2015-05-02 04:19:58
         compiled from "C:/wamp/www/Smarty3.1-test-2 - 3.1.11/vendor/smarty/smarty-phpunit/UnitTests/CacheResourceTests/Memcache/../_shared/templates/cacheresource.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:283645544506ea82522_15247746%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '8116aff437278768935b896ec15aa941ca30b7dc' => 
    array (
      0 => 'C:/wamp/www/Smarty3.1-test-2 - 3.1.11/vendor/smarty/smarty-phpunit/UnitTests/CacheResourceTests/Memcache/../_shared/templates/cacheresource.tpl',
      1 => 1430540396,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '283645544506ea82522_15247746',
  'variables' => 
  array (
    'test' => 0,
  ),
  'has_nocache_code' => true,
  'version' => '3.1.22-dev/21',
  'unifunc' => 'content_5544506ead8391_48098543',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_5544506ead8391_48098543')) {
function content_5544506ead8391_48098543 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '283645544506ea82522_15247746';
?>
cache resource test:<?php echo '/*%%SmartyNocache:283645544506ea82522_15247746%%*/<?php echo $_smarty_tpl->tpl_vars[\'test\']->value;?>
/*/%%SmartyNocache:283645544506ea82522_15247746%%*/';?>
 compiled:6 rendered:<?php echo $_smarty_tpl->tpl_vars['test']->value;?>
<?php }
}
?>