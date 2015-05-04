<?php /* Smarty version 3.1.22-dev/21, created on 2015-05-02 01:37:47
         compiled from "./templates/test_plugin_chained_load.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:564255442a6bb81f81_75273143%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '81882ef0cb63cbc3e4855447293874456908d605' => 
    array (
      0 => './templates/test_plugin_chained_load.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '564255442a6bb81f81_75273143',
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/21',
  'unifunc' => 'content_55442a6bb8d7f2_24791150',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_55442a6bb8d7f2_24791150')) {
function content_55442a6bb8d7f2_24791150 ($_smarty_tpl) {
if (!is_callable('smarty_function_chain1')) require_once 'C:/wamp/www/Smarty3.1-test-2 - 3.1.11/vendor/smarty/smarty-phpunit/UnitTests/_Core/PluginTests/PHPunitplugins/function.chain1.php';
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '564255442a6bb81f81_75273143';
echo smarty_function_chain1(array(),$_smarty_tpl);?>
<?php }
}
?>