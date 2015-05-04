<?php /* Smarty version 3.1.22-dev/21, created on 2015-05-02 01:37:47
         compiled from "./templates/whitespace.tpl" */ ?>
<?php
/*%%SmartyHeaderCode:165355442a6ba29684_87113997%%*/
if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'd62c08dc9f4b141dac0e91ec7143d67ecdfb5f42' => 
    array (
      0 => './templates/whitespace.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '165355442a6ba29684_87113997',
  'has_nocache_code' => false,
  'version' => '3.1.22-dev/21',
  'unifunc' => 'content_55442a6ba2d8f1_07517792',
),false);
/*/%%SmartyHeaderCode%%*/
if ($_valid && !is_callable('content_55442a6ba2d8f1_07517792')) {
function content_55442a6ba2d8f1_07517792 ($_smarty_tpl) {
?>
<?php
$_smarty_tpl->properties['nocache_hash'] = '165355442a6ba29684_87113997';
?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de">
<head>
    <meta charset="utf-8"/>
    <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
    <title>whitespace</title>
    <meta name="title" content=""/>
    <meta name="description" content=""/>

    <link rel="stylesheet" type="text/css" href="screen.css"/>
</head>
<body>
<!--[if lte IE 6]>internet explorer conditional comment<![endif]-->
<!--[if lte IE 7]>internet explorer conditional comment<![endif]-->
<div class="  asdasd   " id='not' data-one=" "
     style=" " title=' '></div>
<!-- html comment -->
<!--
    html
    multiline
    comment
-->
<img
        src="foo" alt=""/>

<?php echo '<script'; ?>
 type="text/javascript">
    foobar
<?php echo '</script'; ?>
>
<?php echo '<script'; ?>
>
    foobar
<?php echo '</script'; ?>
>
	<pre id="foobar">
		foobar
	</pre>
	<pre>
		foobar
	</pre>
<p>
	<textarea name="foobar">
		foobar
	</textarea>
</p>

</body>
</html><?php }
}
?>