<?php /* Smarty version Smarty-3.1.21, created on 2015-05-03 04:55:48
         compiled from ".\templates\whitespace.tpl" */ ?>
<?php /*%%SmartyHeaderCode:302385545aa54463e08-81916285%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    '11bbdf2e48278b7a0e00ea919ed7fdae1292d3b3' => 
    array (
      0 => '.\\templates\\whitespace.tpl',
      1 => 1430530504,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '302385545aa54463e08-81916285',
  'function' => 
  array (
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1.21',
  'unifunc' => 'content_5545aa54468119_67896204',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_5545aa54468119_67896204')) {function content_5545aa54468119_67896204($_smarty_tpl) {?><!DOCTYPE html>
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
</html><?php }} ?>
