<?php

require("Smarty.class.php");

error_reporting(E_ALL);

$smarty = new Smarty;
$smarty->force_compile = true;
//$smarty->left_delimiter = '<!---';
//$smarty->right_delimiter = '--->';
//$smarty->plugins_dir = array('plugins','./my_plugins');
$smarty->plugins_dir = './plugins';
//$smarty->use_sub_dirs = false;
//$smarty->caching = true;
//$smarty->cache_modified_check = true;
//$smarty->compile_check = false;
//$smarty->security = true;
//$smarty->security_settings['ALLOW_CONSTANTS'] = true;
//$smarty->trusted_dir=array("./trusted");
//$smarty->secure_dir=array("./templates");
//$smarty->default_template_handler_func = "make_tpl";

//$smarty->debugging=true;

define('_MY_CONST','myconst');

$smarty->assign('foo','bar');
$smarty->assign('bfoo',true);
$smarty->assign('bar','one');
$smarty->assign('afoo',array('one' => 'foo', 'two' => 'bar'));

class ofoo {
	var $blah = 'myblah';
	function foo($var=null,$var2=null,$var3=null) {
		return '[' . $var . '][' . $var2 . '][' . $var3 . ']';
	}
}

$ofoo = new ofoo;

$smarty->assign('ofoo',$ofoo);

class nfoo {
	var $ofoo = null;
	function nfoo() {
		$this->ofoo = new ofoo;
	}
}


$nfoo = new nfoo;

$smarty->assign('nfoo',$nfoo);


function _smarty_ffoo($params, &$smarty) {
	foreach($params as $var) {
		$return .= '[' . $var . ']'; 
	}
	return $return;
}

$smarty->register_function('ffoo','_smarty_ffoo');

$smarty->assign('sfoo',array('one','two','three'));

function smarty_block_reverse($params, $content, &$smarty) {
	if($content) {
		return strrev($content);
	}
}

$smarty->register_block('reverse', 'smarty_block_reverse');

$smarty->display('index.tpl');

?>
