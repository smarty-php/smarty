<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     block function
 * Name:     textformat
 * Purpose:  format text a certain way with preset styles
 *           or custom wrap/indent settings
 * Params:   style: string (email)
 *           indent: integer (0)
 *           wrap: integer (80)
 *           wrap_char string ("\n")
 *           indent_char: string (" ")
 *           wrap_boundary: boolean (true)
 * -------------------------------------------------------------
 */
function smarty_block_textformat($params, $content, &$this)
{
	$style = null;
	$indent = 0;
	$indent_first = 0;
	$indent_char = ' ';
	$wrap = 80;
	$wrap_char = "\n";
	$wrap_cut = false;
	$assign = null;
	
	if($content == null) {
		return true;
	}

    extract($params);

	if($style == 'email') {
		$wrap = 72;
	}	
	
	// split into paragraphs	
	$paragraphs = preg_split('![\r\n][\r\n]!',$content);
	
	foreach($paragraphs as $paragraph) {
		if($paragraph == '') {
			continue;
		}
		// convert mult. spaces & special chars to single space
		$paragraph = preg_replace(array('!\s+!','!(^\s+)|(\s+$)!'),array(' ',''),$paragraph);
		// indent first line
		if($indent_first > 0) {
			$paragraph = str_repeat($indent_char,$indent_first) . $paragraph;
		}
		// wordwrap sentences
		$paragraph = wordwrap($paragraph, $wrap - $indent, $wrap_char, $wrap_cut);
		// indent lines
		if($indent > 0) {
			$paragraph = preg_replace('!^!m',str_repeat($indent_char,$indent),$paragraph);
		}
		$output .= $paragraph . $wrap_char . $wrap_char;
	}
				
	if($assign != null) {
		$this->assign($assign,$output);
	} else {
		echo $output;
	}
	//echo $content;
}

/* vim: set expandtab: */

?>
