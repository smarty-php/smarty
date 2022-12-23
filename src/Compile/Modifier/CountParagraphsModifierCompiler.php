<?php
namespace Smarty\Compile\Modifier;
/**
 * Smarty count_paragraphs modifier plugin
 * Type:     modifier
 * Name:     count_paragraphs
 * Purpose:  count the number of paragraphs in a text
 *
 * @link   https://www.smarty.net/manual/en/language.modifier.count.paragraphs.php
 *          count_paragraphs (Smarty online manual)
 * @author Uwe Tews
 */

class CountParagraphsModifierCompiler extends Base {

	public function compile($params, \Smarty\Compiler\Template $compiler) {
		// count \r or \n characters
		return '(preg_match_all(\'#[\r\n]+#\', ' . $params[ 0 ] . ', $tmp)+1)';
	}

}