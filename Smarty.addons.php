<?

/*
 * Project:		Smarty: the PHP compiled template engine
 * File:		Smarty.functions.php
 * Author:		Monte Ohrt <monte@ispi.net>
 *				Andrei Zmievski <andrei@ispi.net>
 *
 */


/*======================================================================*\
	Function: smarty_mod_escape
	Purpose:  Escape the string according to escapement type
\*======================================================================*/
function smarty_mod_escape($string, $esc_type = 'html')
{
	switch ($esc_type) {
		case 'html':
			return htmlspecialchars($string);

		case 'url':
			return urlencode($string);

		default:
			return $string;
	}
}


/*======================================================================*\
	Function: smarty_mod_truncate
	Purpose:  Truncate a string to a certain length if necessary,
			  optionally splitting in the middle of a word, and
			  appending the $etc string.
\*======================================================================*/
function smarty_mod_truncate($string, $length = 80, $etc = '...', $break_words = false)
{
	if (strlen($string) > $length) {
		$length -= strlen($etc);
		$fragment = substr($string, 0, $length+1);
		if ($break_words)
			$fragment = substr($fragment, 0, -1);
		else
			$fragment = preg_replace('/\s+(\S+)?$/', '', $fragment);
		return $fragment.$etc;
	} else
		return $string;
}


function smarty_mod_spacify($string, $spacify_char = ' ')
{
	return implode($spacify_char, preg_split('//', $string, -1, PREG_SPLIT_NO_EMPTY));
}

?>
