<?php

/*
 * This script will convert Smarty variable references from the old format to
 * the new one. For example, what used to look like $section1/foo.bar will now
 * be $foo[section1].bar. This allows for more readable syntax and also allows
 * referencing deeply nested structures of arbitrary complexity.
 */

if ($argc < 2) {
	die("\nUsage: php -q fix_vars.php <templates>\n\n");
}

$qstr_regexp = '"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"|\'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'';
$pwd = $HTTP_ENV_VARS['PWD'];

foreach (array_slice($argv, 1) as $template) {
	$template = $pwd . '/' . $template;
    if (!is_file($template)) continue;

	$input = implode('', file($template));
	$fp = fopen($template.'.out', 'w');
	if (!$fp) {
		die("\nError: could not open $template.out for writing\n\n");
	}

	if (function_exists("preg_replace_callback"))
		$output = preg_replace_callback('!\$(\w+(\.\w+)?/)*\w+(?>\.\w+)*(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|}\s]+))*)*!s', 'fix_var', $input);
	else
		$output = preg_replace('!\$(\w+(\.\w+)?/)*\w+(?>\.\w+)*(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|}\s]+))*)*!mF', 'fix_var', $input);

	fwrite($fp, $output);
	fclose($fp);
	copy($template.'.out', $template);
	unlink($template.'.out');

	print "Fixed $template.\n";
}

function fix_var($match)
{
	$var_expr = $match[0];

	list($var_ref, $modifiers) = explode('|', substr($var_expr, 1), 2);

	$sections = explode('/', $var_ref);
	$props = explode('.', array_pop($sections));
	$var_name = array_shift($props);

	$output = "\$$var_name";

	foreach ($sections as $section_ref) {
		$output .= "[$section_ref]";
	}
	if (count($props))
		$output .= ".".implode('.', $props);

	if ($modifiers)
		$output .= '|' . $modifiers;

	return $output;
}

?>
