<?php

/*
 * This script is for upgrading from 1.3.2 -> 1.4.0.
 * This script will convert Smarty variable references from the old format to
 * the new one. For example, what used to look like $section1/foo.bar will now
 * be $foo[section1].bar. This allows for more readable syntax and also allows
 * referencing deeply nested structures of arbitrary complexity.
 */

/*
 * Set these to match your template delimeters.
 */
$left_delimiter  = '{';
$right_delimiter = '}';


if ($argc < 2) {
	die("\nUsage: php -q fix_vars.php <templates>\n\n");
}

$ldq = preg_quote($left_delimiter, '!');
$rdq = preg_quote($right_delimiter, '!');

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

	/* Gather all template tags. */
	preg_match_all("!({$ldq}\s*)(.*?)(\s*{$rdq})!s", $input, $match);
	$template_tags = $match[2];
	$template_pre_tags = $match[1];
	$template_post_tags = $match[3];
	/* Split content by template tags to obtain non-template content. */
	$text_blocks = preg_split("!{$ldq}.*?{$rdq}!s", $input);

	$fixed_tags = array();
	for ($i = 0; $i < count($template_tags); $i++) {
		$fixed_tags[] = fix_tag($template_tags[$i]);
	}

	$output = '';
	/* Interleave the compiled contents and text blocks to get the final result. */
	for ($i = 0; $i < count($fixed_tags); $i++) {
		$output .= $text_blocks[$i].$template_pre_tags[$i].$fixed_tags[$i].$template_post_tags[$i];
	}
	$output .= $text_blocks[$i];
	
	fwrite($fp, $output);
	fclose($fp);
	copy($template.'.out', $template);
	unlink($template.'.out');

	print "Fixed $template.\n";
}

function fix_tag($template_tag)
{
	global	$qstr_regexp;

	if ($template_tag{0} == '*' && $template_tag{strlen($template_tag)-1} == '*')
		return $template_tag;

	/* Split tag into two parts: command and the arguments. */
	preg_match('/^(
				   (?: ' . $qstr_regexp . ' | (?>[^"\'\s]+))+
				  )
			   (\s+(.*))?
			   /xs', $template_tag, $match);
	list(, $tag_command, $tag_args) = $match;
	if (preg_match('!^\$(\w+(\.\w+)?/)*\w+(?>\.\w+)*(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tag_command))
		$tag_command = fix_var($tag_command);
	else if (preg_match('!^#(\w+)#(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tag_command))
		$tag_command = fix_other_var($tag_command);
	else if (preg_match('!^%\w+\.\w+%(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tag_command))
		$tag_command = fix_other_var($tag_command);

	if (function_exists("preg_replace_callback")) {
		$tag_args = preg_replace_callback('!(?<=[\s(:=])\$(\w+(\.\w+)?/)*\w+(?>\.\w+)*(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|}\s]+))*)*!', 'fix_var_match', $tag_args);
		$tag_args = preg_replace_callback('!(?<=[\s(:=])#(\w+)#(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|}\s]+))*)*!', 'fix_other_var_match', $tag_args);
		$tag_args = preg_replace_callback('!(?<=[\s(:=])%\w+\.\w+%(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|}\s]+))*)*!', 'fix_other_var_match', $tag_args);
	} else {
		$tag_args = preg_replace('!(?<=[\s(:=])\$(\w+(\.\w+)?/)*\w+(?>\.\w+)*(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|}\s]+))*)*!F', 'fix_var_match', $tag_args);
		$tag_args = preg_replace('!(?<=[\s(:=])#(\w+)#(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|}\s]+))*)*!F', 'fix_other_var_match', $tag_args);
		$tag_args = preg_replace('!(?<=[\s(:=])%\w+\.\w+%(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|}\s]+))*)*!F', 'fix_other_var_match', $tag_args);
	}

	return $tag_command.$tag_args;
}

function fix_vars_props($tokens)
{        
	global	$qstr_regexp;

	$var_exprs = preg_grep('!^\$(\w+(\.\w+)?/)*\w+(?>\.\w+)*(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tokens);
	$conf_var_exprs = preg_grep('!^#(\w+)#(?>\|@?\w+(:(?>' . $qstr_regexp . '|[^|]+))*)*$!', $tokens);
	$sect_prop_exprs = preg_grep('!^%\w+\.\w+%(?>\|@?\w+(:(?>' .  $qstr_regexp .  '|[^|]+))*)*$!', $tokens);

	if (count($var_exprs)) {
		foreach ($var_exprs as $expr_index => $var_expr) {
			$tokens[$expr_index] = fix_var($var_expr);
		}
	}

	/*
	if (count($conf_var_exprs)) {
		foreach ($conf_var_exprs as $expr_index => $var_expr) {
			$tokens[$expr_index] = $this->_parse_conf_var($var_expr);
		}
	}

	if (count($sect_prop_exprs)) {
		foreach ($sect_prop_exprs as $expr_index => $section_prop_expr) {
			$tokens[$expr_index] = $this->_parse_section_prop($section_prop_expr);
		}
	}
	*/

	return $tokens;
}

function fix_var_match($match)
{
	return fix_var($match[0]);
}

function fix_other_var_match($match)
{
	return fix_other_var($match[0]);
}

function fix_var($var_expr)
{
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
		$output .= fix_modifiers($modifiers);

	return $output;
}

function fix_other_var($var_expr)
{
	list($var_ref, $modifiers) = explode('|', $var_expr, 2);

	$output = $var_ref;
	if ($modifiers)
		$output .= fix_modifiers($modifiers);

	return $output;
}

function fix_modifiers($modifier_string)
{
	global	$qstr_regexp;

	preg_match_all('!\|(@?\w+)((?>:(?:'. $qstr_regexp . '|[^|]+))*)!', '|' . $modifier_string, $match);
	list(, $modifiers, $modifier_arg_strings) = $match;
	$output = '';
	for ($i = 0; $i < count($modifiers); $i++) {
		$modifier_name = $modifiers[$i];
		preg_match_all('!:(' . $qstr_regexp . '|[^:]+)!', $modifier_arg_strings[$i], $match);
		$modifier_args = fix_vars_props($match[1]);

		$output .= '|' . $modifier_name;
		if ($modifier_args)
			$output .= ':'.implode(':', $modifier_args);
	}

	return $output;
}

?>
