<?php
/**
 * Smarty Internal Plugin Compile PHP Expression
 * Compiles any tag which will output an expression or variable
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

/**
 * Smarty Internal Plugin Compile PHP Expression Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Private_Php extends Smarty_Internal_CompileBase
{
    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $required_attributes = array('code', 'type');

    /**
     * Compiles code for generating output from any expression
     *
     * @param array                                 $args      array with attributes from parser
     * @param \Smarty_Internal_TemplateCompilerBase $compiler  compiler object
     * @param array                                 $parameter array with compilation parameter
     *
     * @return string
     * @throws \SmartyException
     */
    public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter)
    {
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);
        $compiler->has_code = false;
        $this->asp_tags = (ini_get('asp_tags') != '0');
        if ($_attr['type'] == 'tag' && !($compiler->smarty instanceof SmartyBC)) {
            $compiler->trigger_template_error('{php}[/php} tags not allowed. Use SmartyBC to enable them', $compiler->lex->taglineno);
        }
        if ($_attr['type'] != 'tag') {
            if (isset($compiler->smarty->security_policy)) {
                $this->php_handling = $compiler->smarty->security_policy->php_handling;
            } else {
                $this->php_handling = $compiler->smarty->php_handling;
            }
            if ($this->php_handling == Smarty::PHP_REMOVE) {
                $output = preg_replace(array('#^(<\?(?:php|=)?)|(<%)|(<script\s+language\s*=\s*["\']?\s*php\s*["\']?\s*>)#', '#(\?>)|(%>)|(<\/script>)$#'), '', $_attr['code']);
                $compiler->parser->current_buffer->append_subtree(new Smarty_Internal_ParseTree_Text($compiler->parser, $output));
                return '';
            } elseif ($this->php_handling == Smarty::PHP_QUOTE) {
                $output = preg_replace_callback(array('#^(<\?(?:php|=)?)|(<%)|(<script\s+language\s*=\s*["\']?\s*php\s*["\']?\s*>)#', '#(\?>)|(%>)|(<\/script>)$#'), function ($match) {return htmlspecialchars($match[0], ENT_QUOTES);}, $_attr['code']);
                $compiler->parser->current_buffer->append_subtree(new Smarty_Internal_ParseTree_Text($compiler->parser, $output));
                return '';
            } elseif ($this->php_handling == Smarty::PHP_PASSTHRU || ($_attr['type'] == 'asp' && !$this->asp_tags) || $_attr['type'] == 'unmatched') {
                $compiler->parser->current_buffer->append_subtree(new Smarty_Internal_ParseTree_Text($compiler->parser, $_attr['code']));
                return '';
            } elseif ($this->php_handling == Smarty::PHP_ALLOW) {
                if (!($compiler->smarty instanceof SmartyBC)) {
                    $compiler->trigger_template_error('$smarty->php_handling PHP_ALLOW not allowed. Use SmartyBC to enable it', $compiler->lex->taglineno);
                }
                $compiler->has_code = true;
                return $_attr['code'];
            } else {
                $compiler->trigger_template_error('Illegal $smarty->php_handling value', $compiler->lex->taglineno);
            }
        } else {
            $compiler->has_code = true;
            $ldel = preg_quote($compiler->smarty->left_delimiter, '#');
            $rdel = preg_quote($compiler->smarty->right_delimiter, '#');
            if (!preg_match("#{$ldel}\\s*/\\s*php\\s*{$rdel}$#", $_attr['code'], $match)) {
                $compiler->trigger_template_error('Missing {/php} closing tag', $compiler->lex->taglineno);
            }
            if (!preg_match("#^({$ldel}\\s*php\\s*)((.)*?)({$rdel})#", $_attr['code'], $match)) {
                $compiler->trigger_template_error('Missing {php} open tag', $compiler->lex->taglineno);
            }
            if (!empty($match[2])) {
                if ('nocache' == trim($match[2])) {
                    $compiler->tag_nocache = true;
                } else {
                    $compiler->trigger_template_error("illegal value of option flag \"{$match[2]}\"", $compiler->lex->taglineno);
                }
            }
            return preg_replace(array("#^{$ldel}\\s*php\\s*(.)*?{$rdel}#", "#{$ldel}\\s*/\\s*php\\s*{$rdel}$#"), array('<?php ', '?>'), $_attr['code']);
        }
    }
}
