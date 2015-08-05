<?php

/**
 * Smarty Internal Plugin Compile extend
 * Compiles the {extends} tag
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

/**
 * Smarty Internal Plugin Compile extend Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Extends extends Smarty_Internal_CompileBase
{
    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $required_attributes = array('file');

    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $shorttag_order = array('file');

    /**
     * Compiles code for the {extends} tag
     *
     * @param array                                        $args     array with attributes from parser
     * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
     *
     * @return string compiled code
     * @throws \SmartyCompilerException
     * @throws \SmartyException
     */
    public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler)
    {
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);
        if ($_attr['nocache'] === true) {
            $compiler->trigger_template_error('nocache option not allowed', null, true);
        }
        if (strpos($_attr['file'], '$_tmp') !== false) {
            $compiler->trigger_template_error('illegal value for file attribute', null, true);
        }

        $name = $_attr['file'];
        if ($compiler->has_variable_string || !((substr_count($name, '"') == 2 || substr_count($name, "'") == 2)) ||
            substr_count($name, '(') != 0 || substr_count($name, '$_smarty_tpl->') != 0
        ) {
            /** @var Smarty_Internal_Template $_smarty_tpl
             * used in evaluated code
             */
            $_smarty_tpl = $compiler->template;
            eval("\$tpl_name = @{$name};");
        } else {
            $tpl_name = trim($name, '\'"');
        }
        // create source object
        $_source = Smarty_Template_Source::load(null, $compiler->smarty, $tpl_name);
        // check for recursion
        $uid = $_source->uid;
        if (isset($compiler->extends_uid[$uid])) {
            $compiler->trigger_template_error("illegal recursive call of \"{$_source->filepath}\"", $compiler->parser->lex->line -
                                                                                                  1);
        }
        $compiler->extends_uid[$uid] = true;
        if (empty($_source->components)) {
            array_unshift($compiler->sources, $_source);
        } else {
            foreach ($_source->components as $source) {
                array_unshift($compiler->sources, $source);
                $uid = $source->uid;
                if (isset($compiler->extends_uid[$uid])) {
                    $compiler->trigger_template_error("illegal recursive call of \"{$source->filepath}\"", $compiler->parser->lex->line -
                                                                                                         1);
                }
                $compiler->extends_uid[$uid] = true;
            }
        }
        $compiler->inheritance_child = true;
        $compiler->parser->lex->yypushstate(Smarty_Internal_Templatelexer::CHILDBODY);
        return '';
    }
}
