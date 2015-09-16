<?php
/*
 * This file is part of Smarty.
 *
 * (c) 2015 Uwe Tews
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Smarty Internal Plugin Compile Block Class
 *
 * @author Uwe Tews <uwe.tews@googlemail.com>
 */
class Smarty_Internal_Compile_Block extends Smarty_Internal_CompileBase
{
    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $required_attributes = array('name');

    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $shorttag_order = array('name');

    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $option_flags = array('hide', 'nocache');

    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $optional_attributes = array();

    /**
     * nesting level of block tags
     *
     * @var int
     */
    public static $blockTagNestingLevel = 0;

    /**
     * Compiles code for the {block} tag
     *
     * @param  array                                 $args      array with attributes from parser
     * @param  \Smarty_Internal_TemplateCompilerBase $compiler  compiler object
     * @param  array                                 $parameter array with compilation parameter
     *
     * @return bool true
     */
    public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter)
    {
        if ($compiler->blockTagNestingLevel == 0 && $compiler->inheritanceChild) {
            $this->option_flags = array('hide', 'nocache', 'append', 'prepend');
        } else {
            $this->option_flags = array('hide', 'nocache');
        }
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);
        $compiler->blockTagNestingLevel ++;
        $this->openTag($compiler, 'block', array($_attr, $compiler->nocache, $compiler->parser->current_buffer,
                                                 $compiler->template->compiled->has_nocache_code,
                                                 $compiler->template->caching));
        // must whole block be nocache ?
        if ($compiler->tag_nocache) {
            $i = 0;
        }
        $compiler->nocache = $compiler->nocache | $compiler->tag_nocache;
        // $compiler->suppressNocacheProcessing = true;
        if ($_attr['nocache'] === true) {
            //$compiler->trigger_template_error('nocache option not allowed', $compiler->parser->lex->taglineno);
        }
        $compiler->parser->current_buffer = new Smarty_Internal_ParseTree_Template();
        $compiler->template->compiled->has_nocache_code = false;
        $compiler->suppressNocacheProcessing = true;
    }

    /**
     * Compile saved child block source
     *
     * @param \Smarty_Internal_TemplateCompilerBase compiler object
     * @param string                                $_name   optional name of child block
     *
     * @return string   compiled code of child block
     */
    static function compileChildBlock(Smarty_Internal_TemplateCompilerBase $compiler, $_name = null)
    {
        if (!$compiler->blockTagNestingLevel) {
            $compiler->trigger_template_error(' tag {$smarty.block.child} used outside {block} tags ',
                                              $compiler->parser->lex->taglineno);
        }
        $compiler->has_code = true;
        $compiler->suppressNocacheProcessing = true;
        $compiler->callChildBlock[$compiler->blockTagNestingLevel] = true;
        $_output = "<?php \n\$_smarty_tpl->_Block->callChildBlock(\$_smarty_tpl, \$block);?>";
        return $_output;
    }

    /**
     * Compile $smarty.block.parent
     *
     * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
     * @param string                                $_name    optional name of child block
     *
     * @return string   compiled code of child block
     */
    static function compileParentBlock(Smarty_Internal_TemplateCompilerBase $compiler, $_name = null)
    {
        if (!$compiler->inheritanceChild) {
            $compiler->trigger_template_error(' tag {$smarty.block.parent} used in parent template ',
                                              $compiler->parser->lex->taglineno);
        }
        if (!$compiler->blockTagNestingLevel) {
            $compiler->trigger_template_error(' tag {$smarty.block.parent} used outside {block} tags ',
                                              $compiler->parser->lex->taglineno);
        }
        $compiler->suppressNocacheProcessing = true;
        $compiler->has_code = true;
        $_output = "<?php \n\$_smarty_tpl->_Block->callParentBlock(\$_smarty_tpl, \$block);?>";
        return $_output;
    }
}

/**
 * Smarty Internal Plugin Compile BlockClose Class
 *
 */
class Smarty_Internal_Compile_Blockclose extends Smarty_Internal_CompileBase
{
    /**
     * Compiles code for the {/block} tag
     *
     * @param  array                                $args      array with attributes from parser
     * @param \Smarty_Internal_TemplateCompilerBase $compiler  compiler object
     * @param  array                                $parameter array with compilation parameter
     *
     * @return bool true
     */
    public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter)
    {
        $this->compiler = $compiler;
        list($_attr, $_nocache, $_buffer, $_has_nocache_code, $_caching) = $this->closeTag($compiler, array('block'));
        $_name = trim($_attr['name'], "'\"");
        $_functionCode = $compiler->parser->current_buffer;
        // setup buffer for template function code
        $compiler->parser->current_buffer = new Smarty_Internal_ParseTree_Template();

        $_funcNameCaching =
        $_funcName = preg_replace('![^\w]+!', '_', "block_function_{$_name}_" . uniqid(rand(), true));
        if ($compiler->template->compiled->has_nocache_code) {
            //            $compiler->parent_compiler->template->tpl_function[$_name]['call_name_caching'] = $_funcNameCaching;
            $_funcNameCaching .= '_nocache';
            $output = "<?php\n\n";
            $output .= "/* {block '{$_name}'} {$compiler->template->source->type}:{$compiler->template->source->name} */\n";
            $output .= "function {$_funcNameCaching} (\$_smarty_tpl, \$block) {\n";
            $output .= "/*/%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%*/\n";
            $output .= "\$_smarty_tpl->cached->hashes['{$compiler->template->compiled->nocache_hash}'] = true;\n?>\n";
            $compiler->parser->current_buffer->append_subtree($compiler->parser,
                                                              new Smarty_Internal_ParseTree_Tag($compiler->parser,
                                                                                                $output));
            $compiler->parser->current_buffer->append_subtree($compiler->parser, $_functionCode);
            $output = "<?php /*%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%*/\n";
            $output .= "\n}\n";
            $output .= "/* {/block '{$_name}'} */\n\n";
            $output .= "?>\n";
            $compiler->parser->current_buffer->append_subtree($compiler->parser,
                                                              new Smarty_Internal_ParseTree_Tag($compiler->parser,
                                                                                                $output));
            $compiler->blockOrFunctionCode .= $f =
                $compiler->parser->current_buffer->to_smarty_php($compiler->parser);
            $compiler->parser->current_buffer = new Smarty_Internal_ParseTree_Template();
            $_functionCode = new Smarty_Internal_ParseTree_Tag($compiler->parser,
                                                               preg_replace_callback("/((<\?php )?echo '\/\*%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%\*\/([\S\s]*?)\/\*\/%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%\*\/';(\?>\n)?)/",
                                                                                     array($this, 'removeNocache'),
                                                                                     $_functionCode->to_smarty_php($compiler->parser)));
        }
        $output = "<?php\n\n";
        $output .= "/* {block '{$_name}'}  {$compiler->template->source->type}:{$compiler->template->source->name} */\n";
        $output .= "function {$_funcName}(\$_smarty_tpl, \$block) {?>";
        $compiler->parser->current_buffer->append_subtree($compiler->parser,
                                                          new Smarty_Internal_ParseTree_Tag($compiler->parser,
                                                                                            $output));
        $compiler->parser->current_buffer->append_subtree($compiler->parser, $_functionCode);
        $output = "<?php\n}\n";
        $output .= "/* {/block '{$_name}'} */\n\n";
        $output .= "?>\n";
        $compiler->parser->current_buffer->append_subtree($compiler->parser,
                                                          new Smarty_Internal_ParseTree_Tag($compiler->parser,
                                                                                            $output));
        $compiler->blockOrFunctionCode .= $f =
            $compiler->parser->current_buffer->to_smarty_php($compiler->parser);
        // nocache plugins must be copied
        if (!empty($compiler->template->compiled->required_plugins['nocache'])) {
            foreach ($compiler->template->compiled->required_plugins['nocache'] as $plugin => $tmp) {
                foreach ($tmp as $type => $data) {
                    $compiler->parent_compiler->template->compiled->required_plugins['compiled'][$plugin][$type] =
                        $data;
                }
            }
        }
        // restore old status
        $compiler->template->compiled->has_nocache_code = $_has_nocache_code;
        $compiler->tag_nocache = $compiler->nocache;
        $compiler->nocache = $_nocache;
        $compiler->parser->current_buffer = $_buffer;

        $_parameter = $_attr;
        foreach ($_parameter as $name => $stat) {
            if ($stat === false) {
                unset($_parameter[$name]);
            }
        }
        if (isset($compiler->callChildBlock[$compiler->blockTagNestingLevel])) {
            $_parameter['callChildBlock'] = 'true';
            unset($compiler->callChildBlock[$compiler->blockTagNestingLevel]);
        }
        $compiler->blockTagNestingLevel --;
        // inner {block} or child template {block} must register block
        if ($compiler->blockTagNestingLevel == 0 && $compiler->inheritanceChild) {
            $_function = 'register';
        } else {
            $_function = 'call';
        }
        $cm = $compiler->template->caching ? 'true' : 'false';
        $output =
            "<?php \n\$_smarty_tpl->_Block->{$_function}Block(\$_smarty_tpl, array('caching' => {$cm}, 'function' => '{$_funcNameCaching}'";
        foreach ($_parameter as $name => $stat) {
            if ($stat !== false) {
                $output .= ", '{$name}' => {$stat}";
            }
        }
        $output .= "));\n?>\n";
        $compiler->has_code = true;
        $compiler->suppressNocacheProcessing = true;
        return $output;
    }

    /**
     * @param $match
     *
     * @return mixed
     */
    function removeNocache($match)
    {
        $code =
            preg_replace("/((<\?php )?echo '\/\*%%SmartyNocache:{$this->compiler->template->compiled->nocache_hash}%%\*\/)|(\/\*\/%%SmartyNocache:{$this->compiler->template->compiled->nocache_hash}%%\*\/';(\?>\n)?)/",
                         '', $match[0]);
        $code = str_replace(array('\\\'', '\\\\\''), array('\'', '\\\''), $code);
        return $code;
    }
}
