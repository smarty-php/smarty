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
class Smarty_Internal_Compile_Block extends Smarty_Internal_Compile_Shared_Inheritance
{
    /**
     * nesting level of block tags
     *
     * @var int
     */
    public static $blockTagNestingLevel = 0;

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
    public $optional_attributes = array('assign');

    /**
     * Saved compiler object
     *
     * @var Smarty_Internal_TemplateCompiler
     */
    public $compiler = null;

    /**
     * Compile saved child block source
     *
     * @param \Smarty_Internal_TemplateCompiler compiler object
     * @param string                            $_name   optional name of child block
     *
     * @return string   compiled code of child block
     */
    static function compileChildBlock(Smarty_Internal_TemplateCompiler $compiler, $_name = null)
    {
        if (!isset($compiler->_cache[ 'blockNesting' ])) {
            $compiler->trigger_template_error(' tag {$smarty.block.child} used outside {block} tags ',
                                              $compiler->parser->lex->taglineno);
        }
        $compiler->has_code = true;
        $compiler->suppressNocacheProcessing = true;
        $compiler->_cache[ 'blockParams' ][ $compiler->_cache[ 'blockNesting' ] ][ 1 ] = true;
        $output =
            "\$_smarty_tpl->_getExtension('_inheritance')->processBlock(\$_smarty_tpl, 2, {$compiler->_cache['blockName'][$compiler->_cache['blockNesting']]}, null, \$_blockParentStack);\n";
        return $output;
    }

    /**
     * Compile $smarty.block.parent
     *
     * @param \Smarty_Internal_TemplateCompiler $compiler compiler object
     * @param string                            $_name    optional name of child block
     *
     * @return string   compiled code of child block
     */
    static function compileParentBlock(Smarty_Internal_TemplateCompiler $compiler, $_name = null)
    {
        if (!isset($compiler->_cache[ 'blockNesting' ])) {
            $compiler->trigger_template_error(' tag {$smarty.block.parent} used outside {block} tags ',
                                              $compiler->parser->lex->taglineno);
        }
        $compiler->suppressNocacheProcessing = true;
        $compiler->has_code = true;
        $output =
            "\$_smarty_tpl->_getExtension('_inheritance')->processBlock(\$_smarty_tpl, 4, {$compiler->_cache['blockName'][$compiler->_cache['blockNesting']]}, null, \$_blockParentStack);\n";
        return $output;
    }

    /**
     * Compiles code for the {block} tag
     *
     * @param  array                             $args      array with attributes from parser
     * @param  \Smarty_Internal_TemplateCompiler $compiler  compiler object
     * @param  array                             $parameter array with compilation parameter
     *
     * @return bool true
     */
    public function compile($args, Smarty_Internal_TemplateCompiler $compiler, $parameter = null)
    {
        if (!isset($compiler->_cache[ 'blockNesting' ])) {
            $compiler->_cache[ 'blockNesting' ] = 0;
        }
        if ($compiler->_cache[ 'blockNesting' ] == 0) {
            // make sure that inheritance gets initialized in template code
            $this->registerInit($compiler);
            $this->option_flags = array('hide', 'nocache', 'append', 'prepend');
        } else {
            $this->option_flags = array('hide', 'nocache');
        }
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);
        $compiler->_cache[ 'blockNesting' ] ++;
        $compiler->_cache[ 'blockName' ][ $compiler->_cache[ 'blockNesting' ] ] = $_attr[ 'name' ];
        $compiler->_cache[ 'blockParams' ][ $compiler->_cache[ 'blockNesting' ] ][ 0 ] =
            'block_' . preg_replace('![^\w]+!', '_', uniqid(rand(), true));
        $compiler->_cache[ 'blockParams' ][ $compiler->_cache[ 'blockNesting' ] ][ 1 ] = false;
        $this->openTag($compiler, 'block', array($_attr, $compiler->nocache, $compiler->parser->current_buffer,
                                                 $compiler->template->compiled->has_nocache_code,
                                                 $compiler->template->caching));
        // must whole block be nocache ?
        if ($compiler->tag_nocache) {
            // TODO rethink nocache handling
        }
        $compiler->nocache = $compiler->nocache | $compiler->tag_nocache;
        // $compiler->suppressNocacheProcessing = true;
        if ($_attr[ 'nocache' ] === true) {
            //$compiler->trigger_template_error('nocache option not allowed', $compiler->parser->lex->taglineno);
        }
        $compiler->parser->current_buffer = new Smarty_Internal_ParseTree_Template();
        $compiler->template->compiled->has_nocache_code = false;
        $compiler->suppressNocacheProcessing = true;
    }
}

/**
 * Smarty Internal Plugin Compile BlockClose Class
 *
 */
class Smarty_Internal_Compile_Blockclose extends Smarty_Internal_Compile_Shared_Inheritance
{
    /**
     * @var Smarty_Internal_TemplateCompiler
     */
    public $compiler = null;

    /**
     * Compiles code for the {/block} tag
     *
     * @param  array                            $args      array with attributes from parser
     * @param \Smarty_Internal_TemplateCompiler $compiler  compiler object
     * @param  array                            $parameter array with compilation parameter
     *
     * @return bool true
     */
    public function compile($args, Smarty_Internal_TemplateCompiler $compiler, $parameter = null)
    {
        list($_attr, $_nocache, $_buffer, $_has_nocache_code, $_caching) = $this->closeTag($compiler, array('block'));
        // init block parameter
        $_block = $compiler->_cache[ 'blockParams' ][ $compiler->_cache[ 'blockNesting' ] ];
        unset($compiler->_cache[ 'blockParams' ][ $compiler->_cache[ 'blockNesting' ] ]);
        $_block[ 2 ] = $_block[ 3 ] = 0;
        $_name = trim($_attr[ 'name' ], "'\"");
        $_assign = isset($_attr[ 'assign' ]) ? $_attr[ 'assign' ] : null;
        unset($_attr[ 'assign' ], $_attr[ 'name' ]);
        foreach ($_attr as $name => $stat) {
            if ((is_bool($stat) && $stat !== false) || (!is_bool($stat) && $stat != 'false')) {
                $_block[ $name ] = is_string($stat) ? trim($stat, "'\"") : $stat;
            }
        }
        if ($compiler->template->source->type == 'file') {
            $sourceInfo = $compiler->template->source->filepath;
        } else {
            $basename = $compiler->template->source->handler->getBasename($compiler->template->source);
            $sourceInfo = $compiler->template->source->type .':' . ($basename ? $basename : $compiler->template->source->name);
        }
        $_funcName = $_block[ 0 ];
        // get compiled block code
        $_functionCode = $compiler->parser->current_buffer->to_smarty_php($compiler->parser);
        // setup buffer for template function code
        $compiler->parser->current_buffer = new Smarty_Internal_ParseTree_Template();

        if ($compiler->template->compiled->has_nocache_code) {
            $_has_nocache_code = true;
            $_block[ 6 ] = $_funcNameCaching = $_funcName . '_nocache';
            $output =
                "/* {block '{$_name}'} {$sourceInfo} */\n";
            $output .= "function {$_funcNameCaching} (\$_smarty_tpl, \$_blockParentStack) {\n";
            $output .= "/*/%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%*/\n";
            $output .= "\$_smarty_tpl->cached->hashes['{$compiler->template->compiled->nocache_hash}'] = true;\n";
            if (isset($_assign)) {
                $output .= "ob_start();\n";
            }
            $output .= $_functionCode;
            if (isset($_assign)) {
                $output .= "\$_smarty_tpl->tpl_vars[{$_assign}] = new Smarty_Variable(ob_get_clean());\n";
            }
            $output .= "/*%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%*/\n";
            $output .= "}\n";
            $output .= "/* {/block '{$_name}'} */\n\n";
            $compiler->parser->current_buffer->append_subtree($compiler->parser,
                                                              new Smarty_Internal_ParseTree_Tag($compiler->parser,
                                                                                                $output));
            $compiler->blockOrFunctionCode .= $compiler->parser->current_buffer->to_smarty_php($compiler->parser);
            $compiler->parser->current_buffer = new Smarty_Internal_ParseTree_Template();
            $this->compiler = $compiler;
            $_functionCode =
                preg_replace_callback("/(echo '\/\*%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%\*\/([\S\s]*?)\/\*\/%%SmartyNocache:{$compiler->template->compiled->nocache_hash}%%\*\/';)/",
                                      array($this, 'removeNocache'), $_functionCode);
            $this->compiler = null;
        }
        $output = "/* {block '{$_name}'}  {$sourceInfo} */\n";
        $output .= "function {$_funcName}(\$_smarty_tpl, \$_blockParentStack) {\n";
        if (isset($_assign)) {
            $output .= "ob_start();\n";
        }
        $output .= $_functionCode;
        if (isset($_assign)) {
            $output .= "\$_smarty_tpl->tpl_vars[{$_assign}] = new Smarty_Variable(ob_get_clean());\n";
        }
        $output .= "}\n";
        $output .= "/* {/block '{$_name}'} */\n\n";
        $compiler->parser->current_buffer->append_subtree($compiler->parser,
                                                          new Smarty_Internal_ParseTree_Tag($compiler->parser,
                                                                                            $output));
        $compiler->blockOrFunctionCode .= $compiler->parser->current_buffer->to_smarty_php($compiler->parser);
        // nocache plugins must be copied
        if (!empty($compiler->template->compiled->required_plugins[ 'nocache' ])) {
            foreach ($compiler->template->compiled->required_plugins[ 'nocache' ] as $plugin => $tmp) {
                foreach ($tmp as $type => $data) {
                    $compiler->parent_compiler->template->compiled->required_plugins[ 'compiled' ][ $plugin ][ $type ] =
                        $data;
                }
            }
        }

        // restore old status
        $compiler->template->compiled->has_nocache_code = $_has_nocache_code;
        $compiler->tag_nocache = $compiler->nocache;
        $compiler->nocache = $_nocache;
        $compiler->parser->current_buffer = $_buffer;
        $output = '';
        if ($compiler->_cache[ 'blockNesting' ] == 1) {
            $output .= "\$_smarty_tpl->_getExtension('_inheritance')->processBlock(\$_smarty_tpl, 0, {$compiler->_cache['blockName'][$compiler->_cache['blockNesting']]}, " .
                var_export($_block, true) . ");\n";
        } else {
            $output .= "\$_smarty_tpl->_getExtension('_inheritance')->processBlock(\$_smarty_tpl, 0, {$compiler->_cache['blockName'][$compiler->_cache['blockNesting']]}, " .
                var_export($_block, true) . ", \$_blockParentStack);\n";
        }
        $compiler->_cache[ 'blockNesting' ] --;
        if ($compiler->_cache[ 'blockNesting' ] == 0) {
            unset($compiler->_cache[ 'blockNesting' ]);
        }
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
            preg_replace("/(echo '\/\*%%SmartyNocache:{$this->compiler->template->compiled->nocache_hash}%%\*\/)|(\/\*\/%%SmartyNocache:{$this->compiler->template->compiled->nocache_hash}%%\*\/';)/",
                         '', $match[ 0 ]);
        $code = str_replace(array('\\\'', '\\\\\''), array('\'', '\\\''), $code);
        return $code;
    }
}
