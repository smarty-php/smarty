<?php
/**
 * Smarty Internal Plugin Compile Foreach
 * Compiles the {foreach} {foreachelse} {/foreach} tags
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

/**
 * Smarty Internal Plugin Compile Foreach Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Foreach extends Smarty_Internal_CompileBase
{
    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $required_attributes = array('from', 'item');

    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $optional_attributes = array('name', 'key');

    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $shorttag_order = array('from', 'item', 'key', 'name');

    /**
     * Foreach counter
     *
     * @var int
     */
    public $foreach_number = 0;

    /**
     * Compiles code for the {foreach} tag
     *
     * @param  array                                $args      array with attributes from parser
     * @param \Smarty_Internal_TemplateCompilerBase $compiler  compiler object
     * @param  array                                $parameter array with compilation parameter
     *
     * @return string compiled code
     * @throws \SmartyCompilerException
     */
    public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter)
    {
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);
        $from = $_attr['from'];
        $item = $compiler->getId($_attr['item']);
        if ($item === false) {
            $item = $compiler->getVariableName($_attr['item']);
        }
        $attributes = array('item' => $item);
        if (isset($_attr['key'])) {
            $key = $compiler->getId($_attr['key']);
            if ($key === false) {
                $key = $compiler->getVariableName($_attr['key']);
            }
            $attributes['key'] = $key;
        }
        if (isset($_attr['name'])) {
            $attributes['name'] = $compiler->getId($_attr['name']);
        }
        foreach ($attributes as $a => $v) {
            if ($v === false) {
                $compiler->trigger_template_error("'{$a}' attribute/variable has illegal value", $compiler->lex->taglineno);
            }
        }
        $fromName = $compiler->getVariableName($_attr['from']);
        if ($fromName) {
            foreach (array('item', 'key') as $a) {
                if (isset($attributes[$a]) && $attributes[$a] == $fromName) {
                    $compiler->trigger_template_error("'{$a}' and 'from' may not have same variable name '{$fromName}'", $compiler->lex->taglineno);
                }
            }
        }
        $attributes['no'] = $this->foreach_number ++ . '_' . (isset($attributes['name']) ? $attributes['name'] : $attributes['item']);
        $this->openTag($compiler, 'foreach', array('foreach', $compiler->nocache, $attributes, true));
        // maybe nocache because of nocache variables
        $compiler->nocache = $compiler->nocache | $compiler->tag_nocache;

        // prepare preg
        if ($has_name = isset($attributes['name'])) {
            $smartyPreg = "|([\$]smarty[.]foreach[.]{$attributes['name']}[.]((first)|(last)|(index)|(iteration)|(show)|(total)))";
        } else {
            $smartyPreg = '';
        }
        $itemPreg = "([\$]{$attributes['item']}[@]((first)|(last)|(index)|(iteration)|(show)|(total)|(key)))";
        $preg = '~(' . $itemPreg . $smartyPreg . ')\W~i';
        $itemAttr = array();
        $smartyAttr = array();

        // search template source
        preg_match_all($preg, $compiler->lex->data, $match, PREG_SET_ORDER);
        foreach ($match as $m) {
            if (isset($m[3]) && !empty($m[3])) {
                $itemAttr[strtolower($m[3])] = true;
            }
            if ($has_name && isset($m[12]) && !empty($m[12])) {
                $smartyAttr[strtolower($m[12])] = true;
            }
        }

        // search {block} sources
        foreach ($compiler->template->block_data as $b) {
            if (isset($b['source'])) {
                preg_match_all($preg, $b['source'], $match, PREG_SET_ORDER);
                foreach ($match as $m) {
                    if (isset($m[3]) && !empty($m[3])) {
                        $itemAttr[strtolower($m[3])] = true;
                    }
                    if ($has_name && isset($m[12]) && !empty($m[12])) {
                        $smartyAttr[strtolower($m[12])] = true;
                    }
                }
            }
        }
        if (class_exists('Smarty_Internal_Compile_Block', false)) {
            foreach (Smarty_Internal_Compile_Block::$block_data as $b) {
                if (isset($b['source'])) {
                    preg_match_all($preg, $b['source'], $match, PREG_SET_ORDER);
                    foreach ($match as $m) {
                        if (isset($m[3]) && !empty($m[3])) {
                            $itemAttr[strtolower($m[3])] = true;
                        }
                        if ($has_name && isset($m[12]) && !empty($m[12])) {
                            $smartyAttr[strtolower($m[12])] = true;
                        }
                    }
                }
            }
        }

        // search parent compiler template source
        $nextCompiler = $compiler;
        while ($nextCompiler !== $nextCompiler->parent_compiler) {
            $nextCompiler = $nextCompiler->parent_compiler;
            preg_match_all($preg, $nextCompiler->template->source->content, $match, PREG_SET_ORDER);
            foreach ($match as $m) {
                if (isset($m[3]) && !empty($m[3])) {
                    $itemAttr[strtolower($m[3])] = true;
                }
                if ($has_name && isset($m[12]) && !empty($m[12])) {
                    $smartyAttr[strtolower($m[12])] = true;
                }
            }
        }

        if (!isset($itemAttr['index']) && (isset($itemAttr['first']) || isset($itemAttr['last']))) {
            $itemAttr['iteration'] = true;
        }
        if (isset($itemAttr['last'])) {
            $itemAttr['total'] = true;
        }
        if ($has_name) {
            if (!isset($smartyAttr['index']) && (isset($smartyAttr['first']) || isset($smartyAttr['last']))) {
                $smartyAttr['iteration'] = true;
            }
            if (isset($smartyAttr['last'])) {
                $smartyAttr['total'] = true;
            }
        }

        $keyTerm = '';
        if (isset($itemAttr['key'])) {
            $keyTerm = "\$_smarty_tpl->tpl_vars['{$item}']->key => ";
        } elseif (isset($attributes['key'])) {
            $keyTerm = "\$_smarty_tpl->tpl_vars['{$key}']->value => ";
        }
        // generate output code
        $output = "<?php\n";
        foreach (array('item', 'key') as $a) {
            if (isset($attributes[$a])) {
                $output .= "\$foreach_{$attributes['no']}_sav['s_{$a}'] = isset(\$_smarty_tpl->tpl_vars['{$attributes[$a]}']) ? \$_smarty_tpl->tpl_vars['{$attributes[$a]}'] : false;\n";
            }
        }
        if (isset($attributes['name'])) {
            $output .= "\$foreach_{$attributes['no']}_sav['s_name'] = isset(\$_smarty_tpl->tpl_vars['__foreach_{$attributes['name']}']) ? \$_smarty_tpl->tpl_vars['__foreach_{$attributes['name']}'] : false;\n";
        }
        $output .= "\$_from = $from;\n";
        $output .= "if (!is_array(\$_from) && !is_object(\$_from)) {\n";
        $output .= "settype(\$_from, 'array');\n";
        $output .= "}\n";
        $output .= "\$_smarty_tpl->tpl_vars['{$item}'] = new Smarty_Variable;\n";
        $output .= "\$_smarty_tpl->tpl_vars['{$item}']->_loop = false;\n";
        if (isset($attributes['key'])) {
            $output .= "\$_smarty_tpl->tpl_vars['{$key}'] = new Smarty_Variable;\n";
        }
        if (isset($itemAttr['total'])) {
            $output .= "\$_smarty_tpl->tpl_vars['{$item}']->total= \$_smarty_tpl->_count(\$_from);\n";
        }
        if (isset($itemAttr['iteration'])) {
            $output .= "\$_smarty_tpl->tpl_vars['{$item}']->iteration=0;\n";
        }
        if (isset($itemAttr['index'])) {
            $output .= "\$_smarty_tpl->tpl_vars['{$item}']->index=-1;\n";
        }
        if (isset($itemAttr['show'])) {
            if (isset($itemAttr['total'])) {
                $output .= "\$_smarty_tpl->tpl_vars['{$item}']->show = (\$_smarty_tpl->tpl_vars['{$item}']->total > 0);\n";
            } else {
                $output .= "\$_smarty_tpl->tpl_vars['{$item}']->show = (\$_smarty_tpl->_count(\$_from) > 0);\n";
            }
        }
        if ($has_name) {
            $prop = array();
            if (isset($smartyAttr['total'])) {
                $prop['total'] = "'total' => ";
                $prop['total'] .= isset($smartyAttr['show']) ? '$total = ' : '';
                $prop['total'] .= '$_smarty_tpl->_count($_from)';
            }
            if (isset($smartyAttr['iteration'])) {
                $prop['iteration'] = "'iteration' => 0";
            }
            if (isset($smartyAttr['index'])) {
                $prop['index'] = "'index' => -1";
            }
            if (isset($smartyAttr['show'])) {
                $prop['show'] = "'show' => ";
                if (isset($smartyAttr['total'])) {
                    $prop['show'] .= "(\$total > 0)";
                } else {
                    $prop['show'] .= "(\$_smarty_tpl->_count(\$_from) > 0)";
                }
            }
            if (!empty($smartyAttr)) {
                $_vars = 'array(' . join(', ', $prop) . ')';
                $foreachVar = "'__foreach_{$attributes['name']}'";
                $output .= "\$_smarty_tpl->tpl_vars[$foreachVar] = new Smarty_Variable({$_vars});\n";
            }
        }
        $output .= "foreach (\$_from as {$keyTerm}\$_smarty_tpl->tpl_vars['{$item}']->value) {\n";
        $output .= "\$_smarty_tpl->tpl_vars['{$item}']->_loop = true;\n";
        if (isset($attributes['key']) && isset($itemAttr['key'])) {
            $output .= "\$_smarty_tpl->tpl_vars['{$key}']->value = \$_smarty_tpl->tpl_vars['{$item}']->key;\n";
        }
        if (isset($itemAttr['iteration'])) {
            $output .= "\$_smarty_tpl->tpl_vars['{$item}']->iteration++;\n";
        }
        if (isset($itemAttr['index'])) {
            $output .= "\$_smarty_tpl->tpl_vars['{$item}']->index++;\n";
        }
        if (isset($itemAttr['first'])) {
            if (isset($itemAttr['index'])) {
                $output .= "\$_smarty_tpl->tpl_vars['{$item}']->first = \$_smarty_tpl->tpl_vars['{$item}']->index == 0;\n";
            } else {
                $output .= "\$_smarty_tpl->tpl_vars['{$item}']->first = \$_smarty_tpl->tpl_vars['{$item}']->iteration == 1;\n";
            }
        }
        if (isset($itemAttr['last'])) {
            if (isset($itemAttr['index'])) {
                $output .= "\$_smarty_tpl->tpl_vars['{$item}']->last = \$_smarty_tpl->tpl_vars['{$item}']->index + 1 == \$_smarty_tpl->tpl_vars['{$item}']->total;\n";
            } else {
                $output .= "\$_smarty_tpl->tpl_vars['{$item}']->last = \$_smarty_tpl->tpl_vars['{$item}']->iteration == \$_smarty_tpl->tpl_vars['{$item}']->total;\n";
            }
        }
        if ($has_name) {
            if (isset($smartyAttr['iteration'])) {
                $output .= "\$_smarty_tpl->tpl_vars[$foreachVar]->value['iteration']++;\n";
            }
            if (isset($smartyAttr['index'])) {
                $output .= "\$_smarty_tpl->tpl_vars[$foreachVar]->value['index']++;\n";
            }
            if (isset($smartyAttr['first'])) {
                if (isset($smartyAttr['index'])) {
                    $output .= "\$_smarty_tpl->tpl_vars[$foreachVar]->value['first'] = \$_smarty_tpl->tpl_vars[$foreachVar]->value['index'] == 0;\n";
                } else {
                    $output .= "\$_smarty_tpl->tpl_vars[$foreachVar]->value['first'] = \$_smarty_tpl->tpl_vars[$foreachVar]->value['iteration'] == 1;\n";
                }
            }
            if (isset($smartyAttr['last'])) {
                if (isset($smartyAttr['index'])) {
                    $output .= "\$_smarty_tpl->tpl_vars[$foreachVar]->value['last'] = \$_smarty_tpl->tpl_vars[$foreachVar]->value['index'] + 1 == \$_smarty_tpl->tpl_vars[$foreachVar]->value['total'];\n";
                } else {
                    $output .= "\$_smarty_tpl->tpl_vars[$foreachVar]->value['last'] = \$_smarty_tpl->tpl_vars[$foreachVar]->value['iteration'] == \$_smarty_tpl->tpl_vars[$foreachVar]->value['total'];\n";
                }
            }
        }
        $output .= "\$foreach_{$attributes['no']}_sav['item'] = \$_smarty_tpl->tpl_vars['{$item}'];\n";
        $output .= "?>";

        return $output;
    }

    /**
     * Compiles code for the {$smarty.foreach} tag
     *
     * @param  array                                $args      array with attributes from parser
     * @param \Smarty_Internal_TemplateCompilerBase $compiler  compiler object
     * @param  array                                $parameter array with compilation parameter
     *
     * @return string compiled code
     * @throws \SmartyCompilerException
     */
    public static function compileSpecialVariable($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter)
    {
        if (!isset($parameter[1]) || false === $name = $compiler->getId($parameter[1])) {
            $compiler->trigger_template_error("missing or illegal \$Smarty.foreach name attribute", $compiler->lex->taglineno);
        }
        if ((!isset($parameter[2]) || false === $property = $compiler->getId($parameter[2])) || !in_array(strtolower($property), array('first', 'last', 'index', 'iteration', 'show', 'total'))) {
            $compiler->trigger_template_error("missing or illegal \$Smarty.foreach property attribute", $compiler->lex->taglineno);
        }
        $property = strtolower($property);
        $foreachVar = "'__foreach_{$name}'";
        return "(isset(\$_smarty_tpl->tpl_vars[{$foreachVar}]->value['{$property}']) ? \$_smarty_tpl->tpl_vars[{$foreachVar}]->value['{$property}'] : null)";
    }
}

/**
 * Smarty Internal Plugin Compile Foreachelse Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Foreachelse extends Smarty_Internal_CompileBase
{
    /**
     * Compiles code for the {foreachelse} tag
     *
     * @param  array                                $args      array with attributes from parser
     * @param \Smarty_Internal_TemplateCompilerBase $compiler  compiler object
     * @param  array                                $parameter array with compilation parameter
     *
     * @return string compiled code
     */
    public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter)
    {
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);

        list($openTag, $nocache, $attributes, $foo) = $this->closeTag($compiler, array('foreach'));
        $this->openTag($compiler, 'foreachelse', array('foreachelse', $nocache, $attributes, false));
        $output = "<?php\n";
        $output .= "\$_smarty_tpl->tpl_vars['{$attributes['item']}'] = \$foreach_{$attributes['no']}_sav['item'];\n";
        $output .= "}\n";
        $output .= "if (!\$_smarty_tpl->tpl_vars['{$attributes['item']}']->_loop) {\n?>";
        return $output;
    }
}

/**
 * Smarty Internal Plugin Compile Foreachclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Foreachclose extends Smarty_Internal_CompileBase
{
    /**
     * Compiles code for the {/foreach} tag
     *
     * @param  array                                $args      array with attributes from parser
     * @param \Smarty_Internal_TemplateCompilerBase $compiler  compiler object
     * @param  array                                $parameter array with compilation parameter
     *
     * @return string compiled code
     */
    public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter)
    {
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);
        // must endblock be nocache?
        if ($compiler->nocache) {
            $compiler->tag_nocache = true;
        }

        list($openTag, $compiler->nocache, $attributes, $restore) = $this->closeTag($compiler, array('foreach',
            'foreachelse'));
        $output = "<?php\n";
        if ($restore) {
            $output .= "\$_smarty_tpl->tpl_vars['{$attributes['item']}'] = \$foreach_{$attributes['no']}_sav['item'];\n";
        }
        $output .= "}\n";
        foreach (array('item', 'key') as $a) {
            if (isset($attributes[$a])) {
                $output .= "if (\$foreach_{$attributes['no']}_sav['s_{$a}']) {\n";
                $output .= "\$_smarty_tpl->tpl_vars['{$attributes[$a]}'] = \$foreach_{$attributes['no']}_sav['s_{$a}'];\n";
                $output .= "}\n";
            }
        }
        if (isset($attributes['name'])) {
            $output .= "if (\$foreach_{$attributes['no']}_sav['s_name']) {\n";
            $output .= "\$_smarty_tpl->tpl_vars['__foreach_{$attributes['name']}'] = \$foreach_{$attributes['no']}_sav['s_name'];\n";
            $output .= "}\n";
        }
        $output .= "?>";

        return $output;
    }
}
