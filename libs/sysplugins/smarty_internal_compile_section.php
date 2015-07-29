<?php
/**
 * Smarty Internal Plugin Compile Section
 * Compiles the {section} {sectionelse} {/section} tags
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

/**
 * Smarty Internal Plugin Compile Section Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Section extends Smarty_Internal_CompileBase
{
    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $required_attributes = array('name', 'loop');

    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $shorttag_order = array('name', 'loop');

    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Smarty_Internal_CompileBase
     */
    public $optional_attributes = array('start', 'step', 'max', 'show');

    /**
     * counter
     *
     * @var int
     */
    public $counter = 0;

    /**
     * Compiles code for the {section} tag
     *
     * @param  array  $args     array with attributes from parser
     * @param  object $compiler compiler object
     *
     * @return string compiled code
     */
    public function compile($args, $compiler)
    {
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);
        $attributes = array('name' => $compiler->getId($_attr['name']));
        unset($_attr['name']);
        foreach ($attributes as $a => $v) {
            if ($v === false) {
                $compiler->trigger_template_error("'{$a}' attribute/variable has illegal value", $compiler->lex->taglineno);
            }
        }
        $attributes['no'] = $this->counter ++ . '_' . $attributes['name'];

        $this->openTag($compiler, 'section', array('section', $compiler->nocache, $attributes));
        // maybe nocache because of nocache variables
        $compiler->nocache = $compiler->nocache | $compiler->tag_nocache;
        $sectionVar = "\$_smarty_tpl->tpl_vars['__section_{$attributes['name']}']->value";
        $local = "\$section_{$attributes['no']}_";

        // prepare preg
        $smartyPreg = "|([\$]smarty[.]section[.]{$attributes['name']}[.]((first)|(last)|(index)|(iteration)|(show)|(total)|(rownum)|(index_prev)|(index_next)))";
        $itemPreg = "(\[{$attributes['name']}[.]((first)|(last)|(index)|(iteration)|(show)|(total)|(rownum)|(index_prev)|(index_next))\])";
        $preg = '~(' . $itemPreg . $smartyPreg . ')\W~i';
        $smartyAttr = array('initLocal' => array('s_name' => "isset(\$_smarty_tpl->tpl_vars['__section_{$attributes['name']}']) ? \$_smarty_tpl->tpl_vars['__section_{$attributes['name']}'] : false",),
                            'isSmarty'  => array('index' => true,), 'initSmarty' => array(), 'initFor' => array(),
                            'incFor'    => array(),
                            'value'     => array('index'     => "{$sectionVar}['index']", 'show' => 'true', 'step' => 1,
                                                 'iteration' => "{$local}iteration",

                            ), 'type'   => array('index' => 2, 'iteration' => 2, 'show' => 0, 'step' => 0,),
                            'cmpFor'    => array(), 'before' => array(), 'after' => array(),);

        // search template source
        preg_match_all($preg, $compiler->lex->data, $match, PREG_SET_ORDER);
        foreach ($match as $m) {
            if (isset($m[3]) && !empty($m[3])) {
                $smartyAttr['isSmarty'][strtolower($m[3])] = true;
            }
            if (isset($m[14]) && !empty($m[14])) {
                $smartyAttr['isSmarty'][strtolower($m[14])] = true;
            }
        }

        // search {block} sources
        foreach ($compiler->template->block_data as $b) {
            if (isset($b['source'])) {
                preg_match_all($preg, $b['source'], $match, PREG_SET_ORDER);
                foreach ($match as $m) {
                    if (isset($m[3]) && !empty($m[3])) {
                        $smartyAttr['isSmarty'][strtolower($m[3])] = true;
                    }
                    if (isset($m[14]) && !empty($m[14])) {
                        $smartyAttr['isSmarty'][strtolower($m[14])] = true;
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
                            $smartyAttr['isSmarty'][strtolower($m[3])] = true;
                        }
                        if (isset($m[14]) && !empty($m[14])) {
                            $smartyAttr['isSmarty'][strtolower($m[14])] = true;
                        }
                    }
                }
            }
        }

        // search parent compiler template source
        $nextCompiler = $compiler;
        while ($nextCompiler !== $nextCompiler->parent_compiler) {
            $nextCompiler = $nextCompiler->parent_compiler;
            preg_match_all($preg, $nextCompiler->template->source->getContent(), $match, PREG_SET_ORDER);
            foreach ($match as $m) {
                if (isset($m[3]) && !empty($m[3])) {
                    $smartyAttr['isSmarty'][strtolower($m[3])] = true;
                }
                if (isset($m[14]) && !empty($m[14])) {
                    $smartyAttr['isSmarty'][strtolower($m[14])] = true;
                }
            }
        }

        $output = "<?php ";
        foreach ($_attr as $attr_name => $attr_value) {
            switch ($attr_name) {
                case 'loop':
                    if (is_numeric($attr_value)) {
                        $v = (int) $attr_value;
                        $t = 0;
                    } else {
                        $v = "(is_array(@\$_loop=$attr_value) ? count(\$_loop) : max(0, (int) \$_loop))";
                        $t = 1;
                    }
                    if (isset($smartyAttr['isSmarty']['loop'])) {
                        $smartyAttr['initSmarty']['loop'] = "'loop' => {$v}";
                        if ($t == 1) {
                            $v = "{$sectionVar}['loop']";
                        }
                    } elseif ($t == 1) {
                        $smartyAttr['initLocal']['loop'] = $v;
                        $v = "{$local}loop";
                    }
                    break;
                case 'show':
                    if (is_bool($attr_value)) {
                        $v = $attr_value ? 'true' : 'false';
                        $t = 0;
                    } else {
                        $v = "(bool) $attr_value";
                        $t = 3;
                    }
                    break;
                case 'step':
                    if (is_numeric($attr_value)) {
                        $v = (int) $attr_value;
                        $v = ($v == 0) ? 1 : $v;
                        $t = 0;
                        break;
                    }
                    $smartyAttr['initLocal']['step'] = "((int)@$attr_value) == 0 ? 1 : (int)@$attr_value";
                    $v = "{$local}step";
                    $t = 2;
                    break;

                case 'max':
                case 'start':
                    if (is_numeric($attr_value)) {
                        $v = (int) $attr_value;
                        $t = 0;
                        break;
                    }
                    $v = "(int)@$attr_value";
                    $t = 3;
                    break;
            }
            if ($t == 3 && $compiler->getId($attr_value)) {
                $t = 1;
            }
            $smartyAttr['value'][$attr_name] = $v;
            $smartyAttr['type'][$attr_name] = $t;
        }

        if (isset($smartyAttr['isSmarty']['step'])) {
            $smartyAttr['initSmarty']['step'] = $smartyAttr['value']['step'];
        }
        if (isset($smartyAttr['isSmarty']['iteration'])) {
            $smartyAttr['value']['iteration'] = "{$sectionVar}['iteration']";
        }
        $smartyAttr['incFor']['iteration'] = "{$smartyAttr['value']['iteration']}++";
        $smartyAttr['initFor']['iteration'] = "{$smartyAttr['value']['iteration']} = 1";

        if ($smartyAttr['type']['step'] == 0) {
            if ($smartyAttr['value']['step'] == 1) {
                $smartyAttr['incFor']['index'] = "{$sectionVar}['index']++";
            } elseif ($smartyAttr['value']['step'] > 1) {
                $smartyAttr['incFor']['index'] = "{$sectionVar}['index'] += {$smartyAttr['value']['step']}";
            } else {
                $smartyAttr['incFor']['index'] = "{$sectionVar}['index'] -= " . - $smartyAttr['value']['step'];
            }
        } else {
            $smartyAttr['incFor']['index'] = "{$sectionVar}['index'] += {$smartyAttr['value']['step']}";
        }

        if (!isset($smartyAttr['value']['max'])) {
            $smartyAttr['value']['max'] = $smartyAttr['value']['loop'];
            $smartyAttr['type']['max'] = $smartyAttr['type']['loop'];
        } elseif ($smartyAttr['type']['max'] != 0) {
            $smartyAttr['value']['max'] = "{$smartyAttr['value']['max']} < 0 ? {$smartyAttr['value']['loop']} : {$smartyAttr['value']['max']}";
            $smartyAttr['type']['max'] = 1;
        } else {
            if ($smartyAttr['value']['max'] < 0) {
                $smartyAttr['value']['max'] = $smartyAttr['value']['loop'];
                $smartyAttr['type']['max'] = $smartyAttr['type']['loop'];
            }
        }

        if (!isset($smartyAttr['value']['start'])) {
            $start_code = array(1 => "{$smartyAttr['value']['step']} > 0 ? ", 2 => '0', 3 => ' : ',
                                4 => $smartyAttr['value']['loop'], 5 => ' - 1');
            if ($smartyAttr['type']['loop'] == 0) {
                $start_code[5] = '';
                $start_code[4] = $smartyAttr['value']['loop'] - 1;
            }
            if ($smartyAttr['type']['step'] == 0) {
                if ($smartyAttr['value']['step'] > 0) {
                    $start_code = array(1 => '0');
                    $smartyAttr['type']['start'] = 0;
                } else {
                    $start_code[1] = $start_code[2] = $start_code[3] = '';
                    $smartyAttr['type']['start'] = $smartyAttr['type']['loop'];
                }
            } else {
                $smartyAttr['type']['start'] = 1;
            }
            $smartyAttr['value']['start'] = join('', $start_code);
        } else {
            $start_code = array(1 => "{$smartyAttr['value']['start']} < 0 ? ", 2 => 'max(',
                                3 => "{$smartyAttr['value']['step']} > 0 ? ", 4 => '0', 5 => ' : ', 6 => '-1',
                                7 => ', ', 8 => "{$smartyAttr['value']['start']} + {$smartyAttr['value']['loop']}",
                                10 => ')', 11 => ' : ', 12 => 'min(', 13 => $smartyAttr['value']['start'], 14 => ', ',
                                15 => "{$smartyAttr['value']['step']} > 0 ? ", 16 => $smartyAttr['value']['loop'],
                                17 => ' : ', 18 => $smartyAttr['type']['loop'] == 0 ? $smartyAttr['value']['loop'] -
                    1 : "{$smartyAttr['value']['loop']} - 1", 19 => ')');
            if ($smartyAttr['type']['step'] == 0) {
                $start_code[3] = $start_code[5] = $start_code[15] = $start_code[17] = '';
                if ($smartyAttr['value']['step'] > 0) {
                    $start_code[6] = $start_code[18] = '';
                } else {
                    $start_code[4] = $start_code[16] = '';
                }
            }
            if ($smartyAttr['type']['start'] == 0) {
                if ($smartyAttr['type']['loop'] == 0) {
                    $start_code[8] = $smartyAttr['value']['start'] + $smartyAttr['value']['loop'];
                }
                $smartyAttr['type']['start'] = $smartyAttr['type']['step'] + $smartyAttr['type']['loop'];
                $start_code[1] = '';
                if ($smartyAttr['value']['start'] < 0) {
                    for ($i = 11; $i <= 19; $i ++) {
                        $start_code[$i] = '';
                    }
                    if ($smartyAttr['type']['start'] == 0) {
                        $start_code = array(max($smartyAttr['value']['step'] >
                                                0 ? 0 : - 1, $smartyAttr['value']['start'] +
                                                $smartyAttr['value']['loop']));
                    }
                } else {
                    for ($i = 1; $i <= 11; $i ++) {
                        $start_code[$i] = '';
                    }
                    if ($smartyAttr['type']['start'] == 0) {
                        $start_code = array(min($smartyAttr['value']['step'] >
                                                0 ? $smartyAttr['value']['loop'] : $smartyAttr['value']['loop'] -
                            1, $smartyAttr['value']['start']));
                    }
                }
            }
            $smartyAttr['value']['start'] = join('', $start_code);
        }
        if ($smartyAttr['type']['start'] != 0) {
            $smartyAttr['initLocal']['start'] = $smartyAttr['value']['start'];
            $smartyAttr['value']['start'] = "{$local}start";
        }

        $smartyAttr['initFor']['index'] = "{$sectionVar}['index'] = {$smartyAttr['value']['start']}";

        if (!isset($_attr['start']) && !isset($_attr['step']) && !isset($_attr['max'])) {
            $smartyAttr['value']['total'] = $smartyAttr['value']['loop'];
            $smartyAttr['type']['total'] = $smartyAttr['type']['loop'];
        } else {
            $smartyAttr['type']['total'] = $smartyAttr['type']['start'] + $smartyAttr['type']['loop'] +
                $smartyAttr['type']['step'] + $smartyAttr['type']['max'];
            if ($smartyAttr['type']['total'] == 0) {
                $smartyAttr['value']['total'] = min(ceil(($smartyAttr['value']['step'] >
                                                         0 ? $smartyAttr['value']['loop'] -
                                                             $smartyAttr['value']['start'] : $smartyAttr['value']['start'] +
                                                             1) /
                                                         abs($smartyAttr['value']['step'])), $smartyAttr['value']['max']);
            } else {
                $total_code = array(1  => 'min(', 2 => 'ceil(', 3 => '(', 4 => "{$smartyAttr['value']['step']} > 0 ? ",
                                    5  => $smartyAttr['value']['loop'], 6 => ' - ', 7 => $smartyAttr['value']['start'],
                                    8  => ' : ', 9 => $smartyAttr['value']['start'], 10 => '+ 1', 11 => ')', 12 => '/ ',
                                    13 => 'abs(', 14 => $smartyAttr['value']['step'], 15 => ')', 16 => ')',
                                    17 => ", {$smartyAttr['value']['max']})",);
                if (!isset($smartyAttr['value']['max'])) {
                    $total_code[1] = $total_code[17] = '';
                }
                if ($smartyAttr['type']['loop'] + $smartyAttr['type']['start'] == 0) {
                    $total_code[5] = $smartyAttr['value']['loop'] - $smartyAttr['value']['start'];
                    $total_code[6] = $total_code[7] = '';
                }
                if ($smartyAttr['type']['start'] == 0) {
                    $total_code[9] = $smartyAttr['value']['start'] + 1;
                    $total_code[10] = '';
                }
                if ($smartyAttr['type']['step'] == 0) {
                    $total_code[13] = $total_code[15] = '';
                    if ($smartyAttr['value']['step'] == 1 || $smartyAttr['value']['step'] == - 1) {
                        $total_code[2] = $total_code[12] = $total_code[14] = $total_code[16] = '';
                    } elseif ($smartyAttr['value']['step'] < 0) {
                        $total_code[14] = - $smartyAttr['value']['step'];
                    }
                    $total_code[3] = $total_code[4] = $total_code[11] = '';
                    if ($smartyAttr['value']['step'] > 0) {
                        $total_code[8] = $total_code[9] = '';
                    } else {
                        $total_code[5] = $total_code[6] = $total_code[7] = $total_code[8] = '';
                    }
                }
                $smartyAttr['value']['total'] = join('', $total_code);
            }
        }

        if (isset($smartyAttr['isSmarty']['total'])) {
            $smartyAttr['initSmarty']['total'] = "'total' => {$smartyAttr['value']['total']}";
            if ($smartyAttr['type']['total'] > 0) {
                $smartyAttr['value']['total'] = "{$sectionVar}['total']";
            }
        } elseif ($smartyAttr['type']['total'] > 0) {
            $smartyAttr['initLocal']['total'] = $smartyAttr['value']['total'];
            $smartyAttr['value']['total'] = "{$local}total";
        }

        $smartyAttr['cmpFor']['iteration'] = "{$smartyAttr['value']['iteration']} <= {$smartyAttr['value']['total']}";

        foreach ($smartyAttr['initLocal'] as $key => $code) {
            $output .= "{$local}{$key} = {$code};\n";
        }

        $_vars = 'array(' . join(', ', $smartyAttr['initSmarty']) . ')';
        $output .= "\$_smarty_tpl->tpl_vars['__section_{$attributes['name']}'] = new Smarty_Variable({$_vars});\n";
        $cond_code = "{$smartyAttr['value']['total']} != 0";
        if ($smartyAttr['type']['total'] == 0) {
            if ($smartyAttr['value']['total'] == 0) {
                $cond_code = 'false';
            } else {
                $cond_code = 'true';
            }
        }
        if ($smartyAttr['type']['show'] > 0) {
            $output .= "{$local}show = {$smartyAttr['value']['show']} ? {$cond_code} : false;\n";
            $output .= "if ({$local}show) {\n";
        } elseif ($smartyAttr['value']['show'] == 'true') {
            $output .= "if ({$cond_code}) {\n";
        } else {
            $output .= "if (false) {\n";
        }
        $jinit = join(', ', $smartyAttr['initFor']);
        $jcmp = join(', ', $smartyAttr['cmpFor']);
        $jinc = join(', ', $smartyAttr['incFor']);
        $output .= "for ({$jinit}; {$jcmp}; {$jinc}){\n";
        if (isset($smartyAttr['rownum'])) {
            $output .= "{$sectionVar}['rownum'] = {$smartyAttr['value']['iteration']};\n";
        }
        if (isset($smartyAttr['index_prev'])) {
            $output .= "{$sectionVar}['index_prev'] = {$smartyAttr['value']['index']} - {$smartyAttr['value']['step']};\n";
        }
        if (isset($smartyAttr['index_next'])) {
            $output .= "{$sectionVar}['index_next'] = {$smartyAttr['value']['index']} + {$smartyAttr['value']['step']};\n";
        }
        if (isset($smartyAttr['first'])) {
            $output .= "{$sectionVar}['first'] = ({$smartyAttr['value']['iteration']} == 1);\n";
        }
        if (isset($smartyAttr['last'])) {
            $output .= "{$sectionVar}['last'] = ({$smartyAttr['value']['iteration']} == {$smartyAttr['value']['total']});\n";
        }
        $output .= "?>";

        return $output;
    }

    /**
     * Compiles code for the {$smarty.section} tag
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
            $compiler->trigger_template_error("missing or illegal \$Smarty.section name attribute", $compiler->lex->taglineno);
        }
        if ((!isset($parameter[2]) || false === $property = $compiler->getId($parameter[2])) ||
            !in_array(strtolower($property), array('first', 'last', 'index', 'iteration', 'show', 'total'))
        ) {
            $compiler->trigger_template_error("missing or illegal \$Smarty.section property attribute", $compiler->lex->taglineno);
        }
        $property = strtolower($property);
        $sectionVar = "'__section_{$name}'";
        return "(isset(\$_smarty_tpl->tpl_vars[{$sectionVar}]->value['{$property}']) ? \$_smarty_tpl->tpl_vars[{$sectionVar}]->value['{$property}'] : null)";
    }
}

/**
 * Smarty Internal Plugin Compile Sectionelse Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Sectionelse extends Smarty_Internal_CompileBase
{
    /**
     * Compiles code for the {sectionelse} tag
     *
     * @param  array  $args     array with attributes from parser
     * @param  object $compiler compiler object
     *
     * @return string compiled code
     */
    public function compile($args, $compiler)
    {
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);

        list($openTag, $nocache, $attributes) = $this->closeTag($compiler, array('section'));
        $this->openTag($compiler, 'sectionelse', array('sectionelse', $nocache, $attributes));

        return "<?php }} else {\n ?>";
    }
}

/**
 * Smarty Internal Plugin Compile Sectionclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Sectionclose extends Smarty_Internal_CompileBase
{
    /**
     * Compiles code for the {/section} tag
     *
     * @param  array  $args     array with attributes from parser
     * @param  object $compiler compiler object
     *
     * @return string compiled code
     */
    public function compile($args, $compiler)
    {
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);

        // must endblock be nocache?
        if ($compiler->nocache) {
            $compiler->tag_nocache = true;
        }

        list($openTag, $compiler->nocache, $attributes) = $this->closeTag($compiler, array('section', 'sectionelse'));

        $output = "<?php\n";
        if ($openTag == 'sectionelse') {
            $output .= "}\n";
        } else {
            $output .= "}\n}\n";
        }
        $output .= "if (\$section_{$attributes['no']}_s_name) {\n";
        $output .= "\$_smarty_tpl->tpl_vars['__section_{$attributes['name']}'] = \$section_{$attributes['no']}_s_name;\n";
        $output .= "}\n";
        $output .= "?>";

        return $output;
    }
}
