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
class Smarty_Internal_Compile_Section extends Smarty_Internal_Compile_Private_ForeachSection
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
     * Name of this tag
     *
     * @var string
     */
    public $tagName = 'section';

    /**
     * Valid properties of $smarty.section.name.xxx variable
     *
     * @var array
     */
    public static $nameProperties = array('first', 'last', 'index', 'iteration', 'show', 'total', 'rownum',
                                          'index_prev', 'index_next');

    /**
     * {section} tag has no item properties
     *
     * @var array
     */
    public $itemProperties = null;

    /**
     * {section} tag has always name attribute
     *
     * @var bool
     */
    public $isNamed = true;

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
        $initLocal = array('saved' => "isset(\$_smarty_tpl->tpl_vars['__section_{$attributes['name']}']) ? \$_smarty_tpl->tpl_vars['__section_{$attributes['name']}'] : false",);
        $initNamedProperty = array();
        $initFor = array();
        $incFor = array();
        $cmpFor = array();
        $propValue = array('index'     => "{$sectionVar}['index']", 'show' => 'true', 'step' => 1,
                           'iteration' => "{$local}iteration",

        );
        $propType = array('index' => 2, 'iteration' => 2, 'show' => 0, 'step' => 0,);
        // search for used tag attributes
        $this->scanForProperties($attributes, $compiler);
        if (!empty($this->matchResults['named'])) {
            $namedAttr = $this->matchResults['named'];
        }
        $namedAttr['index'] = true;
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
                    if (isset($namedAttr['loop'])) {
                        $initNamedProperty['loop'] = "'loop' => {$v}";
                        if ($t == 1) {
                            $v = "{$sectionVar}['loop']";
                        }
                    } elseif ($t == 1) {
                        $initLocal['loop'] = $v;
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
                    $initLocal['step'] = "((int)@$attr_value) == 0 ? 1 : (int)@$attr_value";
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
            $propValue[$attr_name] = $v;
            $propType[$attr_name] = $t;
        }

        if (isset($namedAttr['step'])) {
            $initNamedProperty['step'] = $propValue['step'];
        }
        if (isset($namedAttr['iteration'])) {
            $propValue['iteration'] = "{$sectionVar}['iteration']";
        }
        $incFor['iteration'] = "{$propValue['iteration']}++";
        $initFor['iteration'] = "{$propValue['iteration']} = 1";

        if ($propType['step'] == 0) {
            if ($propValue['step'] == 1) {
                $incFor['index'] = "{$sectionVar}['index']++";
            } elseif ($propValue['step'] > 1) {
                $incFor['index'] = "{$sectionVar}['index'] += {$propValue['step']}";
            } else {
                $incFor['index'] = "{$sectionVar}['index'] -= " . - $propValue['step'];
            }
        } else {
            $incFor['index'] = "{$sectionVar}['index'] += {$propValue['step']}";
        }

        if (!isset($propValue['max'])) {
            $propValue['max'] = $propValue['loop'];
            $propType['max'] = $propType['loop'];
        } elseif ($propType['max'] != 0) {
            $propValue['max'] = "{$propValue['max']} < 0 ? {$propValue['loop']} : {$propValue['max']}";
            $propType['max'] = 1;
        } else {
            if ($propValue['max'] < 0) {
                $propValue['max'] = $propValue['loop'];
                $propType['max'] = $propType['loop'];
            }
        }

        if (!isset($propValue['start'])) {
            $start_code = array(1 => "{$propValue['step']} > 0 ? ", 2 => '0', 3 => ' : ', 4 => $propValue['loop'],
                                5 => ' - 1');
            if ($propType['loop'] == 0) {
                $start_code[5] = '';
                $start_code[4] = $propValue['loop'] - 1;
            }
            if ($propType['step'] == 0) {
                if ($propValue['step'] > 0) {
                    $start_code = array(1 => '0');
                    $propType['start'] = 0;
                } else {
                    $start_code[1] = $start_code[2] = $start_code[3] = '';
                    $propType['start'] = $propType['loop'];
                }
            } else {
                $propType['start'] = 1;
            }
            $propValue['start'] = join('', $start_code);
        } else {
            $start_code = array(1  => "{$propValue['start']} < 0 ? ", 2 => 'max(', 3 => "{$propValue['step']} > 0 ? ",
                                4  => '0', 5 => ' : ', 6 => '-1', 7 => ', ',
                                8  => "{$propValue['start']} + {$propValue['loop']}", 10 => ')', 11 => ' : ',
                                12 => 'min(', 13 => $propValue['start'], 14 => ', ',
                                15 => "{$propValue['step']} > 0 ? ", 16 => $propValue['loop'], 17 => ' : ',
                                18 => $propType['loop'] == 0 ? $propValue['loop'] - 1 : "{$propValue['loop']} - 1",
                                19 => ')');
            if ($propType['step'] == 0) {
                $start_code[3] = $start_code[5] = $start_code[15] = $start_code[17] = '';
                if ($propValue['step'] > 0) {
                    $start_code[6] = $start_code[18] = '';
                } else {
                    $start_code[4] = $start_code[16] = '';
                }
            }
            if ($propType['start'] == 0) {
                if ($propType['loop'] == 0) {
                    $start_code[8] = $propValue['start'] + $propValue['loop'];
                }
                $propType['start'] = $propType['step'] + $propType['loop'];
                $start_code[1] = '';
                if ($propValue['start'] < 0) {
                    for ($i = 11; $i <= 19; $i ++) {
                        $start_code[$i] = '';
                    }
                    if ($propType['start'] == 0) {
                        $start_code = array(max($propValue['step'] > 0 ? 0 : - 1, $propValue['start'] +
                                                                                $propValue['loop']));
                    }
                } else {
                    for ($i = 1; $i <= 11; $i ++) {
                        $start_code[$i] = '';
                    }
                    if ($propType['start'] == 0) {
                        $start_code = array(min($propValue['step'] > 0 ? $propValue['loop'] : $propValue['loop'] -
                            1, $propValue['start']));
                    }
                }
            }
            $propValue['start'] = join('', $start_code);
        }
        if ($propType['start'] != 0) {
            $initLocal['start'] = $propValue['start'];
            $propValue['start'] = "{$local}start";
        }

        $initFor['index'] = "{$sectionVar}['index'] = {$propValue['start']}";

        if (!isset($_attr['start']) && !isset($_attr['step']) && !isset($_attr['max'])) {
            $propValue['total'] = $propValue['loop'];
            $propType['total'] = $propType['loop'];
        } else {
            $propType['total'] = $propType['start'] + $propType['loop'] + $propType['step'] + $propType['max'];
            if ($propType['total'] == 0) {
                $propValue['total'] = min(ceil(($propValue['step'] > 0 ? $propValue['loop'] -
                                                   $propValue['start'] : $propValue['start'] + 1) /
                                               abs($propValue['step'])), $propValue['max']);
            } else {
                $total_code = array(1  => 'min(', 2 => 'ceil(', 3 => '(', 4 => "{$propValue['step']} > 0 ? ",
                                    5  => $propValue['loop'], 6 => ' - ', 7 => $propValue['start'], 8 => ' : ',
                                    9  => $propValue['start'], 10 => '+ 1', 11 => ')', 12 => '/ ', 13 => 'abs(',
                                    14 => $propValue['step'], 15 => ')', 16 => ')', 17 => ", {$propValue['max']})",);
                if (!isset($propValue['max'])) {
                    $total_code[1] = $total_code[17] = '';
                }
                if ($propType['loop'] + $propType['start'] == 0) {
                    $total_code[5] = $propValue['loop'] - $propValue['start'];
                    $total_code[6] = $total_code[7] = '';
                }
                if ($propType['start'] == 0) {
                    $total_code[9] = $propValue['start'] + 1;
                    $total_code[10] = '';
                }
                if ($propType['step'] == 0) {
                    $total_code[13] = $total_code[15] = '';
                    if ($propValue['step'] == 1 || $propValue['step'] == - 1) {
                        $total_code[2] = $total_code[12] = $total_code[14] = $total_code[16] = '';
                    } elseif ($propValue['step'] < 0) {
                        $total_code[14] = - $propValue['step'];
                    }
                    $total_code[4] = '';
                    if ($propValue['step'] > 0) {
                        $total_code[8] = $total_code[9] = $total_code[10] = '';
                    } else {
                        $total_code[5] = $total_code[6] = $total_code[7] = $total_code[8] = '';
                    }
                }
                $propValue['total'] = join('', $total_code);
            }
        }

        if (isset($namedAttr['total'])) {
            $initNamedProperty['total'] = "'total' => {$propValue['total']}";
            if ($propType['total'] > 0) {
                $propValue['total'] = "{$sectionVar}['total']";
            }
        } elseif ($propType['total'] > 0) {
            $initLocal['total'] = $propValue['total'];
            $propValue['total'] = "{$local}total";
        }

        $cmpFor['iteration'] = "{$propValue['iteration']} <= {$propValue['total']}";

        foreach ($initLocal as $key => $code) {
            $output .= "{$local}{$key} = {$code};\n";
        }

        $_vars = 'array(' . join(', ', $initNamedProperty) . ')';
        $output .= "\$_smarty_tpl->tpl_vars['__section_{$attributes['name']}'] = new Smarty_Variable({$_vars});\n";
        $cond_code = "{$propValue['total']} != 0";
        if ($propType['total'] == 0) {
            if ($propValue['total'] == 0) {
                $cond_code = 'false';
            } else {
                $cond_code = 'true';
            }
        }
        if ($propType['show'] > 0) {
            $output .= "{$local}show = {$propValue['show']} ? {$cond_code} : false;\n";
            $output .= "if ({$local}show) {\n";
        } elseif ($propValue['show'] == 'true') {
            $output .= "if ({$cond_code}) {\n";
        } else {
            $output .= "if (false) {\n";
        }
        $jinit = join(', ', $initFor);
        $jcmp = join(', ', $cmpFor);
        $jinc = join(', ', $incFor);
        $output .= "for ({$jinit}; {$jcmp}; {$jinc}){\n";
        if (isset($namedAttr['rownum'])) {
            $output .= "{$sectionVar}['rownum'] = {$propValue['iteration']};\n";
        }
        if (isset($namedAttr['index_prev'])) {
            $output .= "{$sectionVar}['index_prev'] = {$propValue['index']} - {$propValue['step']};\n";
        }
        if (isset($namedAttr['index_next'])) {
            $output .= "{$sectionVar}['index_next'] = {$propValue['index']} + {$propValue['step']};\n";
        }
        if (isset($namedAttr['first'])) {
            $output .= "{$sectionVar}['first'] = ({$propValue['iteration']} == 1);\n";
        }
        if (isset($namedAttr['last'])) {
            $output .= "{$sectionVar}['last'] = ({$propValue['iteration']} == {$propValue['total']});\n";
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
        $output .= "if (\$section_{$attributes['no']}_saved) {\n";
        $output .= "\$_smarty_tpl->tpl_vars['__section_{$attributes['name']}'] = \$section_{$attributes['no']}_saved;\n";
        $output .= "}\n";
        $output .= "?>";

        return $output;
    }
}
