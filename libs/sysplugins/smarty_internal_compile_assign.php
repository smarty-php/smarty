<?php
/**
 * Smarty Internal Plugin Compile Assign
 * Compiles the {assign} tag
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

/**
 * Smarty Internal Plugin Compile Assign Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Smarty_Internal_Compile_Assign extends Smarty_Internal_CompileBase
{
    /**
     * Valid scope names
     *
     * @var array
     */
    public $valid_scopes = array('local'    => true, 'parent' => true, 'root' => true, 'global' => true,
                                 'tpl_root' => true);

    /**
     * Compiles code for the {assign} tag
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
        // the following must be assigned at runtime because it will be overwritten in Smarty_Internal_Compile_Append
        $this->required_attributes = array('var', 'value');
        $this->shorttag_order = array('var', 'value');
        $this->optional_attributes = array('scope', 'bubble_up');
        $_nocache = 'null';
        // check and get attributes
        $_attr = $this->getAttributes($compiler, $args);
        // nocache ?
        if ($compiler->tag_nocache || $compiler->nocache) {
            $_nocache = 'true';
            // create nocache var to make it know for further compiling
            if (isset($compiler->template->tpl_vars[ trim($_attr[ 'var' ], "'") ])) {
                $compiler->template->tpl_vars[ trim($_attr[ 'var' ], "'") ]->nocache = true;
            } else {
                $compiler->template->tpl_vars[ trim($_attr[ 'var' ], "'") ] = new Smarty_Variable(null, true);
            }
        }
        // scope setup
        $_scope = Smarty::SCOPE_LOCAL;
        if (isset($_attr[ 'scope' ])) {
            $_attr[ 'scope' ] = trim($_attr[ 'scope' ], "'\"");
            if (!isset($this->valid_scopes[ $_attr[ 'scope' ] ])) {
                $compiler->trigger_template_error("illegal value '{$_attr['scope']}' for \"scope\" attribute", null,
                                                  true);
            }
            if ($_attr[ 'scope' ] != 'local') {
                if ($_attr[ 'scope' ] == 'parent') {
                    $_scope = Smarty::SCOPE_PARENT;
                } elseif ($_attr[ 'scope' ] == 'root') {
                    $_scope = Smarty::SCOPE_ROOT;
                } elseif ($_attr[ 'scope' ] == 'global') {
                    $_scope = Smarty::SCOPE_GLOBAL;
                } elseif ($_attr[ 'scope' ] == 'tpl_root') {
                    $_scope = Smarty::SCOPE_TPL_ROOT;
                }
                $_scope += (isset($_attr[ 'bubble_up' ]) && $_attr[ 'bubble_up' ] == 'false') ? 0 :
                    Smarty::SCOPE_BUBBLE_UP;
            }
        }
        if (isset($parameter[ 'smarty_internal_index' ])) {
            $output = "<?php \$_smarty_tpl->_createLocalArrayVariable({$_attr['var']}, {$_nocache});\n";
            $output .= "\$_smarty_tpl->tpl_vars[{$_attr['var']}]->value{$parameter['smarty_internal_index']} = {$_attr['value']};\n";
            if ($_scope != Smarty::SCOPE_LOCAL) {
                $output .= "\$_smarty_tpl->ext->_updateScope->updateScope(\$_smarty_tpl, {$_attr['var']}, {$_scope});\n";
            } else {
                $output .= "if (\$_smarty_tpl->scope & Smarty::SCOPE_BUBBLE_UP) {\n";
                $output .= "\$_smarty_tpl->ext->_updateScope->updateScope(\$_smarty_tpl, {$_attr['var']});\n}\n";
            }
            $output .= '?>';
        } else {
            if ($compiler->template->smarty instanceof SmartyBC) {
                $_smartyBC = 'true';
            } else {
                $_smartyBC = 'false';
            }
            $output =
                "<?php \$_smarty_tpl->_assignInScope({$_attr['var']}, {$_attr['value']}, {$_nocache}, {$_scope}, {$_smartyBC});\n?>";
        }
        return $output;
    }
}
