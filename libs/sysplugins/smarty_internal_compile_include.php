<?php

/**
 * Smarty Internal Plugin Compile Include
 * 
 * Compiles the {include} tag
 * 
 * @package Smarty
 * @subpackage Compiler
 * @author Uwe Tews 
 */
/**
 * Smarty Internal Plugin Compile Include Class
 */
class Smarty_Internal_Compile_Include extends Smarty_Internal_CompileBase {
    /**
     * Compiles code for the {include} tag
     * 
     * @param array $args array with attributes from parser
     * @param object $compiler compiler object
     * @return string compiled code
     */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('file');
        $this->optional_attributes = array('_any'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args); 
        // save posible attributes
        $include_file = $_attr['file'];
        $has_compiled_template = false;
        if ($compiler->smarty->merge_compiled_includes || isset($_attr['inline'])) {
            // check if compiled code can be merged (contains no variable part)
            if (!$compiler->has_variable_string && (substr_count($include_file, '"') == 2 or substr_count($include_file, "'") == 2) and substr_count($include_file, '(') == 0) {
                eval("\$tmp = $include_file;");
                if ($this->compiler->template->template_resource != $tmp) {
                    $tpl = $compiler->smarty->createTemplate ($tmp, $compiler->template->cache_id, $compiler->template->compile_id, $compiler->template);
                    if ($this->compiler->template->caching) {
                        // needs code for cached page but no cache file
                        $tpl->caching = 9999;
                    } 
                   if ($this->compiler->template->mustCompile) {
                        // make sure whole chain gest compiled
                        $tpl->mustCompile = true;
                    } 
                    if ($tpl->resource_object->usesCompiler && $tpl->isExisting()) {
                        // get compiled code
                        $compiled_tpl = $tpl->getCompiledTemplate();
                        // merge compiled code for {function} tags
                        $compiler->template->properties['function'] = array_merge($compiler->template->properties['function'], $tpl->properties['function']); 
                        // merge filedependency by evaluating header code
                        preg_match_all("/(<\?php \/\*%%SmartyHeaderCode:{$tpl->properties['nocache_hash']}%%\*\/(.+?)\/\*\/%%SmartyHeaderCode%%\*\/\?>\n)/s", $compiled_tpl, $result);
                        $saved_has_nocache_code = $compiler->template->has_nocache_code;
                        $saved_nocache_hash = $compiler->template->properties['nocache_hash'];
                        $_smarty_tpl = $compiler->template;
                        eval($result[2][0]);
                        $compiler->template->properties['nocache_hash'] = $saved_nocache_hash;
                        $compiler->template->has_nocache_code = $saved_has_nocache_code; 
                        // remove header code
                        $compiled_tpl = preg_replace("/(<\?php \/\*%%SmartyHeaderCode:{$tpl->properties['nocache_hash']}%%\*\/(.+?)\/\*\/%%SmartyHeaderCode%%\*\/\?>\n)/s", '', $compiled_tpl);
                        if ($tpl->has_nocache_code) {
                            // replace nocache_hash
                            $compiled_tpl = preg_replace("/{$tpl->properties['nocache_hash']}/", $compiler->template->properties['nocache_hash'], $compiled_tpl);
                            $compiler->template->has_nocache_code = true;
                        } 
                        $has_compiled_template = true;
                    } 
                } 
            } 
        } 

        if (isset($_attr['assign'])) {
            // output will be stored in a smarty variable instead of beind displayed
            $_assign = $_attr['assign'];
        } 

        $_parent_scope = SMARTY_LOCAL_SCOPE;
        if (isset($_attr['scope'])) {
            if ($_attr['scope'] == '\'parent\'') {
                $_parent_scope = SMARTY_PARENT_SCOPE;
            } elseif ($_attr['scope'] == '\'root\'') {
                $_parent_scope = SMARTY_ROOT_SCOPE;
            } elseif ($_attr['scope'] == '\'global\'') {
                $_parent_scope = SMARTY_GLOBAL_SCOPE;
            } 
        } 
        $_caching = 'null'; 
        // default for included templates
        if ($this->compiler->template->caching && !$this->compiler->nocache) {
            $_caching = 9999;
        } 
        /*
        * if the {include} tag provides individual parameter for caching
        * it will not be included into the common cache file and treated like
        * a nocache section
        */
        if (isset($_attr['cache_lifetime'])) {
            $_cache_lifetime = $_attr['cache_lifetime'];
            $this->compiler->tag_nocache = true;
            $_caching = SMARTY_CACHING_LIFETIME_CURRENT;
        } else {
            $_cache_lifetime = 'null';
        } 
        if (isset($_attr['nocache'])) {
            if ($_attr['nocache'] == 'true') {
                $this->compiler->tag_nocache = true;
                $_caching = SMARTY_CACHING_OFF;
            } 
        } 
        if (isset($_attr['caching'])) {
            if ($_attr['caching'] == 'true') {
                $_caching = SMARTY_CACHING_LIFETIME_CURRENT;
            } else {
                $this->compiler->tag_nocache = true;
                $_caching = SMARTY_CACHING_OFF;
            } 
        } 
        // create template object
        $_output = "<?php \$_template = new {$compiler->smarty->template_class}($include_file, \$_smarty_tpl->smarty, \$_smarty_tpl, \$_smarty_tpl->cache_id, \$_smarty_tpl->compile_id, $_caching, $_cache_lifetime);\n"; 
        // delete {include} standard attributes
        unset($_attr['file'], $_attr['assign'], $_attr['cache_lifetime'], $_attr['nocache'], $_attr['caching'], $_attr['scope'], $_attr['inline']); 
        // remaining attributes must be assigned as smarty variable
        if (!empty($_attr)) {
            if ($_parent_scope == SMARTY_LOCAL_SCOPE) {
                // create variables
                foreach ($_attr as $_key => $_value) {
                    $_output .= "\$_template->assign('$_key',$_value);";
                } 
            } else {
                $this->compiler->trigger_template_error('variable passing not allowed in parent/global scope');
            } 
        } 
        // was there an assign attribute
        if (isset($_assign)) {
            $_output .= "\$_smarty_tpl->assign($_assign,\$_template->getRenderedTemplate());?>";
        } else {
            if ($has_compiled_template && !($compiler->template->caching && ($this->compiler->tag_nocache || $this->compiler->nocache))) {
                $_output .= "\$_template->properties['nocache_hash']  = '{$compiler->template->properties['nocache_hash']}';\n";
                $_output .= "\$_tpl_stack[] = \$_smarty_tpl; \$_smarty_tpl = \$_template;?>\n";
                $_output .= $compiled_tpl;
                $_output .= "<?php \$_smarty_tpl->updateParentVariables($_parent_scope);?>\n";
                $_output .= "<?php /*  End of included template \"" . $tpl->getTemplateFilepath() . "\" */ ?>\n";
                $_output .= "<?php \$_smarty_tpl = array_pop(\$_tpl_stack);?>";
            } else {
                $_output .= " echo \$_template->getRenderedTemplate();?>";
                $_output .= "<?php \$_template->updateParentVariables($_parent_scope);?>";
            } 
        } 
        $_output .= "<?php unset(\$_template);?>";
        return $_output;
    } 
} 

?>
