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
        if ($compiler->smarty->merge_compiled_includes) {
            // check if compiled code can be merged (contains no variable part)
            if (!$compiler->has_variable_string && (substr_count($include_file, '"') == 2 or substr_count($include_file, "'") == 2) and substr_count($include_file, '(') == 0) {
                eval("\$tmp = $include_file;");
				if ($this->compiler->template->template_resource != $tmp) {
                $tpl = $compiler->smarty->createTemplate ($tmp, $compiler->template->cache_id, $compiler->template->compile_id, $compiler->template);
                if ($tpl->usesCompiler() && $tpl->isExisting()) {
                    do {
                        $must_compile = false;
                        $prop = array();
                        $compiled_tpl = $tpl->getCompiledTemplate();
                        preg_match('/(\<\?php \$_smarty_tpl-\>decodeProperties\(\')(.*)(\'.*\?\>)/', $compiled_tpl, $matches);
                        $compiled_tpl = preg_replace(array('/(\<\?php \$_smarty_tpl-\>decodeProperties\(\')(.*)(\'.*\?\>.*\n)/', '/(\<\?php if\(\!defined\(\'SMARTY_DIR\'\)\))(.*)(\?\>.*\n)/'), '', $compiled_tpl); 
                        // var_dump($matches, $compiled_tpl);
                        if (isset($matches[2])) {
                            $prop = unserialize($matches[2]);
                            foreach ($prop['file_dependency'] as $_file_to_check) {
                                If (is_file($_file_to_check[0])) {
                                    $mtime = filemtime($_file_to_check[0]);
                                } else {
                                    $tpl->parseResourceName($_file_to_check[0], $resource_type, $resource_name, $resource_handler);
                                    $mtime = $resource_handler->getTemplateTimestampTypeName($resource_type, $resource_name);
                                } 
                                If ($mtime != $_file_to_check[1]) {
                                    $must_compile = true;
                                    break;
                                } 
                            } 
                            if ($must_compile) {
                                // recompile
                                $tpl->compileTemplateSource();
                            } 
                        } 
                    } while ($must_compile);
                    if (isset($prop['file_dependency'])) {
                        $compiler->template->properties['file_dependency'] = array_merge($compiler->template->properties['file_dependency'], $prop['file_dependency']);
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
        // default for included templates
        if ($compiler->template->caching) {
            $_caching = SMARTY_CACHING_LIFETIME_CURRENT;
        } else {
            $_caching = SMARTY_CACHING_OFF;
        } 
        /*
        * if the {include} tag provides individual parameter for caching
        * it will not be included into the common cache file and treated like
        * a nocache section
        */
        if (isset($_attr['cache_lifetime'])) {
            $_cache_lifetime = $_attr['cache_lifetime'];
            $this->compiler->tag_nocache = true;
        } 
        if (isset($_attr['nocache'])) {
            if ($_attr['nocache'] == 'true') {
                $this->compiler->tag_nocache = true;
            } 
        } 
        if (isset($_attr['caching'])) {
            if ($_attr['caching'] == 'true') {
                $_caching = SMARTY_CACHING_LIFETIME_CURRENT;
            } else {
                $_caching = SMARTY_CACHING_OFF;
            } 
        } 
        // create template object
        $_output = "<?php \$_template = new Smarty_Template ($include_file, \$_smarty_tpl->smarty, \$_smarty_tpl, \$_smarty_tpl->cache_id,  \$_smarty_tpl->compile_id);"; 
        // delete {include} standard attributes
        unset($_attr['file'], $_attr['assign'], $_attr['cache_lifetime'], $_attr['nocache'], $_attr['caching'], $_attr['scope']); 
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
        // add caching parameter if required
        if (isset($_cache_lifetime)) {
            $_output .= "\$_template->cache_lifetime = $_cache_lifetime;";
            $_caching = SMARTY_CACHING_LIFETIME_CURRENT;
        } 
        $_output .= "\$_template->caching = $_caching;"; 
        // was there an assign attribute
        if (isset($_assign)) {
            $_output .= "\$_smarty_tpl->assign($_assign,\$_template->fetch()); ?>";
        } else {
            if ($has_compiled_template) {
                $_output .= " \$_tpl_stack[] = \$_smarty_tpl; \$_smarty_tpl = \$_template;?>\n";
                $_output .= $compiled_tpl . "<?php /*  End of included template \"" . $tpl->getTemplateFilepath() . "\" */ ?>";
                $_output .= "<?php  \$_smarty_tpl = array_pop(\$_tpl_stack);?>";
            } else {
                $_output .= " echo \$_template->fetch(); ?>";
            } 
        } 
        if ($_parent_scope != SMARTY_LOCAL_SCOPE) {
            $_output .= "<?php \$_template->updateParentVariables($_parent_scope); ?>";
        } 
        $_output .= "<?php unset(\$_template); ?>";
        return $_output;
    } 
} 

?>
