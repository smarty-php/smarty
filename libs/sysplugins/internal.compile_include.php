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
                $_caching = SMARTY_cache_lifetime_CURRENT;
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
        } 
        $_output .= "\$_template->caching = $_caching;"; 
        // was there an assign attribute
        if (isset($_assign)) {
            $_output .= "\$_smarty_tpl->assign($_assign,\$_smarty_tpl->smarty->fetch(\$_template)); ?>";
        } else {
            $_output .= "\$_template->processInclude(); ?>";
        } 
        if ($_parent_scope != SMARTY_LOCAL_SCOPE) {
            $_output .= "<?php \$_template->updateParentVariables($_parent_scope); ?>";
        } 
        return $_output;
    } 
} 

?>
