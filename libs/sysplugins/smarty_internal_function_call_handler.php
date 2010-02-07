<?php
/**
* Smarty Internal Plugin Function Call Handler
* 
* @package Smarty
* @subpackage PluginsInternal
* @author Uwe Tews 
*/
/**
* This class does call function defined with the {function} tag
*/
class Smarty_Internal_Function_Call_Handler extends Smarty_Internal_Template {
    function __construct($name, $smarty, $parent, $nocache)
    {
        parent::__construct('string:', $smarty, $parent);
        if (!isset($this->smarty->template_functions[$name])) {
            throw new Exception("Call to undefined template function \"{$name}\" in template \"{$parent->template_resource}\"");
        } 
        $this->called_nocache = $nocache;
        $this->mustCompile = false;
        if ($nocache) {
            $smarty->template_functions[$name]['called_nocache'] = true;
            $this->properties['function'][$name]['called_nocache'] = true;
        } 
        $this->properties['nocache_hash'] = $smarty->template_functions[$name]['nocache_hash']; 
        // load compiled function
        if ($nocache) {
            // if called in nocache mode convert nocache code to real code
            $this->compiled_template = preg_replace(array("!(<\?php echo ')?/\*/?%%SmartyNocache:{$this->smarty->template_functions[$name]['nocache_hash']}%%\*/(';\?>)?!", "!\\\'!"), array('', "'"), $smarty->template_functions[$name]['compiled']);
        } else {
            $this->compiled_template = $smarty->template_functions[$name]['compiled'];
        } 
        // assign default paramter
        if (isset($smarty->template_functions[$name]['parameter'])) {
            $_smarty_tpl = $this;
            foreach ($smarty->template_functions[$name]['parameter'] as $_key => $_value) {
                $this->assign($_key, eval("return {$_value};"));
            } 
        } 
        // set flag if {function} contains nocache code
        if ($smarty->template_functions[$name]['has_nocache_code']) {
            $this->has_nocache_code = true;
        } 
    } 
} 

?>
