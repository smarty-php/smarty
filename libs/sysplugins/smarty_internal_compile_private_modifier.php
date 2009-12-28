<?php
/**
* Smarty Internal Plugin Compile Modifier
* 
* Compiles code for modifier execution
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Modifier Class
*/
class Smarty_Internal_Compile_Private_Modifier extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for modifier execution
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->smarty = $this->compiler->smarty;
        $this->required_attributes = array('modifier', 'params'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args); 
        // check for registered modifier
        if (isset($compiler->smarty->registered_plugins['modifier'][$_attr['modifier']])) {
            $function = $compiler->smarty->registered_plugins['modifier'][$_attr['modifier']][0];
            if (!is_array($function)) {
                $output = "{$function}({$_attr['params']})";
            } else if (is_object($function[0])) {
                $output = 'call_user_func_array($_smarty_tpl->smarty->registered_plugins[\'modifier\'][\'' . $_attr['modifier'] . '\'][0],array(' . $_attr['params'] . '))';
            } else {
                $output = 'call_user_func_array(array(\'' . $function[0] . '\',\'' . $function[1] . '\'),array(' . $_attr['params'] . '))';
            } 
        // check for plugin modifier
        } else if ($function = $this->compiler->getPlugin($_attr['modifier'], 'modifier')) {
            if (!is_array($function)) {
                $output = "{$function}({$_attr['params']})";
            } else {
                $output = 'call_user_func_array(array(\'' . $function[0] . '\',\'' . $function[1] . '\'),array(' . $_attr['params'] . '))';
            } 
            // check if trusted PHP function
        } else if (is_callable($_attr['modifier'])) {
            // check if modifier allowed
            if (!$this->compiler->template->security || $this->smarty->security_handler->isTrustedModifier($_attr['modifier'], $this->compiler)) {
                $output = "{$_attr['modifier']}({$_attr['params']})";
            } 
        } else {
            $this->compiler->trigger_template_error ("unknown modifier \"" . $_attr['modifier'] . "\"");
        } 
        return $output;
    } 
} 

?>
