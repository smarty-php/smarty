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
        $this->required_attributes = array('value', 'modifierlist'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);
        $output = $_attr['value']; 
        // loop over list of modifiers
        foreach ($_attr['modifierlist'] as $single_modifier) {
            preg_match_all('/(\'(.+?)\'|\"(.+?)\"|:|[^:]+)/', $single_modifier, $mod_array);
            $modifier = $mod_array[0][0];
            $i = 1;
            while (isset($mod_array[0][$i])) {
                unset($mod_array[0][$i]);
                $i = $i + 2;
            } 
            $mod_array[0][0] = $output;
            $params = implode(",", $mod_array[0]); 
            // check for registered modifier
            if (isset($compiler->smarty->registered_plugins['modifier'][$modifier])) {
                $function = $compiler->smarty->registered_plugins['modifier'][$modifier][0];
                if (!is_array($function)) {
                    $output = "{$function}({$params})";
                } else {
                    if (is_object($function[0])) {
                        $output = '$_smarty_tpl->smarty->registered_plugins[\'modifier\'][\'' . $modifier . '\'][0][0]->' . $function[1] . '(' . $params . ')';
                    } else {
                        $output = $function[0] . '::' . $function[1] . '(' . $params . ')';
                    } 
                } 
                // check for plugin modifier
            } else if ($function = $this->compiler->getPlugin($modifier, 'modifier')) {
                $output = "{$function}({$params})"; 
                // check if trusted PHP function
            } else if (is_callable($modifier)) {
                // check if modifier allowed
                if (!$this->compiler->template->security || $this->smarty->security_handler->isTrustedModifier($modifier, $this->compiler)) {
                    $output = "{$modifier}({$params})";
                } 
            } else {
                $this->compiler->trigger_template_error ("unknown modifier \"" . $modifier . "\"");
            } 
        } 
        return $output;
    } 
} 

?>