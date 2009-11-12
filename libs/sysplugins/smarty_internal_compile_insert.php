<?php

/**
* Smarty Internal Plugin Compile Insert
* 
* Compiles the {insert} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Insert Class
*/
class Smarty_Internal_Compile_Insert extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {insert} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('name');
        $this->optional_attributes = array('_any'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args); 
        // this tag must not be cached
        $this->compiler->tag_nocache = true;

        $_output = '<?php '; 
        // save posible attributes
        $_name = 'insert_' . trim($_attr['name'], "'\"");
        if (isset($_attr['assign'])) {
            // output will be stored in a smarty variable instead of beind displayed
            $_assign = $_attr['assign']; 
            // create variable to make shure that the compiler knows about its nocache status
            $this->compiler->template->tpl_vars[trim($_attr['assign'], "'")] = new Smarty_Variable(null, true);
        } 
        if (isset($_attr['script'])) {
            // script which must be included
            $_smarty_tpl = $compiler->template;
            eval('$_script = ' . $_attr['script'] . ';');
            if (!file_exists($_script)) {
                $this->compiler->trigger_template_error('missing file "' . $_script . '"');
            } 
            // code for script file loading
            $_output .= 'require once ' . $_script . ';';
        } else {
            if (!is_callable($_name)) {
                $this->compiler->trigger_template_error('function "' . $_name . '" is not callable');
            } 
        } 
        // delete {insert} standard attributes
        unset($_attr['name'], $_attr['assign'], $_attr['script']); 
        // convert attributes into parameter array string
        $_paramsArray = array();
        foreach ($_attr as $_key => $_value) {
            $_paramsArray[] = "'$_key'=>$_value";
        } 
        $_params = 'array(' . implode(",", $_paramsArray) . ')'; 
        // call insert
        if (isset($_assign)) {
            $_output .= '$_smarty_tpl->assign(' . $_assign . ',' . $_name . '(' . $_params . '),true); ?>';
        } else {
            $this->compiler->has_output = true;
            $_output .= 'echo ' . $_name . '(' . $_params . '); ?>';
        } 
        return $_output;
    } 
} 

?>
