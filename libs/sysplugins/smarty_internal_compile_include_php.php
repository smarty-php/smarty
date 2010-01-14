<?php

/**
* Smarty Internal Plugin Compile Include PHP
* 
* Compiles the {include_php} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Insert Class
*/
class Smarty_Internal_Compile_Include_Php extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {include_php} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('file');
        $this->optional_attributes = array('once', 'assign'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        $_output = '<?php '; 

        $_smarty_tpl = $compiler->template; 
        eval('$_file = ' . $_attr['file'] . ';'); 
        
        $_file = realpath($_file);

        if ($this->compiler->smarty->security) {
            $this->compiler->smarty->security_handler->isTrustedPHPDir($_file);
        } 

        if ($_file === false) {
            $this->compiler->trigger_template_error('include_php: file "' . $_attr['file'] . '" is not readable');
        } 

        if ($this->compiler->smarty->security) {
            $this->compiler->smarty->security_handler->isTrustedPHPDir($_file);
        } 
        if (isset($_attr['assign'])) {
            // output will be stored in a smarty variable instead of being displayed
            $_assign = $_attr['assign'];
        } 
        $_once = '_once';
        if (isset($_attr['once'])) {
            if ($_attr['once'] == 'false') {
                $_once = '';
            } 
        } 

        if (isset($_assign)) {
            return "<?php ob_start(); include{$_once} ('{$_file}'); \$_smarty_tpl->assign({$_assign},ob_get_contents()); ob_end_clean();?>";
        } else {
            return "<?php include{$_once} ('{$_file}');?>\n";
        } 
    } 
} 

?>
