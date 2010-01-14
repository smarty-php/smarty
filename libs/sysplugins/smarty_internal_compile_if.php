<?php
/**
* Smarty Internal Plugin Compile If
* 
* Compiles the {if} {else} {elseif} {/if} tags
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile If Class
*/
class Smarty_Internal_Compile_If extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {if} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('if condition'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);
        $this->_open_tag('if',array(1,$compiler->tag_nocache));
        if (is_array($args['if condition'])) {
            $_output = "<?php if (!isset(\$_smarty_tpl->tpl_vars[".$args['if condition']['var']."])) \$_smarty_tpl->tpl_vars[".$args['if condition']['var']."] = new Smarty_Variable;";
            $_output .= "if (\$_smarty_tpl->tpl_vars[".$args['if condition']['var']."]->value = ".$args['if condition']['value']."){?>";
            return $_output;
        } else {
            return "<?php if ({$args['if condition']}){?>";
        } 
    } 
} 

/**
* Smarty Internal Plugin Compile Else Class
*/
class Smarty_Internal_Compile_Else extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {else} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler; 
        list($nesting, $compiler->tag_nocache) = $this->_close_tag(array('if', 'elseif'));
        $this->_open_tag('else',array($nesting,$compiler->tag_nocache));

        return "<?php }else{ ?>";
    } 
} 

/**
* Smarty Internal Plugin Compile ElseIf Class
*/
class Smarty_Internal_Compile_Elseif extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {elseif} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('if condition'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);

        list($nesting, $compiler->tag_nocache) = $this->_close_tag(array('if', 'elseif'));

        if (empty($this->compiler->prefix_code)) {
            $this->_open_tag('elseif', array($nesting, $compiler->tag_nocache));
            return "<?php }elseif({$args['if condition']}){?>";
        } else {
            $tmp = '';
            foreach ($this->compiler->prefix_code as $code) $tmp .= $code;
            $this->compiler->prefix_code = array();
            $this->_open_tag('elseif', array($nesting + 1, $compiler->tag_nocache));
            return "<?php }else{?>{$tmp}<?php if ({$args['if condition']}){?>";
        } 
    } 
} 

/**
* Smarty Internal Plugin Compile Ifclose Class
*/
class Smarty_Internal_Compile_Ifclose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/if} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler; 
        list($nesting, $compiler->tag_nocache) = $this->_close_tag(array('if', 'else', 'elseif'));
        $tmp = '';
        for ($i = 0; $i < $nesting ; $i++) $tmp .= '}';
        return "<?php {$tmp}?>";
    } 
} 

?>
