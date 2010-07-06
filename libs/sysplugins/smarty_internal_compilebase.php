<?php

/**
* Smarty Internal Plugin CompileBase
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/

/**
* This class does extend all internal compile plugins
*/
//abstract class Smarty_Internal_CompileBase implements TagCompilerInterface 
abstract class Smarty_Internal_CompileBase 
{
    function __construct()
    {
        // initialize valid attributes
        $this->required_attributes = array();
        $this->optional_attributes = array();
    } 

    /**
    * This function checks if the attributes passed are valid
    * 
    * The attributes passed for the tag to compile are checked against the list of required and 
    * optional attributes. Required attributes must be present. Optional attributes are check against
    * against the corresponding list. The keyword '_any' specifies that any attribute will be accepted 
    * as valid
    * 
    * @todo More generallized handling of the nocache attributes in compile plugins
    * @param array $args attributes applied to the tag
    * @return array attributes for further processing
    */
    function _get_attributes ($args)
    { 
        // check if all required attributes present
        foreach ($this->required_attributes as $attr) {
            if (!array_key_exists($attr, $args)) {
                $this->compiler->trigger_template_error("missing \"" . $attr . "\" attribute");
            } 
        } 
        // check for unallowed attributes
        if ($this->optional_attributes != array('_any')) {
            $tmp_array = array_merge($this->required_attributes, $this->optional_attributes);
            foreach ($args as $key => $dummy) {
                 if (!in_array($key, $tmp_array) && $key !== 0) {
                   $this->compiler->trigger_template_error("unexpected \"" . $key . "\" attribute");
                } 
            } 
        } 

        return $args;
    } 

    /**
    * Push opening tag name on stack
    * 
    * Optionally additional data can be saved on stack
    * 
    * @param string $open_tag the opening tag's name
    * @param anytype $data optional data which shall be saved on stack
    */
    function _open_tag($open_tag, $data = null)
    {
        array_push($this->compiler->_tag_stack, array($open_tag, $data));
    } 

    /**
    * Pop closing tag
    * 
    * Raise an error if this stack-top doesn't match with expected opening tags
    * 
    * @param array $ |string $expected_tag the expected opening tag names
    * @return anytype the opening tag's name or saved data
    */
    function _close_tag($expected_tag)
    {
        if (count($this->compiler->_tag_stack) > 0) {
            // get stacked info
            list($_open_tag, $_data) = array_pop($this->compiler->_tag_stack); 
            // open tag must match with the expected ones
            if (in_array($_open_tag, (array)$expected_tag)) {
                if (is_null($_data)) {
                    // return opening tag
                    return $_open_tag;
                } else {
                    // return restored data
                    return $_data;
                } 
            } 
            // wrong nesting of tags
            $this->compiler->trigger_template_error("unclosed {" . $_open_tag . "} tag");
            return;
        } 
        // wrong nesting of tags
        $this->compiler->trigger_template_error("unexpected closing tag",$this->compiler->lex->taglineno);
        return;
    } 
} 

?>