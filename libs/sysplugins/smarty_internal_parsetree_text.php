<?php

/**
 * Smarty Internal Plugin Templateparser Parsetrees
 * These are classes to build parsetrees in the template parser
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Thue Kristensen
 * @author     Uwe Tews
 *             *
 *             template text
 * @package    Smarty
 * @subpackage Compiler
 * @ignore
 */
class Smarty_Internal_ParseTree_Text extends Smarty_Internal_ParseTree
{
    /**
     * Create template text buffer
     *
     * @param object $parser parser object
     * @param string $data   text
     */
    public function __construct($parser, $data)
    {
        $this->parser = $parser;
        $this->data = $data;
    }

    /**
     * Return buffer content
     *
     * @return string text
     */
    public function to_smarty_php()
    {
        return $this->data;
    }
}
