<?php

namespace Smarty\ParseTree;

/**
 * Smarty Internal Plugin Templateparser ParseTree
 * These are classes to build parsetree in the template parser
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Thue Kristensen
 * @author     Uwe Tews
 */

/**
 * @package    Smarty
 * @subpackage Compiler
 * @ignore
 */
abstract class Base
{
    /**
     * Buffer content
     *
     * @var mixed
     */
    public $data;

    /**
     * Subtree array
     *
     * @var array
     */
    public $subtrees = array();

    /**
     * Return buffer
     *
     * @param \Smarty\Parser\TemplateParser $parser
     *
     * @return string buffer content
     */
    abstract public function to_smarty_php(\Smarty\Parser\TemplateParser $parser);

    /**
     * Template data object destructor
     */
    public function __destruct()
    {
        $this->data = null;
        $this->subtrees = null;
    }
}
