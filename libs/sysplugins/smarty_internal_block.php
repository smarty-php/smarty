<?php

/**
 * Smarty {block} tag class
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Block
{
    /**
     * Block name
     *
     * @var string
     */
    public $name = '';

    /**
     * Hide attribute
     *
     * @var bool
     */
    public $hide = false;

    /**
     * Append attribute
     *
     * @var bool
     */
    public $append = false;

    /**
     * prepend attribute
     *
     * @var bool
     */
    public $prepend = false;

    /**
     * Block calls {$smarty.block.child}
     *
     * @var bool
     */
    public $callsChild = false;

    /**
     * Inheritance child block
     *
     * @var Smarty_Internal_Block|null
     */
    public $child = null;

    /**
     * Inheritance calling parent block
     *
     * @var Smarty_Internal_Block|null
     */
    public $parent = null;

    /**
     * Inheritance Template index
     *
     * @var int
     */
    public $tplIndex = 0;

    /**
     * Smarty_Internal_Block constructor.
     * - if outer level {block} of child template ($state == 1) save it as child root block
     * - otherwise process inheritance and render
     *
     * @param \Smarty_Internal_Template $tpl
     * @param int|null                  $tplIndex index of outer level {block} if nested
     */
    public function __construct(Smarty_Internal_Template $tpl, $tplIndex = null)
    {
        $this->tplIndex = $tplIndex ? $tplIndex : $tpl->ext->_inheritance->tplIndex;
        if (isset($tpl->ext->_inheritance->childRoot[ $this->name ])) {
            $this->child = $tpl->ext->_inheritance->childRoot[ $this->name ];
        }
        if ($tpl->ext->_inheritance->state == 1) {
            $tpl->ext->_inheritance->childRoot[ $this->name ] = $this;
            return;
        }
        // make sure we got child block of child template of current block
        while ($this->child && $this->tplIndex <= $this->child->tplIndex) {
            $this->child = $this->child->child;
        }
        $this->process($tpl);
    }

    /**
     * Goto child block or render this
     *
     * @param \Smarty_Internal_Template   $tpl
     * @param \Smarty_Internal_Block|null $parent
     */
    public function process(Smarty_Internal_Template $tpl, Smarty_Internal_Block $parent = null)
    {
        if ($this->hide && !isset($this->child)) {
            return;
        }
        if (isset($this->child) && $this->child->hide && !isset($this->child->child)) {
            $this->child = null;
        }
        $this->parent = $parent;
        if ($this->append && !$this->prepend && isset($parent)) {
            $this->callParent($tpl);
        }
        if ($this->callsChild || !isset($this->child) || ($this->child->hide && !isset($this->child->child))) {
            $this->callBlock($tpl);
        } else {
            $this->child->process($tpl, $this);
        }
        if ($this->prepend && isset($parent)) {
            $this->callParent($tpl);
            if ($this->append) {
                if ($this->callsChild || !isset($this->child) || ($this->child->hide && !isset($this->child->child))) {
                    $this->callBlock($tpl);
                } else {
                    $this->child->process($tpl, $this);
                }
            }
        }
        $this->parent = null;
    }

    /**
     * Compiled block code overloaded by {block} class
     *
     * @param \Smarty_Internal_Template $tpl
     */
    public function callBlock(Smarty_Internal_Template $tpl)
    {
    }

    /**
     * Render child on {$smarty.block.child}
     *
     * @param \Smarty_Internal_Template $tpl
     */
    public function callChild(Smarty_Internal_Template $tpl)
    {
        if (isset($this->child)) {
            $this->child->process($tpl, $this);
        }
    }

    /**
     * Render parent on {$smarty.block.parent} or {block append/prepend}     *
     *
     * @param \Smarty_Internal_Template $tpl
     *
     * @throws \SmartyException
     */
    public function callParent(Smarty_Internal_Template $tpl)
    {
        if (isset($this->parent)) {
            $this->parent->callBlock($tpl);
        } else {
            throw new SmartyException("inheritance: illegal {\$smarty.block.parent} or {block append/prepend} used in parent template '{$tpl->ext->_inheritance->templateResource[$this->tplIndex]}' block '{$this->name}'");
        }
    }
}