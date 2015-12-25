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
     *
     * @param \Smarty_Internal_Template $_smarty_tpl
     *
     */
    public function __construct(Smarty_Internal_Template $_smarty_tpl)
    {
        $this->tplIndex = $_smarty_tpl->ext->_inheritance->tplIndex;
        if (isset($_smarty_tpl->ext->_inheritance->blockParameter[ $this->name ])) {
            $this->child = $_smarty_tpl->ext->_inheritance->blockParameter[ $this->name ];
        }
        if ($_smarty_tpl->ext->_inheritance->state == 1) {
            $_smarty_tpl->ext->_inheritance->blockParameter[ $this->name ] = $this;
            return;
        }
        $this->process($_smarty_tpl);
    }

    /**
     * Goto child block or render this
     *
     * @param \Smarty_Internal_Template   $_smarty_tpl
     * @param \Smarty_Internal_Block|null $parent
     */
    public function process(Smarty_Internal_Template $_smarty_tpl, Smarty_Internal_Block $parent = null) {
        if ($this->hide && !isset($this->child)) {
             return;
        }
        if (isset($this->child) && $this->child->hide && !isset($this->child->child)) {
            $this->child = null;
        }
        $this->parent = $parent;
        if ($this->append && !$this->prepend && isset($parent)) {
            $this->callParent($_smarty_tpl);
        }
        if ($this->callsChild || !isset($this->child) || ($this->child->hide && !isset($this->child->child))) {
            $this->callBlock($_smarty_tpl);
        } else {
            $this->child->process($_smarty_tpl, $this);
        }
        if ($this->prepend && isset($parent)) {
            $this->callParent($_smarty_tpl);
            if ($this->append) {
                if ($this->callsChild || !isset($this->child) || ($this->child->hide && !isset($this->child->child))) {
                    $this->callBlock($_smarty_tpl);
                } else {
                    $this->child->process($_smarty_tpl, $this);
                }
           }
        }
        $this->parent = null;
    }

    /**
     * Compiled block code overloaded by {block} class
     *
     * @param \Smarty_Internal_Template   $_smarty_tpl
     */
    public function callBlock(Smarty_Internal_Template $_smarty_tpl) {
    }

    /**
     * Render child on {$smarty.block.child}
     *
     * @param \Smarty_Internal_Template $_smarty_tpl
     */
    public function callChild (Smarty_Internal_Template $_smarty_tpl) {
        if (isset($this->child)) {
            $this->child->process($_smarty_tpl, $this);
        }
    }

    /**
     * Render parent on {$smarty.block.parent} or {block append/prepend}     *
     *
     * @param \Smarty_Internal_Template   $_smarty_tpl
     *
     * @throws \SmartyException
     */
    public function callParent (Smarty_Internal_Template $_smarty_tpl) {
        if (isset($this->parent)) {
            $this->parent->callBlock($_smarty_tpl, $this->parent->parent);
        } else {
            throw new SmartyException("inheritance: illegal {\$smarty.block.parent} or {block append/prepend} used in parent template '{$_smarty_tpl->ext->_inheritance->compiledFilePath[$this->tplIndex]}' block '{$this->name}'");
        }
    }
}