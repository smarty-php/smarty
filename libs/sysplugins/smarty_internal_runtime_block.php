<?php

/**
 * Runtime Method _callBlock, _callParentBlock, _callChildBlock, _registerBlock
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_Block
{
    
    public $inheritanceBlocks = array();
    
    /**
     * Call inheritance {block} tag
     *
     * @param \Smarty_Internal_Template $callerTpl template object of caller
     * @param array                     $block     block parameter
     */
    public function callBlock(Smarty_Internal_Template $callerTpl, $block)
    {
        $function = $block['function'];
        $level = isset($block['level']) ? $block['level'] : 0;
        // find to level child block
        while (!isset($block['callChildBlock']) &&
            isset($this->inheritanceBlocks[$block['name']][$level])) {
            $block = $this->inheritanceBlocks[$block['name']][$level];
            $block['level'] = $level;
            $level ++;
        }
        // ignore hidden block
        if (isset($block['hide'])) {
            return;
        }
        // root block function for possible parent block call
        $block['root'] = $function;
        if (isset($block['append'])) {
            $this->callParentBlock($callerTpl, $block);
        }
        $block['function']($callerTpl, $block);
        if (isset($block['prepend'])) {
            $this->callParentBlock($callerTpl, $block);
        }
    }

    /**
     * Call inheritance parent {block} tag
     *
     * @param \Smarty_Internal_Template $callerTpl template object of caller
     * @param array                     $block     block parameter
     */
    public function callParentBlock(Smarty_Internal_Template $callerTpl, $block)
    {
        $level = isset($block['level']) ? $block['level'] : 0;
        if (isset($this->inheritanceBlocks[$block['name']][$level - 1])) {
            // call registered parent
            $parent = $this->inheritanceBlocks[$block['name']][$level - 1];
            $parent['root'] = $block['root'];
            $parent['function']($callerTpl, $parent);
        } else {
            // default to root block
            $block['root']($callerTpl, $block);
        }
    }

    /**
     * Call inheritance child {block} tag
     *
     * @param \Smarty_Internal_Template $callerTpl template object of caller
     * @param array                     $block     block parameter
     */
    public function callChildBlock(Smarty_Internal_Template $callerTpl, $block)
    {
        $level = isset($block['level']) ? $block['level'] : - 1;
        if (isset($this->inheritanceBlocks[$block['name']][$level + 1])) {
            $child = $this->inheritanceBlocks[$block['name']][$level + 1];
            $child['level'] = $level + 1;
            $child['function']($callerTpl, $child);
        }
    }

    /**
     * Register inheritance {block} tag
     *
     * @param \Smarty_Internal_Template $callerTpl template object of caller
     * @param array                     $block     block parameter
     */
    public function registerBlock(Smarty_Internal_Template $callerTpl, $block)
    {
        if (!isset($this->inheritanceBlocks[$block['name']])) {
            $this->inheritanceBlocks[$block['name']][0] = $block;
        } else {
            array_unshift($this->inheritanceBlocks[$block['name']], $block);
        }
    }
}