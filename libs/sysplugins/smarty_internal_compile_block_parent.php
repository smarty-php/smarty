<?php
/**
 * This file is part of Smarty.
 *
 * (c) 2015 Uwe Tews
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Smarty Internal Plugin Compile Block Parent Class
 *
 * @author Uwe Tews <uwe.tews@googlemail.com>
 */
// PHP 8.2+: Allow dynamic properties for compiler state and tag-specific data
#[\AllowDynamicProperties]
class Smarty_Internal_Compile_Block_Parent extends Smarty_Internal_Compile_Child
{
    /**
     * Tag name
     *
     * @var string
     */
    public $tag = 'block_parent';

    /**
     * Block type
     *
     * @var string
     */
    public $blockType = 'Parent';
}
