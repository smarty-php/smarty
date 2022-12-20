<?php
/**
 * This file is part of Smarty.
 *
 * (c) 2015 Uwe Tews
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Smarty\Compile;

/**
 * Smarty Internal Plugin Compile Block Parent Class
 *
 * @author Uwe Tews <uwe.tews@googlemail.com>
 */
class BlockParent extends Child {

	/**
	 * Tag name
	 *
	 * @var string
	 */
	protected $tag = 'block_parent';

	/**
	 * Block type
	 *
	 * @var string
	 */
	protected $blockType = 'Parent';
}
