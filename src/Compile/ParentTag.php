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
 * Smarty Internal Plugin Compile Parent Class
 *
 * @author Uwe Tews <uwe.tews@googlemail.com>
 */
class ParentTag extends Child {

	/**
	 * Tag name
	 *
	 * @var string
	 */
	protected $tag = 'parent';

	/**
	 * Block type
	 *
	 * @var string
	 */
	protected $blockType = 'Parent';
}
