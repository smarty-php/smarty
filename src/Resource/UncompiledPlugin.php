<?php
/**
 * Smarty Resource Plugin
 *


 * @author     Rodney Rehm
 */

namespace Smarty\Resource;

use Smarty\Smarty;
use Smarty\Template;
use Smarty\Template\Compiled;

/**
 * Smarty Resource Plugin
 * Base implementation for resource plugins that don't use the compiler
 *


 */
abstract class UncompiledPlugin extends BasePlugin {

	/**
	 * Flag that it's an uncompiled resource
	 *
	 * @var bool
	 */
	public $uncompiled = true;

	/**
	 * Resource does implement populateCompiledFilepath() method
	 *
	 * @var bool
	 */
	public $hasCompiledHandler = true;

	/**
	 * populate compiled object with compiled filepath
	 *
	 * @param Compiled $compiled compiled object
	 * @param Template $_template template object
	 */
	public function populateCompiledFilepath(Compiled $compiled, Template $_template) {
		$compiled->filepath = $_template->source->filepath;
		$compiled->timestamp = $_template->source->timestamp;
		$compiled->exists = $_template->source->exists;
		if ($_template->smarty->merge_compiled_includes || $_template->source->handler->checkTimestamps()) {
			$compiled->file_dependency[$_template->source->uid] =
				[$compiled->filepath, $compiled->timestamp, $_template->source->type,];
		}
	}
}
