<?php
/**
 * Smarty Resource Plugin
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Rodney Rehm
 */

namespace Smarty\Resource;

use Smarty;
use Smarty_Internal_Template;
use Smarty_Template_Compiled;

/**
 * Smarty Resource Plugin
 * Base implementation for resource plugins that don't use the compiler
 *
 * @package    Smarty
 * @subpackage TemplateResources
 */
abstract class UncompiledPlugin extends asePlugin {

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
	 * @param Smarty_Template_Compiled $compiled compiled object
	 * @param Smarty_Internal_Template $_template template object
	 */
	public function populateCompiledFilepath(Smarty_Template_Compiled $compiled, Smarty_Internal_Template $_template) {
		$compiled->filepath = $_template->source->filepath;
		$compiled->timestamp = $_template->source->timestamp;
		$compiled->exists = $_template->source->exists;
		if ($_template->smarty->merge_compiled_includes || $_template->source->handler->checkTimestamps()) {
			$compiled->file_dependency[$_template->source->uid] =
				[$compiled->filepath, $compiled->timestamp, $_template->source->type,];
		}
	}
}
