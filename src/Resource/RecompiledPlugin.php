<?php
/**
 * Smarty Resource Plugin
 *


 * @author     Rodney Rehm
 */

namespace Smarty\Resource;

use Smarty\Template;

/**
 * Smarty Resource Plugin
 * Base implementation for resource plugins that don't compile cache
 *


 */
abstract class RecompiledPlugin extends BasePlugin {

	/**
	 * Flag that it's an recompiled resource
	 *
	 * @var bool
	 */
	public $recompiled = true;

	/**
	 * Flag if resource does allow compilation
	 *
	 * @return bool
	 */
	public function supportsCompiledTemplates(): bool {
		return false;
	}

	/**
	 * compile template from source
	 *
	 * @param Template $_smarty_tpl do not change variable name, is used by compiled template
	 *
	 * @throws Exception
	 */
	public function recompile(Template $_smarty_tpl) {
		$compiled = $_smarty_tpl->getCompiled();
		$compiled->file_dependency = [];
		$compiled->includes = [];
		$compiled->nocache_hash = null;
		$compiled->unifunc = null;
		$level = ob_get_level();
		ob_start();
		// call compiler
		try {
			eval('?>' . $_smarty_tpl->getCompiler()->compileTemplate($_smarty_tpl));
		} catch (\Exception $e) {
			while (ob_get_level() > $level) {
				ob_end_clean();
			}
			throw $e;
		}
		ob_get_clean();
		$compiled->timestamp = time();
		$compiled->exists = true;
	}

	/*
	   * Disable timestamp checks for recompiled resource.
	   *
	   * @return bool
	   */
	/**
	 * @return bool
	 */
	public function checkTimestamps() {
		return false;
	}
}
