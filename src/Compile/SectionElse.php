<?php

namespace Smarty\Compile;

use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile Sectionelse Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class SectionElse extends Base {

	/**
	 * Compiles code for the {sectionelse} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$this->getAttributes($compiler, $args);
		[$openTag, $nocache, $local, $sectionVar] = $this->closeTag($compiler, ['section']);
		$this->openTag($compiler, 'sectionelse', ['sectionelse', $nocache, $local, $sectionVar]);
		return "<?php }} else {\n ?>";
	}
}