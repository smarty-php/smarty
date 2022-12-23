<?php

namespace Smarty\Compile\Tag;

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
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$this->getAttributes($compiler, $args);
		[$openTag, $nocache, $local, $sectionVar] = $this->closeTag($compiler, ['section']);
		$this->openTag($compiler, 'sectionelse', ['sectionelse', $nocache, $local, $sectionVar]);
		return "<?php }} else {\n ?>";
	}
}