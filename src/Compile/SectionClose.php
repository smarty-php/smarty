<?php
/**
 * Smarty Internal Plugin Compile Section
 * Compiles the {section} {sectionelse} {/section} tags
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile;

use Smarty\Compile\ForeachSection;
use Smarty\Compile\Base;
use Smarty_Internal_TemplateCompilerBase;

/**
 * Smarty Internal Plugin Compile Sectionclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class SectionClose extends Base {

	/**
	 * Compiles code for the {/section} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty_Internal_TemplateCompilerBase $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, Smarty_Internal_TemplateCompilerBase $compiler, $parameter = [], $tag = null, $function = null) {
		$compiler->loopNesting--;
		// must endblock be nocache?
		if ($compiler->nocache) {
			$compiler->tag_nocache = true;
		}
		[$openTag, $compiler->nocache, $local, $sectionVar] =
			$this->closeTag($compiler, ['section', 'sectionelse']);
		$output = "<?php\n";
		if ($openTag === 'sectionelse') {
			$output .= "}\n";
		} else {
			$output .= "}\n}\n";
		}
		$output .= '?>';
		return $output;
	}
}
