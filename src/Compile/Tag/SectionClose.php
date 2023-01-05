<?php
/**
 * Smarty Internal Plugin Compile Section
 * Compiles the {section} {sectionelse} {/section} tags
 *


 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;

/**
 * Smarty Internal Plugin Compile Sectionclose Class
 *


 */
class SectionClose extends Base {

	/**
	 * Compiles code for the {/section} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
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
