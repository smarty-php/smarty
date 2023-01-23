<?php
/**
 * Smarty Internal Plugin Compile Foreach
 * Compiles the {foreach} {foreachelse} {/foreach} tags
 *


 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;

/**
 * Smarty Internal Plugin Compile Foreachclose Class
 *


 */
class ForeachClose extends Base {

	/**
	 * Compiles code for the {/foreach} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 * @throws \Smarty\CompilerException
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		$compiler->loopNesting--;

		[$openTag, $nocache_pushed, $local, $itemVar, $restore] = $this->closeTag($compiler, ['foreach', 'foreachelse']);

		if ($nocache_pushed) {
			// pop the pushed virtual nocache tag
			$this->closeTag($compiler, 'nocache');
			$compiler->tag_nocache = true;
		}

		$output = "<?php\n";
		if ($restore === 2) {
			$output .= "{$itemVar} = {$local}saved;\n";
		}
		$output .= "}\n";
		/* @var \Smarty\Compile\Tag\ForeachTag $foreachCompiler */
		$foreachCompiler = $compiler->getTagCompiler('foreach');
		$output .= $foreachCompiler->compileRestore(1);
		$output .= "?>";
		return $output;
	}
}
