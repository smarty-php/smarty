<?php
/**
 * Smarty Internal Plugin Compile Foreach
 * Compiles the {foreach} {foreachelse} {/foreach} tags
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

/**
 * Smarty Internal Plugin Compile Foreachclose Class
 *
 * @package    Smarty
 * @subpackage Compiler
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
		// must endblock be nocache?
		if ($compiler->nocache) {
			$compiler->tag_nocache = true;
		}
		[
			$openTag, $compiler->nocache, $local, $itemVar, $restore,
		] = $this->closeTag($compiler, ['foreach', 'foreachelse']);
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
