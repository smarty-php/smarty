<?php
/**
 * Smarty Internal Plugin Compile extend
 * Compiles the {extends} tag
 *


 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

/**
 * Smarty Internal Plugin Compile extend Class
 *


 */
class ExtendsTag extends Inheritance {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BasePlugin
	 */
	protected $required_attributes = ['file'];

	/**
	 * Array of names of optional attribute required by tag
	 * use array('_any') if there is no restriction of attributes names
	 *
	 * @var array
	 */
	protected $optional_attributes = ['extends_resource'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BasePlugin
	 */
	protected $shorttag_order = ['file'];

	/**
	 * Compiles code for the {extends} tag extends: resource
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 *
	 * @return string compiled code
	 * @throws \Smarty\CompilerException
	 * @throws \Smarty\Exception
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		if ($_attr['nocache'] === true) {
			$compiler->trigger_template_error('nocache option not allowed', $compiler->getParser()->lex->line - 1);
		}
		if (strpos($_attr['file'], '$_tmp') !== false) {
			$compiler->trigger_template_error('illegal value for file attribute', $compiler->getParser()->lex->line - 1);
		}
		// add code to initialize inheritance
		$this->registerInit($compiler, true);
		$file = trim($_attr['file'], '\'"');
		if (strlen($file) > 8 && substr($file, 0, 8) === 'extends:') {
			// generate code for each template
			$files = array_reverse(explode('|', substr($file, 8)));
			$i = 0;
			foreach ($files as $file) {
				if ($file[0] === '"') {
					$file = trim($file, '".');
				} else {
					$file = "'{$file}'";
				}
				$i++;
				if ($i === count($files) && isset($_attr['extends_resource'])) {
					$this->compileEndChild($compiler);
				}
				$this->compileInclude($compiler, $file);
			}
			if (!isset($_attr['extends_resource'])) {
				$this->compileEndChild($compiler);
			}
		} else {
			$this->compileEndChild($compiler, $_attr['file']);
		}
		$compiler->has_code = false;
		return '';
	}

	/**
	 * Add code for inheritance endChild() method to end of template
	 *
	 * @param \Smarty\Compiler\Template $compiler
	 * @param null|string $template optional inheritance parent template
	 *
	 * @throws \Smarty\CompilerException
	 * @throws \Smarty\Exception
	 */
	private function compileEndChild(\Smarty\Compiler\Template $compiler, $template = null) {
		$compiler->getParser()->template_postfix[] = new \Smarty\ParseTree\Tag(
			$compiler->getParser(),
			'<?php $_smarty_tpl->getInheritance()->endChild($_smarty_tpl' .
			(isset($template) ?	", {$template}, \$_smarty_current_dir" : '') . ");\n?>"
		);
	}

	/**
	 * Add code for including subtemplate to end of template
	 *
	 * @param \Smarty\Compiler\Template $compiler
	 * @param string $template subtemplate name
	 *
	 * @throws \Smarty\CompilerException
	 * @throws \Smarty\Exception
	 */
	private function compileInclude(\Smarty\Compiler\Template $compiler, $template) {
		$compiler->getParser()->template_postfix[] = new \Smarty\ParseTree\Tag(
			$compiler->getParser(),
			$compiler->compileTag(
				'include',
				[
					$template,
					['scope' => 'parent'],
				]
			)
		);
	}

	/**
	 * Create source code for {extends} from source components array
	 *
	 * @param \Smarty\Template $template
	 *
	 * @return string
	 */
	public static function extendsSourceArrayCode(\Smarty\Template $template) {
		$resources = [];
		foreach ($template->getSource()->components as $source) {
			$resources[] = $source->resource;
		}
		return $template->getLeftDelimiter() . 'extends file=\'extends:' . join('|', $resources) .
			'\' extends_resource=true' . $template->getRightDelimiter();
	}
}
