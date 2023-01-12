<?php
/**
 * Smarty Internal Plugin Compile Include
 * Compiles the {include} tag
 *


 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;
use Smarty\Compiler\Template;
use Smarty\Data;
use Smarty\Smarty;
use Smarty\Template\Compiled;

/**
 * Smarty Internal Plugin Compile Include Class
 *


 */
class IncludeTag extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BaseCompiler
	 */
	protected $required_attributes = ['file'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BaseCompiler
	 */
	protected $shorttag_order = ['file'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BaseCompiler
	 */
	protected $option_flags = ['nocache', 'inline', 'caching'];

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see BaseCompiler
	 */
	protected $optional_attributes = ['_any'];

	/**
	 * Compiles code for the {include} tag
	 *
	 * @param array $args array with attributes from parser
	 * @param Template $compiler compiler object
	 *
	 * @return string
	 * @throws \Exception
	 * @throws \Smarty\CompilerException
	 * @throws \Smarty\Exception
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		$uid = $t_hash = null;
		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		$fullResourceName = $source_resource = $_attr['file'];
		$variable_template = false;
		$cache_tpl = false;
		// parse resource_name
		if (preg_match('/^([\'"])(([A-Za-z0-9_\-]{2,})[:])?(([^$()]+)|(.+))\1$/', $source_resource, $match)) {
			$type = !empty($match[3]) ? $match[3] : $compiler->template->smarty->default_resource_type;
			$name = !empty($match[5]) ? $match[5] : $match[6];
			$handler = \Smarty\Resource\BasePlugin::load($compiler->smarty, $type);
			if ($handler->recompiled) {
				$variable_template = true;
			}
			if (!$variable_template) {
				if ($type !== 'string') {
					$fullResourceName = "{$type}:{$name}";
					$compiled = $compiler->parent_compiler->template->getCompiled();
					if (isset($compiled->includes[$fullResourceName])) {
						$compiled->includes[$fullResourceName]++;
						$cache_tpl = true;
					} else {
						if ("{$compiler->template->source->type}:{$compiler->template->source->name}" ==
							$fullResourceName
						) {
							// recursive call of current template
							$compiled->includes[$fullResourceName] = 2;
							$cache_tpl = true;
						} else {
							$compiled->includes[$fullResourceName] = 1;
						}
					}
					$fullResourceName = $match[1] . $fullResourceName . $match[1];
				}
			}
			if (empty($match[5])) {
				$variable_template = true;
			}
		} else {
			$variable_template = true;
		}
		// scope setup
		$_scope = $this->convertScope($_attr, [Data::SCOPE_LOCAL]);

		// assume caching is off
		$_caching = Smarty::CACHING_OFF;
		$call_nocache = $compiler->tag_nocache || $compiler->nocache;
		// caching was on and {include} is not in nocache mode
		if ($compiler->template->caching && !$compiler->nocache && !$compiler->tag_nocache) {
			$_caching = \Smarty\Template::CACHING_NOCACHE_CODE;
		}
		// flag if included template code should be merged into caller
		$merge_compiled_includes = ($compiler->smarty->merge_compiled_includes || $_attr['inline'] === true) &&
			!$compiler->template->source->handler->recompiled;
		if ($merge_compiled_includes) {
			// variable template name ?
			if ($variable_template) {
				$merge_compiled_includes = false;
			}
			// variable compile_id?
			if (isset($_attr['compile_id']) && $compiler->isVariable($_attr['compile_id'])) {
				$merge_compiled_includes = false;
			}
		}
		/*
		* if the {include} tag provides individual parameter for caching or compile_id
		* the subtemplate must not be included into the common cache file and is treated like
		* a call in nocache mode.
		*
		*/
		if ($_attr['nocache'] !== true && $_attr['caching']) {
			$_caching = $_new_caching = (int)$_attr['caching'];
			$call_nocache = true;
		} else {
			$_new_caching = Smarty::CACHING_LIFETIME_CURRENT;
		}
		if (isset($_attr['cache_lifetime'])) {
			$_cache_lifetime = $_attr['cache_lifetime'];
			$call_nocache = true;
			$_caching = $_new_caching;
		} else {
			$_cache_lifetime = '$_smarty_tpl->cache_lifetime';
		}
		if (isset($_attr['cache_id'])) {
			$_cache_id = $_attr['cache_id'];
			$call_nocache = true;
			$_caching = $_new_caching;
		} else {
			$_cache_id = '$_smarty_tpl->cache_id';
		}
		if (isset($_attr['compile_id'])) {
			$_compile_id = $_attr['compile_id'];
		} else {
			$_compile_id = '$_smarty_tpl->compile_id';
		}
		// if subtemplate will be called in nocache mode do not merge
		if ($compiler->template->caching && $call_nocache) {
			$merge_compiled_includes = false;
		}
		// assign attribute
		if (isset($_attr['assign'])) {
			// output will be stored in a smarty variable instead of being displayed
			if ($_assign = $compiler->getId($_attr['assign'])) {
				$_assign = "'{$_assign}'";
				if ($compiler->tag_nocache || $compiler->nocache || $call_nocache) {
					// create nocache var to make it know for further compiling
					$compiler->setNocacheInVariable($_attr['assign']);
				}
			} else {
				$_assign = $_attr['assign'];
			}
		}
		$has_compiled_template = false;
		if ($merge_compiled_includes) {
			$c_id = $_attr['compile_id'] ?? $compiler->template->compile_id;
			// we must observe different compile_id and caching
			$t_hash = sha1($c_id . ($_caching ? '--caching' : '--nocaching'));

			$compiler->smarty->setAllowAmbiguousResources(true);
			$tpl = $compiler->smarty->createTemplate(
				trim($fullResourceName, '"\''),
				$compiler->template->cache_id,
				$c_id,
				$compiler->template,
				$_caching
			);

			$uid = $tpl->source->type . $tpl->source->uid;
			if (!isset($compiler->parent_compiler->mergedSubTemplatesData[$uid][$t_hash])) {
				$has_compiled_template = $this->compileInlineTemplate($compiler, $tpl, $t_hash);
			} else {
				$has_compiled_template = true;
			}

			$compiler->smarty->setAllowAmbiguousResources(false);

		}
		// delete {include} standard attributes
		unset($_attr['file'], $_attr['assign'], $_attr['cache_id'], $_attr['compile_id'], $_attr['cache_lifetime'], $_attr['nocache'], $_attr['caching'], $_attr['scope'], $_attr['inline']);
		// remaining attributes must be assigned as smarty variable
		$_vars = 'array()';
		if (!empty($_attr)) {
			$_pairs = [];
			// create variables
			foreach ($_attr as $key => $value) {
				$_pairs[] = "'$key'=>$value";
			}
			$_vars = 'array(' . join(',', $_pairs) . ')';
		}
		$update_compile_id = $compiler->template->caching && !$compiler->tag_nocache && !$compiler->nocache &&
			$_compile_id !== '$_smarty_tpl->compile_id';
		if ($has_compiled_template && !$call_nocache) {
			$_output = "<?php\n";
			if ($update_compile_id) {
				$_output .= $compiler->makeNocacheCode("\$_compile_id_save[] = \$_smarty_tpl->compile_id;\n\$_smarty_tpl->compile_id = {$_compile_id};\n");
			}
			if (!empty($_attr) && $_caching === \Smarty\Template::CACHING_NOCACHE_CODE && $compiler->template->caching) {
				$_vars_nc = "foreach ($_vars as \$ik => \$iv) {\n";
				$_vars_nc .= "\$_smarty_tpl->assign(\$ik, \$iv);\n";
				$_vars_nc .= "}\n";
				$_output .= substr($compiler->processNocacheCode('<?php ' . $_vars_nc . "?>\n", true), 6, -3);
			}
			if (isset($_assign)) {
				$_output .= "ob_start();\n";
			}
			$_output .= "\$_smarty_tpl->_subTemplateRender({$fullResourceName}, {$_cache_id}, {$_compile_id}, {$_caching}, {$_cache_lifetime}, {$_vars}, '{$compiler->parent_compiler->mergedSubTemplatesData[$uid][$t_hash]['uid']}', '{$compiler->parent_compiler->mergedSubTemplatesData[$uid][$t_hash]['func']}');\n";
			if (isset($_assign)) {
				$_output .= "\$_smarty_tpl->assign({$_assign}, ob_get_clean(), false, {$_scope});\n";
			}
			if ($update_compile_id) {
				$_output .= $compiler->makeNocacheCode("\$_smarty_tpl->compile_id = array_pop(\$_compile_id_save);\n");
			}
			$_output .= "?>";
			return $_output;
		}
		if ($call_nocache) {
			$compiler->tag_nocache = true;
		}
		$_output = "<?php ";
		if ($update_compile_id) {
			$_output .= "\$_compile_id_save[] = \$_smarty_tpl->compile_id;\n\$_smarty_tpl->compile_id = {$_compile_id};\n";
		}
		// was there an assign attribute
		if (isset($_assign)) {
			$_output .= "ob_start();\n";
		}
		$_output .= "\$_smarty_tpl->_subTemplateRender({$fullResourceName}, $_cache_id, $_compile_id, $_caching, $_cache_lifetime, $_vars);\n";
		if (isset($_assign)) {
			$_output .= "\$_smarty_tpl->assign({$_assign}, ob_get_clean(), false, {$_scope});\n";
		}
		if ($update_compile_id) {
			$_output .= "\$_smarty_tpl->compile_id = array_pop(\$_compile_id_save);\n";
		}
		$_output .= "?>";
		return $_output;
	}

	/**
	 * Compile inline sub template
	 *
	 * @param \Smarty\Compiler\Template $compiler
	 * @param \Smarty\Template $tpl
	 * @param string $t_hash
	 *
	 * @return bool
	 * @throws \Exception
	 * @throws \Smarty\Exception
	 */
	private function compileInlineTemplate(
		Template $compiler,
		\Smarty\Template $tpl,
		         $t_hash
	) {
		$uid = $tpl->source->type . $tpl->source->uid;
		if ($tpl->source->exists) {
			$compiler->parent_compiler->mergedSubTemplatesData[$uid][$t_hash]['uid'] = $tpl->source->uid;
			$tpl->getCompiled(true)->nocache_hash = $compiler->parent_compiler->template->getCompiled()->nocache_hash;
			// save unique function name
			$compiler->parent_compiler->mergedSubTemplatesData[$uid][$t_hash]['func'] =
			$tpl->getCompiled()->unifunc = 'content_' . str_replace(['.', ','], '_', uniqid('', true));
			// make sure whole chain gets compiled
			$tpl->mustCompile = true;
			$compiler->parent_compiler->mergedSubTemplatesData[$uid][$t_hash]['nocache_hash'] =
				$tpl->getCompiled()->nocache_hash;
			if ($tpl->source->type === 'file') {
				$sourceInfo = $tpl->source->filepath;
			} else {
				$basename = $tpl->source->handler->getBasename($tpl->source);
				$sourceInfo = $tpl->source->type . ':' .
					($basename ? $basename : $tpl->source->name);
			}
			// get compiled code
			$compiled_code = "<?php\n\n";
			$compiled_code .= $compiler->cStyleComment(" Start inline template \"{$sourceInfo}\" =============================") . "\n";
			$compiled_code .= "function {$tpl->getCompiled()->unifunc} (\\Smarty\\Template \$_smarty_tpl) {\n";
			$compiled_code .= "?>\n" . $tpl->getCompiler()->compileTemplateSource($tpl, null, $compiler->parent_compiler);
			$compiled_code .= "<?php\n";
			$compiled_code .= "}\n?>\n";
			$compiled_code .= $tpl->smarty->runPostFilters($tpl->getCompiler()->blockOrFunctionCode, $tpl);
			$compiled_code .= "<?php\n\n";
			$compiled_code .= $compiler->cStyleComment(" End inline template \"{$sourceInfo}\" =============================") . "\n";
			$compiled_code .= '?>';

//			unset($tpl->compiler); // @TODO removed this.

			if ($tpl->getCompiled()->getNocacheCode()) {
				// replace nocache_hash
				$compiled_code =
					str_replace(
						"{$tpl->getCompiled()->nocache_hash}",
						$compiler->template->getCompiled()->nocache_hash,
						$compiled_code
					);
				$compiler->template->getCompiled()->setNocacheCode(true);
			}
			$compiler->parent_compiler->mergedSubTemplatesCode[$tpl->getCompiled()->unifunc] = $compiled_code;
			return true;
		} else {
			return false;
		}
	}
}
