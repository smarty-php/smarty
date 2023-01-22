<?php
/**
 * Smarty Internal Plugin Template
 * This file contains the Smarty template engine
 *


 * @author     Uwe Tews
 */

namespace Smarty;

use Smarty\Runtime\InheritanceRuntime;
use Smarty\Template\Source;
use Smarty\Template\Cached;
use Smarty\Template\Compiled;
use Smarty\Template\Config;

/**
 * Main class with template data structures and methods
 */
#[\AllowDynamicProperties]
class Template extends TemplateBase {

	/**
	 * caching mode to create nocache code but no cache file
	 */
	public const CACHING_NOCACHE_CODE = 9999;

	/**
	 * @var Compiled
	 */
	private $compiled = null;

	/**
	 * @var Cached
	 */
	private $cached = null;

	/**
	 * @var \Smarty\Compiler\Template
	 */
	private $compiler = null;

	/**
	 * Source instance
	 *
	 * @var Source|Config
	 */
	private $source = null;

	/**
	 * Template resource
	 *
	 * @var string
	 */
	public $template_resource = null;

	/**
	 * flag if compiled template is invalid and must be (re)compiled
	 *
	 * @var bool
	 */
	public $mustCompile = null;

	/**
	 * Template ID
	 *
	 * @var null|string
	 */
	public $templateId = null;

	/**
	 * Flag which is set while rending a cache file
	 *
	 * @var bool
	 */
	public $isRenderingCache = false;

	/**
	 * Callbacks called before rendering template
	 *
	 * @var callback[]
	 */
	public $startRenderCallbacks = [];

	/**
	 * Callbacks called after rendering template
	 *
	 * @var callback[]
	 */
	public $endRenderCallbacks = [];

	/**
	 * Template left-delimiter. If null, defaults to $this->getSmarty()-getLeftDelimiter().
	 *
	 * @var string
	 */
	private $left_delimiter = null;

	/**
	 * Template right-delimiter. If null, defaults to $this->getSmarty()-getRightDelimiter().
	 *
	 * @var string
	 */
	private $right_delimiter = null;


	/**
	 * Create template data object
	 * Some of the global Smarty settings copied to template scope
	 * It load the required template resources and caching plugins
	 *
	 * @param string $template_resource template resource string
	 * @param Smarty $smarty Smarty instance
	 * @param \Smarty\Data|null $_parent back pointer to parent object with variables or null
	 * @param mixed $_cache_id cache   id or null
	 * @param mixed $_compile_id compile id or null
	 * @param bool|int|null $_caching use caching?
	 * @param bool $_isConfig
	 *
	 * @throws \Smarty\Exception
	 */
	public function __construct(
		$template_resource,
		Smarty $smarty,
		\Smarty\Data $_parent = null,
		$_cache_id = null,
		$_compile_id = null,
		$_caching = null,
		$_isConfig = false
	) {
		$this->smarty = $smarty;
		// Smarty parameter
		$this->cache_id = $_cache_id === null ? $this->smarty->cache_id : $_cache_id;
		$this->compile_id = $_compile_id === null ? $this->smarty->compile_id : $_compile_id;
		$this->caching = (int)($_caching === null ? $this->smarty->caching : $_caching);
		$this->cache_lifetime = $this->smarty->cache_lifetime;
		$this->compile_check = (int)$smarty->compile_check;
		$this->parent = $_parent;
		// Template resource
		$this->template_resource = $template_resource;
		$this->source = $_isConfig ? Config::load($this) : Source::load($this);

		if ($smarty->security_policy) {
			$smarty->security_policy->registerCallBacks($this);
		}
	}

	/**
	 * render template
	 *
	 * @param bool $no_output_filter if true do not run output filter
	 * @param null|bool $display true: display, false: fetch null: sub-template
	 *
	 * @return string
	 * @throws \Exception
	 * @throws \Smarty\Exception
	 */
	public function render($no_output_filter = true, $display = null) {
		if ($this->smarty->debugging) {
			$this->smarty->getDebug()->start_template($this, $display);
		}
		// checks if template exists
		if (!$this->getSource()->exists) {
			throw new Exception(
				"Unable to load template '{$this->getSource()->type}:{$this->getSource()->name}'" .
				($this->_isSubTpl() ? " in '{$this->parent->template_resource}'" : '')
			);
		}
		// disable caching for evaluated code
		if ($this->getSource()->handler->recompiled) {
			$this->caching = \Smarty\Smarty::CACHING_OFF;
		}
		// read from cache or render
		if ($this->caching === \Smarty\Smarty::CACHING_LIFETIME_CURRENT || $this->caching === \Smarty\Smarty::CACHING_LIFETIME_SAVED) {
			if ($this->getCached()->cache_id !== $this->cache_id || $this->getCached()->compile_id !== $this->compile_id) {
				$this->getCached(true);
			}
			$this->getCached()->render($this, $no_output_filter);
		} else {
			$compiled = $this->getCompiled();
			if ($compiled->compile_id !== $this->compile_id) {
				$compiled = $this->getCompiled(true);
			}
			$compiled->render($this);
		}
		// display or fetch
		if ($display) {
			if ($this->caching && $this->smarty->cache_modified_check) {
				$this->smarty->cacheModifiedCheck(
					$this->getCached(),
					$this,
					isset($content) ? $content : ob_get_clean()
				);
			} else {
				if ((!$this->caching || $this->getCached()->getNocacheCode() || $this->getSource()->handler->recompiled)
					&& !$no_output_filter && isset($this->smarty->registered_filters['output'])
				) {
					echo $this->smarty->runOutputFilters(ob_get_clean(), $this);
				} else {
					echo ob_get_clean();
				}
			}
			if ($this->smarty->debugging) {
				$this->smarty->getDebug()->end_template($this);
				// debug output
				$this->smarty->getDebug()->display_debug($this, true);
			}
			return '';
		} else {
			if ($this->smarty->debugging) {
				$this->smarty->getDebug()->end_template($this);
				if ($this->smarty->debugging === 2 && $display === false) {
					$this->smarty->getDebug()->display_debug($this, true);
				}
			}
			if (
				!$no_output_filter
				&& (!$this->caching || $this->getCached()->getNocacheCode() || $this->getSource()->handler->recompiled)
			) {

				return $this->smarty->runOutputFilters(ob_get_clean(), $this);
			}
			// return cache content
			return null;
		}
	}

	/**
	 * Runtime function to render sub-template
	 *
	 * @param string $template_name template name
	 * @param mixed $cache_id cache id
	 * @param mixed $compile_id compile id
	 * @param integer $caching cache mode
	 * @param integer $cache_lifetime lifetime of cache data
	 * @param array $extra_vars passed parameter template variables
	 * @param null $uid file dependency uid
	 * @param null $content_func function name
	 * @param int|null $scope
	 *
	 * @throws Exception
	 */
	public function _subTemplateRender(
		$template_name,
		$cache_id,
		$compile_id,
		$caching,
		$cache_lifetime,
		array $extra_vars,
		$uid = null,
		$content_func = null,
		int $scope = null
	) {

		$baseFilePath = $this->source && $this->getSource()->filepath ? dirname($this->getSource()->filepath) : null;

		$tpl = $this->getSmarty()->createTemplate($template_name, $cache_id, $compile_id, $this, $caching, $cache_lifetime, $baseFilePath);
		$tpl->setCached($this->getCached()); // re-use the same Cache object across subtemplates to gather hashes and file dependencies.

		if ($scope) {
			$tpl->setDefaultScope($scope);
		}

		// copy variables
		$tpl->tpl_vars = $this->tpl_vars;
		$tpl->config_vars = $this->config_vars;

		// recursive call ?
		if ($tpl->getTemplateId() == $this->getTemplateId()) {

			if (isset($uid) && $this->getCompiled()->file_dependency) {
				// for inline templates we can get all resource information from file dependency
				[$filepath, $timestamp, $type] = $this->getCompiled()->file_dependency[$uid];
				$source = new Source($this->getSmarty(), $filepath, $type, $filepath);
				$source->filepath = $filepath;
				$source->timestamp = $timestamp;
				$source->exists = true;
				$source->uid = $uid;
				$tpl->setSource($source);
			} else {
				$tpl->setSource(Source::load($tpl));
				$tpl->getCompiled(true); // @TODO this unset($tpl->compiled), there might be a bug here
			}
			if ($caching !== \Smarty\Template::CACHING_NOCACHE_CODE) {
				$tpl->getCached(true); // @TODO this unset($tpl->cached), there might be a bug here
			}
		}

		if (!empty($extra_vars)) {
			// set up variable values
			foreach ($extra_vars as $_key => $_val) {
				$tpl->assign($_key, $_val);
			}
		}
		if ($tpl->caching === \Smarty\Template::CACHING_NOCACHE_CODE) {
			if ($tpl->getCompiled()->getNocacheCode()) {
				$this->getCached()->hashes[$tpl->getCompiled()->nocache_hash] = true;
			}
		}
		if (isset($uid)) {
			$smarty = $this->getSmarty();
			if ($smarty->debugging) {
				$smarty->getDebug()->start_template($tpl);
				$smarty->getDebug()->start_render($tpl);
			}
			$tpl->getRenderedTemplateCode($content_func);
			if ($smarty->debugging) {
				$smarty->getDebug()->end_template($tpl);
				$smarty->getDebug()->end_render($tpl);
			}
		} else {
			$tpl->getCompiled()->render($tpl);

			// @TODO: this used to be like this. Might cause a bug.
//			if (isset($tpl->compiled)) {
//				$tpl->getCompiled()->render($tpl);
//			} else {
//				$tpl->render();
//			}
		}
	}

	/**
	 * Check if this is a sub template
	 *
	 * @return bool true is sub template
	 */
	public function _isSubTpl() {
		return isset($this->parent) && $this->parent instanceof Template;
	}

	public function assign($tpl_var, $value = null, $nocache = false, $scope = null) {
		return parent::assign($tpl_var, $value, $nocache || $this->isRenderingCache, $scope);
	}

	/**
	 * This function is executed automatically when a compiled or cached template file is included
	 * - Decode saved properties from compiled template and cache files
	 * - Check if compiled or cache file is valid
	 *
	 * @param array $properties special template properties
	 * @param bool $cache flag if called from cache file
	 *
	 * @return bool flag if compiled or cache file is valid
	 * @throws \Smarty\Exception
	 */
	public function isFresh($properties, $cache = false) {
		// on cache resources other than file check version stored in cache code
		if (!isset($properties['version']) || \Smarty\Smarty::SMARTY_VERSION !== $properties['version']) {
			if ($cache) {
				$this->getSmarty()->clearAllCache();
			} else {
				$this->getSmarty()->clearCompiledTemplate();
			}
			return false;
		}
		$is_valid = true;
		if (!empty($properties['file_dependency'])
			&& ((!$cache && $this->compile_check) || $this->compile_check === \Smarty\Smarty::COMPILECHECK_ON)
		) {
			// check file dependencies at compiled code
			foreach ($properties['file_dependency'] as $_file_to_check) {
				if ($_file_to_check[2] === 'file' || $_file_to_check[2] === 'php') {
					if ($this->getSource()->filepath === $_file_to_check[0]) {
						// do not recheck current template
						continue;
						//$mtime = $this->getSource()->getTimeStamp();
					} else {
						// file and php types can be checked without loading the respective resource handlers
						$mtime = is_file($_file_to_check[0]) ? filemtime($_file_to_check[0]) : false;
					}
				} else {
					$handler = \Smarty\Resource\BasePlugin::load($this->getSmarty(), $_file_to_check[2]);
					if ($handler->checkTimestamps()) {
						$source = Source::load($this, $this->getSmarty(), $_file_to_check[0]);
						$mtime = $source->getTimeStamp();
					} else {
						continue;
					}
				}
				if ($mtime === false || $mtime > $_file_to_check[1]) {
					$is_valid = false;
					break;
				}
			}
		}
		if ($cache) {
			// CACHING_LIFETIME_SAVED cache expiry has to be validated here since otherwise we'd define the unifunc
			if ($this->caching === \Smarty\Smarty::CACHING_LIFETIME_SAVED && $properties['cache_lifetime'] >= 0
				&& (time() > ($this->getCached()->timestamp + $properties['cache_lifetime']))
			) {
				$is_valid = false;
			}
			$this->getCached()->cache_lifetime = $properties['cache_lifetime'];
			$this->getCached()->valid = $is_valid;
			$generatedFile = $this->getCached();
		} else {
			$this->mustCompile = !$is_valid;
			$generatedFile = $this->getCompiled();
			$generatedFile->includes = $properties['includes'] ?? [];
		}
		if ($is_valid) {
			$generatedFile->unifunc = $properties['unifunc'];
			$generatedFile->setNocacheCode($properties['has_nocache_code']);
			$generatedFile->file_dependency = $properties['file_dependency'];
		}
		return $is_valid && !function_exists($properties['unifunc']);
	}

	/**
	 * Compiles the template
	 * If the template is not evaluated the compiled template is saved on disk
	 *
	 * @throws \Exception
	 */
	public function compileTemplateSource() {
		return $this->getCompiled()->compileTemplateSource($this);
	}

	/**
	 * Return cached content
	 *
	 * @return null|string
	 * @throws Exception
	 */
	public function getCachedContent() {
		return $this->getCachedContent($this);
	}

	/**
	 * Writes the content to cache resource
	 *
	 * @param string $content
	 *
	 * @return bool
	 */
	public function writeCachedContent($content) {
		if ($this->getSource()->handler->recompiled || !$this->caching
		) {
			// don't write cache file
			return false;
		}
		$codeframe = $this->createCodeFrame($content, '', true);
		return $this->getCached()->writeCache($this, $codeframe);
	}

	/**
	 * Get unique template id
	 *
	 * @return string
	 */
	public function getTemplateId() {
		return $this->templateId ?? $this->templateId =
			$this->smarty->generateUniqueTemplateId($this->template_resource, $this->cache_id, $this->compile_id);
	}

	/**
	 * runtime error not matching capture tags
	 *
	 * @throws \Smarty\Exception
	 */
	public function capture_error() {
		throw new Exception("Not matching {capture} open/close in '{$this->template_resource}'");
	}

	/**
	 * Return Compiled object
	 *
	 * @param bool $forceNew force new compiled object
	 */
	public function getCompiled($forceNew = false) {
		if ($forceNew || !isset($this->compiled)) {
			$this->compiled = Compiled::load($this);
		}
		return $this->compiled;
	}

	/**
	 * Return Cached object
	 *
	 * @param bool $forceNew force new cached object
	 *
	 * @throws Exception
	 */
	public function getCached($forceNew = false): Cached {
		if ($forceNew || !isset($this->cached)) {
			$cacheResource = $this->smarty->getCacheResource();
			$this->cached = new Cached(
				$this->source,
				$cacheResource,
				$this->compile_id,
				$this->cache_id
			);
			$cacheResource->populate($this->cached, $this);
			if (!$this->isCachingEnabled()) {
				$this->cached->setValid(false);
			}
		}
		return $this->cached;
	}

	public function isCachingEnabled(): bool {
		return $this->caching && !$this->getSource()->handler->recompiled;
	}

	/**
	 * Helper function for InheritanceRuntime object
	 *
	 * @return InheritanceRuntime
	 * @throws Exception
	 */
	public function getInheritance(): InheritanceRuntime {
		return $this->getSmarty()->getRuntime('Inheritance');
	}

	/**
	 * Unload event callbacks
	 */
	private function _cleanUp() {
		$this->startRenderCallbacks = [];
		$this->endRenderCallbacks = [];
	}

	/**
	 * Return Compiler object
	 */
	public function getCompiler() {
		if (!isset($this->compiler)) {
			$this->compiler = $this->getSource()->createCompiler();
		}
		return $this->compiler;
	}

	/**
	 * Create code frame for compiled and cached templates
	 *
	 * @param string $content optional template content
	 * @param string $functions compiled template function and block code
	 * @param bool $cache flag for cache file
	 * @param Compiler\Template|null $compiler
	 *
	 * @return string
	 * @throws Exception
	 */
	public function createCodeFrame($content = '', $functions = '', $cache = false, \Smarty\Compiler\Template $compiler = null) {
		return $this->getCodeFrameCompiler()->create($content, $functions, $cache, $compiler);
	}

	/**
	 * Template data object destructor
	 */
	public function __destruct() {
		if ($this->smarty->cache_locking && $this->getCached()->is_locked) {
			$this->getCached()->handler->releaseLock($this->smarty, $this->getCached());
		}
	}

	/**
	 * Returns if the current template must be compiled by the Smarty compiler
	 * It does compare the timestamps of template source and the compiled templates and checks the force compile
	 * configuration
	 *
	 * @return bool
	 * @throws \Smarty\Exception
	 */
	public function mustCompile() {
		if (!$this->getSource()->exists) {
			if ($this->_isSubTpl()) {
				$parent_resource = " in '{$this->parent->template_resource}'";
			} else {
				$parent_resource = '';
			}
			throw new Exception("Unable to load template {$this->getSource()->type} '{$this->getSource()->name}'{$parent_resource}");
		}
		if ($this->mustCompile === null) {
			$this->mustCompile = $this->smarty->force_compile
				|| $this->getSource()->handler->recompiled
				|| !$this->getCompiled()->exists
				|| ($this->compile_check &&	$this->getCompiled()->getTimeStamp() < $this->getSource()->getTimeStamp());
		}
		return $this->mustCompile;
	}

	private function getCodeFrameCompiler(): Compiler\CodeFrame {
		return new \Smarty\Compiler\CodeFrame($this);
	}

	/**
	 * Get left delimiter
	 *
	 * @return string
	 */
	public function getLeftDelimiter()
	{
		return $this->left_delimiter ?? $this->getSmarty()->getLeftDelimiter();
	}

	/**
	 * Set left delimiter
	 *
	 * @param string $left_delimiter
	 */
	public function setLeftDelimiter($left_delimiter)
	{
		$this->left_delimiter = $left_delimiter;
	}

	/**
	 * Get right delimiter
	 *
	 * @return string $right_delimiter
	 */
	public function getRightDelimiter()
	{
		return $this->right_delimiter ?? $this->getSmarty()->getRightDelimiter();;
	}

	/**
	 * Set right delimiter
	 *
	 * @param string
	 */
	public function setRightDelimiter($right_delimiter)
	{
		$this->right_delimiter = $right_delimiter;
	}

	/**
	 * gets  a stream variable
	 *
	 * @param string                                                  $variable the stream of the variable
	 *
	 * @return mixed
	 * @throws \Smarty\Exception
	 *
	 */
	public function getStreamVariable($variable)
	{
		$_result = '';
		$fp = fopen($variable, 'r+');
		if ($fp) {
			while (!feof($fp) && ($current_line = fgets($fp)) !== false) {
				$_result .= $current_line;
			}
			fclose($fp);
			return $_result;
		}
		if ($this->getSmarty()->error_unassigned) {
			throw new Exception('Undefined stream variable "' . $variable . '"');
		}
		return null;
	}
	/**
	 * @inheritdoc
	 */
	public function configLoad($config_file, $sections = null)
	{
		$confObj = parent::configLoad($config_file, $sections);

		$this->getCompiled()->file_dependency[ $confObj->getSource()->uid ] =
			array($confObj->getSource()->filepath, $confObj->getSource()->getTimeStamp(), $confObj->getSource()->type);

		return $confObj;
	}

	public function fetch() {
		$result = $this->_execute(0);
		return $result === null ? ob_get_clean() : $result;
	}

	public function display() {
		$this->_execute(1);
	}

	/**
	 * test if cache is valid
	 *
	 * @param mixed $cache_id cache id to be used with this template
	 * @param mixed $compile_id compile id to be used with this template
	 * @param object $parent next higher level of Smarty variables
	 *
	 * @return bool cache status
	 * @throws \Exception
	 * @throws \Smarty\Exception
	 * @link https://www.smarty.net/docs/en/api.is.cached.tpl
	 *
	 * @api  Smarty::isCached()
	 */
	public function isCached(): bool {
		return (bool) $this->_execute(2);
	}

	/**
	 * fetches a rendered Smarty template
	 *
	 * @param string $function function type 0 = fetch,  1 = display, 2 = isCache
	 *
	 * @return mixed
	 * @throws Exception
	 * @throws \Throwable
	 */
	private function _execute($function) {

		$smarty = $this->getSmarty();

		// make sure we have integer values
		$this->caching = (int)$this->caching;
		// fetch template content
		$level = ob_get_level();
		try {
			$_smarty_old_error_level =
				isset($smarty->error_reporting) ? error_reporting($smarty->error_reporting) : null;

			if ($smarty->isMutingUndefinedOrNullWarnings()) {
				$errorHandler = new \Smarty\ErrorHandler();
				$errorHandler->activate();
			}

			if ($function === 2) {
				if ($this->caching) {
					// return cache status of template
					$result = $this->getCached()->isCached($this);
				} else {
					return false;
				}
			} else {

				// After rendering a template, the tpl/config variables are reset, so the template can be re-used.
				$savedTplVars = $this->tpl_vars;
				$savedConfigVars = $this->config_vars;

				// Start output-buffering. @TODO keep all ob_* calls together
				ob_start();

				$result = $this->render(false, $function);

				// Restore the template to its previous state
				$this->_cleanUp();
				$this->tpl_vars = $savedTplVars;
				$this->config_vars = $savedConfigVars;
			}

			if (isset($errorHandler)) {
				$errorHandler->deactivate();
			}

			if (isset($_smarty_old_error_level)) {
				error_reporting($_smarty_old_error_level);
			}
			return $result;
		} catch (\Throwable $e) {
			while (ob_get_level() > $level) {
				ob_end_clean();
			}
			if (isset($errorHandler)) {
				$errorHandler->deactivate();
			}

			if (isset($_smarty_old_error_level)) {
				error_reporting($_smarty_old_error_level);
			}
			throw $e;
		}
	}

	/**
	 * get rendered template content by calling compiled or cached template code
	 *
	 * @param string $unifunc function with template code
	 *
	 * @throws \Exception
	 */
	public function getRenderedTemplateCode($unifunc) {
		$level = ob_get_level();
		try {
			if (empty($unifunc) || !function_exists($unifunc)) {
				throw new \Smarty\Exception("Invalid compiled template for '{$this->template_resource}'");
			}
			if ($this->startRenderCallbacks) {
				foreach ($this->startRenderCallbacks as $callback) {
					call_user_func($callback, $this);
				}
			}
			$unifunc($this);
			foreach ($this->endRenderCallbacks as $callback) {
				call_user_func($callback, $this);
			}
			$this->isRenderingCache = false;
		} catch (\Exception $e) {
			$this->isRenderingCache = false;
			while (ob_get_level() > $level) {
				ob_end_clean();
			}
			if (isset($this->getSmarty()->security_policy)) {
				$this->getSmarty()->security_policy->endTemplate();
			}
			throw $e;
		}
	}

	/**
	 * @return Config|Source|null
	 */
	public function getSource() {
		return $this->source;
	}

	/**
	 * @param Config|Source|null $source
	 */
	public function setSource($source): void {
		$this->source = $source;
	}

	/**
	 * Sets the Cached object, so subtemplates can share one Cached object to gather meta-data.
	 *
	 * @param Cached $cached
	 *
	 * @return void
	 */
	private function setCached(Cached $cached) {
		$this->cached = $cached;
	}

}
