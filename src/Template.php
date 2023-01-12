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
 *
 * @property Compiled $compiled
 * @property Cached $cached
 * @property \Smarty\Compiler\Template $compiler
 */
#[\AllowDynamicProperties]
class Template extends TemplateBase {

	/**
	 * Source instance
	 *
	 * @var Source|Config
	 */
	public $source = null;

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
	 * Template Id
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
	 * @param int|null $_cache_lifetime cache life-time in seconds
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
		$_cache_lifetime = null,
		$_isConfig = false
	) {
		$this->smarty = $smarty;
		// Smarty parameter
		$this->cache_id = $_cache_id === null ? $this->smarty->cache_id : $_cache_id;
		$this->compile_id = $_compile_id === null ? $this->smarty->compile_id : $_compile_id;
		$this->caching = (int)($_caching === null ? $this->smarty->caching : $_caching);
		$this->cache_lifetime = $_cache_lifetime === null ? $this->smarty->cache_lifetime : $_cache_lifetime;
		$this->compile_check = (int)$smarty->compile_check;
		$this->parent = $_parent;
		// Template resource
		$this->template_resource = $template_resource;
		$this->source = $_isConfig ? Config::load($this) : Source::load($this);

		if ($smarty->security_policy && method_exists($smarty->security_policy, 'registerCallBacks')) {
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
		if (!$this->source->exists) {
			throw new Exception(
				"Unable to load template '{$this->source->type}:{$this->source->name}'" .
				($this->_isSubTpl() ? " in '{$this->parent->template_resource}'" : '')
			);
		}
		// disable caching for evaluated code
		if ($this->source->handler->recompiled) {
			$this->caching = \Smarty\Smarty::CACHING_OFF;
		}
		// read from cache or render
		if ($this->caching === \Smarty\Smarty::CACHING_LIFETIME_CURRENT || $this->caching === \Smarty\Smarty::CACHING_LIFETIME_SAVED) {
			if (!isset($this->cached) || $this->cached->cache_id !== $this->cache_id
				|| $this->cached->compile_id !== $this->compile_id
			) {
				$this->loadCached(true);
			}
			$this->cached->render($this, $no_output_filter);
		} else {
			if (!isset($this->compiled) || $this->compiled->compile_id !== $this->compile_id) {
				$this->loadCompiled(true);
			}
			$this->compiled->render($this);
		}
		// display or fetch
		if ($display) {
			if ($this->caching && $this->smarty->cache_modified_check) {
				$this->smarty->cacheModifiedCheck(
					$this->cached,
					$this,
					isset($content) ? $content : ob_get_clean()
				);
			} else {
				if ((!$this->caching || $this->cached->has_nocache_code || $this->source->handler->recompiled)
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
				&& (!$this->caching || $this->cached->has_nocache_code || $this->source->handler->recompiled)
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
	 * @param string $template template name
	 * @param mixed $cache_id cache id
	 * @param mixed $compile_id compile id
	 * @param integer $caching cache mode
	 * @param integer $cache_lifetime life time of cache data
	 * @param array $data passed parameter template variables
	 * @param string $uid file dependency uid
	 * @param string $content_func function name
	 *
	 * @throws \Exception
	 * @throws \Smarty\Exception
	 */
	public function _subTemplateRender(
		$template,
		$cache_id,
		$compile_id,
		$caching,
		$cache_lifetime,
		$data,
		$uid = null,
		$content_func = null
	) {
		$tpl = clone $this;
		$tpl->parent = $this;
		$smarty = &$this->smarty;
		$_templateId = $smarty->_getTemplateId($template, $cache_id, $compile_id, $caching, $tpl);
		// recursive call ?
		if ((isset($tpl->templateId) ? $tpl->templateId : $tpl->_getTemplateId()) !== $_templateId) {

			$tpl->templateId = $_templateId;
			$tpl->template_resource = $template;
			$tpl->cache_id = $cache_id;
			$tpl->compile_id = $compile_id;
			if (isset($uid)) {
				// for inline templates we can get all resource information from file dependency
				[$filepath, $timestamp, $type] = $tpl->compiled->file_dependency[$uid];
				$tpl->source = new Source($smarty, $filepath, $type, $filepath);
				$tpl->source->filepath = $filepath;
				$tpl->source->timestamp = $timestamp;
				$tpl->source->exists = true;
				$tpl->source->uid = $uid;
			} else {
				$tpl->source = Source::load($tpl);
				unset($tpl->compiled);
			}
			if ($caching !== 9999) {
				unset($tpl->cached);
			}
		} else {
			// on recursive calls force caching
			$forceTplCache = true;
		}
		$tpl->caching = $caching;
		$tpl->cache_lifetime = $cache_lifetime;

		if (!empty($data)) {
			// set up variable values
			foreach ($data as $_key => $_val) {
				$tpl->assign($_key, $_val);
			}
		}
		if ($tpl->caching === 9999) {
			if (!isset($tpl->compiled)) {
				$tpl->loadCompiled(true);
			}
			if ($tpl->compiled->has_nocache_code) {
				$this->cached->hashes[$tpl->compiled->nocache_hash] = true;
			}
		}
		if (isset($uid)) {
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
			if (isset($tpl->compiled)) {
				$tpl->compiled->render($tpl);
			} else {
				$tpl->render();
			}
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

	public function assign($tpl_var, $value = null, $nocache = false, $scope = 0) {
		return parent::assign($tpl_var, $value, $nocache || $this->isRenderingCache, $scope);
	}

	/**
	 * This function is executed automatically when a compiled or cached template file is included
	 * - Decode saved properties from compiled template and cache files
	 * - Check if compiled or cache file is valid
	 *
	 * @param \Smarty\Template $tpl
	 * @param array $properties special template properties
	 * @param bool $cache flag if called from cache file
	 *
	 * @return bool flag if compiled or cache file is valid
	 * @throws \Smarty\Exception
	 */
	public function isFresh(Template $tpl, $properties, $cache = false) {
		// on cache resources other than file check version stored in cache code
		if (!isset($properties['version']) || \Smarty\Smarty::SMARTY_VERSION !== $properties['version']) {
			if ($cache) {
				$tpl->smarty->clearAllCache();
			} else {
				$tpl->smarty->clearCompiledTemplate();
			}
			return false;
		}
		$is_valid = true;
		if (!empty($properties['file_dependency'])
			&& ((!$cache && $tpl->compile_check) || $tpl->compile_check === \Smarty\Smarty::COMPILECHECK_ON)
		) {
			// check file dependencies at compiled code
			foreach ($properties['file_dependency'] as $_file_to_check) {
				if ($_file_to_check[2] === 'file' || $_file_to_check[2] === 'php') {
					if ($tpl->source->filepath === $_file_to_check[0]) {
						// do not recheck current template
						continue;
						//$mtime = $tpl->source->getTimeStamp();
					} else {
						// file and php types can be checked without loading the respective resource handlers
						$mtime = is_file($_file_to_check[0]) ? filemtime($_file_to_check[0]) : false;
					}
				} else {
					$handler = \Smarty\Resource\BasePlugin::load($tpl->smarty, $_file_to_check[2]);
					if ($handler->checkTimestamps()) {
						$source = Source::load($tpl, $tpl->smarty, $_file_to_check[0]);
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
			if ($tpl->caching === \Smarty\Smarty::CACHING_LIFETIME_SAVED && $properties['cache_lifetime'] >= 0
				&& (time() > ($tpl->cached->timestamp + $properties['cache_lifetime']))
			) {
				$is_valid = false;
			}
			$tpl->cached->cache_lifetime = $properties['cache_lifetime'];
			$tpl->cached->valid = $is_valid;
			$generatedFile = $tpl->cached;
		} else {
			$tpl->mustCompile = !$is_valid;
			$generatedFile = $tpl->compiled;
			$generatedFile->includes = $properties['includes'] ?? [];
		}
		if ($is_valid) {
			$generatedFile->unifunc = $properties['unifunc'];
			$generatedFile->has_nocache_code = $properties['has_nocache_code'];
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
		return $this->compiled->compileTemplateSource($this);
	}

	/**
	 * Writes the content to cache resource
	 *
	 * @param string $content
	 *
	 * @return bool
	 */
	public function writeCachedContent($content) {
		if ($this->source->handler->recompiled || !$this->caching
		) {
			// don't write cache file
			return false;
		}
		if (!isset($this->cached)) {
			$this->loadCached();
		}
		$codeframe = $this->createCodeFrame($content, '', true);
		return $this->cached->writeCache($this, $codeframe);
	}

	/**
	 * Get unique template id
	 *
	 * @return string
	 * @throws \Smarty\Exception
	 */
	public function _getTemplateId() {
		return $this->templateId ?? $this->templateId =
			$this->smarty->_getTemplateId($this->template_resource, $this->cache_id, $this->compile_id);
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
	 * Load compiled object
	 *
	 * @param bool $force force new compiled object
	 */
	public function loadCompiled($force = false) {
		if ($force || !isset($this->compiled)) {
			$this->compiled = Compiled::load($this);
		}
	}

	/**
	 * Load cached object
	 *
	 * @param bool $force force new cached object
	 *
	 * @throws Exception
	 */
	public function loadCached($force = false) {
		if ($force || !isset($this->cached)) {
			$this->cached = new Cached($this);
			$this->cached->handler->populate($this->cached, $this);
			// caching enabled ?
			if (!$this->caching || $this->source->handler->recompiled) {
				$this->cached->valid = false;
			}
		}
	}

	/**
	 * Helper function for InheritanceRuntime object
	 *
	 * @return InheritanceRuntime
	 * @throws Exception
	 */
	public function getInheritance(): InheritanceRuntime {
		return $this->_getSmartyObj()->getRuntime('Inheritance');
	}

	/**
	 * Unload event callbacks
	 */
	private function _cleanUp() {
		$this->startRenderCallbacks = [];
		$this->endRenderCallbacks = [];
	}

	/**
	 * Load compiler object
	 *
	 * @throws \Smarty\Exception
	 */
	public function loadCompiler() {
		if (!isset($this->compiler)) {
			$this->compiler = $this->source->createCompiler();
		}
	}

	/**
	 * Create code frame for compiled and cached templates
	 *
	 * @param string $content optional template content
	 * @param string $functions compiled template function and block code
	 * @param bool $cache flag for cache file
	 * @param \Smarty\Compiler\Template $compiler
	 *
	 * @return string
	 */
	public function createCodeFrame($content = '', $functions = '', $cache = false, \Smarty\Compiler\Template $compiler = null) {
		return $this->getCodeFrameCompiler()->create($content, $functions, $cache, $compiler);
	}

	/**
	 * Handle unknown class methods
	 *
	 * @param string $name unknown method-name
	 * @param array $args argument array
	 *
	 * @return mixed
	 */
	public function __call($name, $args) {
		// method of Smarty object?
		if (method_exists($this->smarty, $name)) {
			return call_user_func_array([$this->smarty, $name], $args);
		}
		// parent
		return parent::__call($name, $args);
	}

	/**
	 * get Smarty property in template context
	 *
	 * @param string $property_name property name
	 *
	 * @return mixed|Cached
	 * @throws Exception
	 *
	 * @deprecated
	 * @TODO remove
	 */
	public function __get($property_name) {
		switch ($property_name) {
			case 'compiled':
				$this->loadCompiled();
				return $this->compiled;
			case 'cached':
				$this->loadCached();
				return $this->cached;
			case 'compiler':
				$this->loadCompiler();
				return $this->compiler;
			default:
				// Smarty property ?
				if (property_exists($this->smarty, $property_name)) {
					return $this->smarty->$property_name;
				}
		}
		throw new Exception("template property '$property_name' does not exist.");
	}

	/**
	 * set Smarty property in template context
	 *
	 * @param string $property_name property name
	 * @param mixed $value value
	 *
	 * @throws Exception
	 *
	 *
	 * @deprecated
	 * @TODO remove
	 */
	public function __set($property_name, $value) {
		switch ($property_name) {
			case 'compiled':
			case 'cached':
			case 'compiler':
				$this->$property_name = $value;
				return;
			default:
				// Smarty property ?
				if (property_exists($this->smarty, $property_name)) {
					$this->smarty->$property_name = $value;
					return;
				}
		}
		throw new Exception("invalid template property '$property_name'.");
	}

	/**
	 * Template data object destructor
	 */
	public function __destruct() {
		if ($this->smarty->cache_locking && isset($this->cached) && $this->cached->is_locked) {
			$this->cached->handler->releaseLock($this->smarty, $this->cached);
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
		if (!$this->source->exists) {
			if ($this->_isSubTpl()) {
				$parent_resource = " in '{$this->parent->template_resource}'";
			} else {
				$parent_resource = '';
			}
			throw new Exception("Unable to load template {$this->source->type} '{$this->source->name}'{$parent_resource}");
		}
		if ($this->mustCompile === null) {
			$this->mustCompile = $this->smarty->force_compile
				|| $this->source->handler->recompiled
				|| !$this->compiled->exists
				|| ($this->compile_check &&	$this->compiled->getTimeStamp() < $this->source->getTimeStamp());
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
		return $this->left_delimiter ?? $this->_getSmartyObj()->getLeftDelimiter();
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
		return $this->right_delimiter ?? $this->_getSmartyObj()->getRightDelimiter();;
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
		if ($this->_getSmartyObj()->error_unassigned) {
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

		$this->compiled->file_dependency[ $confObj->source->uid ] =
			array($confObj->source->filepath, $confObj->source->getTimeStamp(), $confObj->source->type);

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

		$smarty = $this->_getSmartyObj();

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
					if (!isset($this->cached)) {
						$this->loadCached();
					}
					$result = $this->cached->isCached($this);
				} else {
					return false;
				}
			} else {
				$savedTplVars = $this->tpl_vars;
				$savedConfigVars = $this->config_vars;
				ob_start();

				$result = $this->render(false, $function);
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
			if (isset($this->_getSmartyObj()->security_policy)) {
				$this->_getSmartyObj()->security_policy->endTemplate();
			}
			throw $e;
		}
	}

}
