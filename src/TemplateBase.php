<?php
/**
 * Smarty Internal Plugin Smarty Template  Base
 * This file contains the basic shared methods for template handling
 *


 * @author     Uwe Tews
 */

namespace Smarty;

/**
 * Class with shared smarty/template methods
 */
abstract class TemplateBase extends Data {

	/**
	 * Set this if you want different sets of cache files for the same
	 * templates.
	 *
	 * @var string
	 */
	public $cache_id = null;

	/**
	 * Set this if you want different sets of compiled files for the same
	 * templates.
	 *
	 * @var string
	 */
	public $compile_id = null;

	/**
	 * caching enabled
	 *
	 * @var int
	 */
	public $caching = \Smarty\Smarty::CACHING_OFF;

	/**
	 * cache lifetime in seconds
	 *
	 * @var int
	 */
	public $cache_lifetime = 3600;

	/**
	 * Array of source information for known template functions
	 *
	 * @var array
	 */
	public $tplFunctions = [];

	/**
	 * When initialized to an (empty) array, this variable will hold a stack of template variables.
	 *
	 * @var null|array
	 */
	public $_var_stack = null;




	/**
	 * @param int $caching
	 */
	public function setCaching($caching) {
		$this->caching = (int)$caching;
	}

	/**
	 * @param int $cache_lifetime
	 */
	public function setCacheLifetime($cache_lifetime) {
		$this->cache_lifetime = $cache_lifetime;
	}

	/**
	 * @param string $compile_id
	 */
	public function setCompileId($compile_id) {
		$this->compile_id = $compile_id;
	}

	/**
	 * @param string $cache_id
	 */
	public function setCacheId($cache_id) {
		$this->cache_id = $cache_id;
	}



}
