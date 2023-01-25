<?php

namespace Smarty\Template;

/**
 * Base class for generated PHP files, such as compiled and cached versions of templates and config files.
 * @author     Rodney Rehm
 */
abstract class GeneratedPhpFile {

	/**
	 * Compiled Filepath
	 *
	 * @var string
	 */
	public $filepath = null;

	/**
	 * Compiled Timestamp
	 *
	 * @var int|bool
	 */
	public $timestamp = false;

	/**
	 * Compiled Existence
	 *
	 * @var boolean
	 */
	public $exists = false;

	/**
	 * Template Compile Id (\Smarty\Template::$compile_id)
	 *
	 * @var string
	 */
	public $compile_id = null;

	/**
	 * Compiled Content Loaded
	 *
	 * @var boolean
	 */
	protected $processed = false;

	/**
	 * unique function name for compiled template code
	 *
	 * @var string
	 */
	public $unifunc = '';

	/**
	 * flag if template does contain nocache code sections
	 *
	 * @var bool
	 */
	private $has_nocache_code = false;

	/**
	 * resource file dependency
	 *
	 * @var array
	 */
	public $file_dependency = [];

	/**
	 * Get compiled time stamp
	 *
	 * @return int
	 */
	public function getTimeStamp() {
		if ($this->exists && !$this->timestamp) {
			$this->timestamp = filemtime($this->filepath);
		}
		return $this->timestamp;
	}

	/**
	 * @return bool
	 */
	public function getNocacheCode(): bool {
		return $this->has_nocache_code;
	}

	/**
	 * @param bool $has_nocache_code
	 */
	public function setNocacheCode(bool $has_nocache_code): void {
		$this->has_nocache_code = $has_nocache_code;
	}

	/**
	 * get rendered template content by calling compiled or cached template code
	 *
	 * @param string $unifunc function with template code
	 *
	 * @throws \Exception
	 */
	protected function getRenderedTemplateCode(\Smarty\Template $_template, $unifunc) {
		$level = ob_get_level();
		try {
			if (empty($unifunc) || !function_exists($unifunc)) {
				throw new \Smarty\Exception("Invalid compiled template for '{$this->filepath}'");
			}
			$unifunc($_template);
		} catch (\Exception $e) {
			while (ob_get_level() > $level) {
				ob_end_clean();
			}

			throw $e;
		}
	}

}
