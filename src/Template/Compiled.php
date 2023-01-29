<?php

namespace Smarty\Template;

use Smarty\Exception;
use Smarty\Resource\BasePlugin;
use Smarty\Smarty;
use Smarty\Template;

/**
 * Represents a compiled version of a template or config file.
 */
class Compiled {

	/**
	 * nocache hash
	 *
	 * @var string|null
	 */
	public $nocache_hash = null;

	/**
	 * Included sub templates
	 * - index name
	 * - value use count
	 *
	 * @var int[]
	 */
	public $includes = [];

	/**
	 * @var Template
	 */
	private $template;

	/**
	 * flag if template does contain nocache code sections
	 *
	 * @var bool
	 */
	private $has_nocache_code = false;

	/**
	 * @var false|int
	 */
	private $timestamp = null;

	/**
	 * resource file dependency
	 *
	 * @var array
	 */
	public $file_dependency = [];

	/**
	 * @var null|\Smarty\CodeFrame\Compiled
	 */
	private $codeFrame = null;

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
	 * @param Template $template
	 */
	public function __construct(Template $template) {
		$this->template = $template;
	}

	/**
	 * Return compiled template code
	 *
	 * @return string
	 * @throws Exception
	 * @throws \Exception
	 */
	public function fetch(): string {
		// checks if template exists
		$source = $this->template->getSource();
		if (!$source->exists) {
			$type = $source->isConfig ? 'config' : 'template';
			throw new Exception("Unable to load $type '$source->type:$source->name'");
		}
		if ($this->template->getSmarty()->debugging) {
			$this->template->getSmarty()->getDebug()->start_render($this->template);
		}

		$codeFrame = $this->getCodeFrame();

		// @TODO Can't Cached handle this? Maybe introduce an event to decouple.
		$this->template->getCached()->file_dependency =
			array_merge($this->template->getCached()->file_dependency, $this->file_dependency);

		$level = ob_get_level();

		try {

			ob_start();

			$codeFrame->renderContent($this->template);

			// @TODO Can't Cached handle this? Maybe introduce an event to decouple and remove the $this->caching property.
			if ($this->template->caching && $this->getNocacheCode()) {
				$this->template->getCached()->hashes[$this->nocache_hash] = true;
			}

			if ($this->template->getSmarty()->debugging) {
				$this->template->getSmarty()->getDebug()->end_render($this->template);
			}
			return $this->template->getSmarty()->runOutputFilters(ob_get_clean(), $this->template);

		} catch (\Exception $e) {
			while (ob_get_level() > $level) {
				ob_end_clean();
			}
			throw $e;
		}
	}

	/**
	 * Loads compiled template (or compile from source and (usually) store the compiled version).
	 * Should only be called when code frame class doest NOT exist yet.
	 *
	 * @throws Exception
	 */
	private function getCodeFrame(): \Smarty\CodeFrame\Compiled {

		if ($this->codeFrame !== null) {
			return $this->codeFrame;
		}

		$properties = [];

		if ($this->template->getSource()->handler->recompiled) {
			$properties = eval('?>' . $this->doCompile());
		} elseif (file_exists($this->getFilePath())) {
			$properties = @include $this->getFilePath();
		}

		if (empty($properties) || !$this->isFresh($properties)) {
			$content = $this->doCompile();
			$this->write($content);
			return $this->getCodeFrame(); // recursion
		}

		return new $properties['codeFrameClass']();
	}

	/**
	 * This function is executed automatically when a generated file is included
	 * - Decode saved properties
	 * - Check if file is valid
	 *
	 * @param array $properties
	 *
	 * @return bool flag if compiled or cache file is valid
	 * @throws Exception
	 */
	public function isFresh(array $properties): bool {

		if (Smarty::SMARTY_VERSION !== $properties['version']) {
			return false;
		}

		if ($this->template->getSmarty()->getCompileCheck()) {
			if (!$this->checkFileDependencies($properties['file_dependency'])) {
				return false;
			}
		}

		return true;
	}

	/**
	 * @param array $file_dependency
	 *
	 * @return bool
	 * @throws Exception
	 */
	protected function checkFileDependencies(array $file_dependency): bool {
		// check file dependencies at compiled code
		foreach ($file_dependency as $_file_to_check) {
			if ($_file_to_check[2] === 'file') {
				if ($this->template->getSource()->filepath === $_file_to_check[0]) {
					// do not recheck current template
					continue;
				}
				// file and php types can be checked without loading the respective resource handlers
				$mtime = is_file($_file_to_check[0]) ? filemtime($_file_to_check[0]) : false;
			} else {
				$handler = BasePlugin::load($this->template->getSmarty(), $_file_to_check[2]);
				if ($handler->checkTimestamps()) {
					$source = Source::load($this->template, $this->template->getSmarty(), $_file_to_check[0]);
					$mtime = $source->getTimeStamp();
				} else {
					continue;
				}
			}
			if ($mtime === false || $mtime > $_file_to_check[1]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * compile template from source
	 *
	 * @throws Exception
	 */
	public function compileAndWrite(): string {

		$_template = $this->template;

		$filepath = $this->getFilePath();

		// compile locking
		if ($saved_timestamp = (!$_template->getSource()->handler->recompiled && is_file($filepath))) {
			$saved_timestamp = $this->getTimeStamp();
			touch($filepath);
		}
		// compile locking
		try {
			// call compiler
			$this->write($content = $this->doCompile());

		} catch (\Exception $e) {
			// restore old timestamp in case of error
			if ($saved_timestamp && is_file($filepath)) {
				touch($filepath, $saved_timestamp);
			}
			throw $e;
		}

		return $content;
	}

	/**
	 * Do the actual compiling.
	 *
	 * @return string
	 * @throws Exception
	 */
	private function doCompile(): string {
		$this->file_dependency = [];
		$this->includes = [];
		$this->nocache_hash = null;

		$level = ob_get_level();

		try {
			$result = $this->template->getCompiler()->compileTemplate($this->template);
		} catch (\Exception $e) {
			// close output buffers that were left open because of the exception
			while (ob_get_level() > $level) {
				ob_end_clean();
			}
			throw $e;
		}

		$this->timestamp = time();
		return $result;
	}

	/**
	 * Write compiled code by handler
	 *
	 * @param string $code compiled code
	 *
	 * @return void
	 * @throws Exception
	 */
	private function write(string $code) {
		if (!$this->template->getSource()->handler->recompiled) {
			$filePath = $this->getFilePath();
			if ($this->template->getSmarty()->writeFile($filePath, $code) === true) {
				$this->timestamp = is_file($filePath) ? filemtime($filePath) : false;
			}
		}
	}

	private function getFilePath(): string {

		$source = $this->template->getSource();
		$smarty = $this->template->getSmarty();

		$prefix = $smarty->getCompileDir() . $source->uid[0] . $source->uid[1];

		return $prefix . DIRECTORY_SEPARATOR . join('_', [
			$source->type,
			$source->getBasename(),
			$this->getCompiledUid()
		]) . '.php';
	}

	private function getCompiledUid(): string {
		return hash(
			PHP_VERSION_ID < 80100 ? 'sha256' : 'xxh128',
			join('_', [
				$this->template->getSource()->uid,
				$this->template->compile_id,
				$this->template->getSmarty()->escape_html ? '1' : '0',
				$this->template->caching ? '1' : '0',
			])
		);
	}

	/**
	 * Get compiled time stamp or null if there is no compiled file
	 *
	 * @return int|null
	 */
	public function getTimeStamp(): ?int {
		if ($this->timestamp === null && file_exists($this->getFilePath())) {
			$this->timestamp = filemtime($this->getFilePath());
		}
		return $this->timestamp;
	}

}
