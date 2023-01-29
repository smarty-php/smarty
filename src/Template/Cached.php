<?php

namespace Smarty\Template;

use Smarty\Exception;
use Smarty\Template;
use Smarty\Template\Cacheresource\Base;
use Smarty\Template\Compiler\CodeFrame;

/**
 * Represents a cached version of a template or config file.
 * @author     Rodney Rehm
 */
class Cached {

	/**
	 * Filepath of cached file
	 *
	 * @var string
	 */
	public $filepath = null;

	/**
	 * Cache Is Valid
	 *
	 * @var boolean
	 */
	private $valid = null;

	/**
	 * Indicates existence of cache in cacheresource
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
	 * Compiled Timestamp
	 *
	 * @var int|bool
	 */
	public $timestamp = false;

	/**
	 * CacheResource Handler
	 *
	 * @var \Smarty\Cacheresource\Base
	 */
	public $handler = null;

	/**
	 * Template Cache Id (\Smarty\Template::$cache_id)
	 *
	 * @var string
	 */
	public $cache_id = null;

	/**
	 * saved cache lifetime in seconds
	 *
	 * @var int
	 */
	public $cache_lifetime = 0;

	/**
	 * Id for cache locking
	 *
	 * @var string
	 */
	public $lock_id = null;

	/**
	 * flag that cache is locked by this instance
	 *
	 * @var bool
	 */
	public $is_locked = false;

	/**
	 * Source Object
	 *
	 * @var Source
	 */
	private $source = null;

	/**
	 * Nocache hash codes of processed compiled templates
	 *
	 * @var array
	 */
	public $hashes = [];

	/**
	 * Content buffer
	 *
	 * @var string
	 */
	public $content = null;

	/**
	 * resource file dependency
	 *
	 * @var array
	 */
	public $file_dependency = [];

	/**
	 * create Cached Object container
	 *
	 * @param Source $source
	 * @param \Smarty\Cacheresource\Base $handler
	 * @param $compile_id
	 * @param $cache_id
	 */
	public function __construct(Source $source, \Smarty\Cacheresource\Base $handler, $compile_id, $cache_id) {
		$this->compile_id = $compile_id;
		$this->cache_id = $cache_id;
		$this->source = $source;
		$this->handler = $handler;
		//@TODO we need $template here too.
	}

	/**
	 * Return cached template contents
	 *
	 * @param Template $_template
	 *
	 * @return string
	 * @throws Exception
	 */
	public function fetch(Template $_template): string {

		if (!$this->isCached($_template)) {
			$this->saveCache($_template);
		}

		if ($_template->getSmarty()->debugging) {
			$_template->getSmarty()->getDebug()->start_cache($_template);
		}

		$codeFrameClassname = $this->getCodeFrameClassname();
		if ($this->getCodeFrameClassname() && !class_exists($codeFrameClassname)) {
			$this->runCodeFrame($_template, $this->readCache($_template));
		}

		/** @var \Smarty\CodeFrame\Cached $codeFrameClassname */
		$cachedTemplate = new $codeFrameClassname();
		ob_start();

		$cachedTemplate->renderContent($_template);

		$result = ob_get_clean();

		if ($_template->getSmarty()->debugging) {
			$_template->getSmarty()->getDebug()->end_cache($_template);
		}

		return $result;
	}

	/**
	 * Render cache template
	 *
	 * @param \Smarty\Template $_template
	 *
	 * @throws \Exception
	 */
	public function render(Template $_template): void {
		echo $this->fetch($_template);
	}

	/**
	 * Returns the codeframe for this cache, but only if it is loaded already. Will not load any data from
	 * the cacheresource.
	 *
	 * @return \Smarty\CodeFrame\Cached|null
	 */
	private function getCodeFrame(): ?\Smarty\CodeFrame\Cached {
		if (!$this->getCodeFrameClassname() || !class_exists($classname = $this->getCodeFrameClassname())) {
			return null;
		}
		return new $classname;
	}

	/**
	 * Check if cache is valid, lock cache if required
	 *
	 * @param Template $_template
	 *
	 * @return bool flag true if cache is valid
	 * @throws Exception
	 */
	public function isCached(Template $_template) {
		return $this->getCodeFrame() && $this->getCodeFrame()->isFresh($_template);
	}

	/**
	 * Read cache content from handler
	 *
	 * @param Template $_template template object
	 *
	 * @return string content
s	 */
	private function readCache(Template $_template) {
		if (!$_template->getSource()->handler->recompiled) {
			return $this->handler->retrieveCachedContent($_template);
		}
		return '';
	}

	/**
	 * Write this cache object to handler
	 *
	 * @param string $content content to cache
	 *
	 * @return bool success
	 */
	private function writeCache(Template $_template, $content) {
		if (!$_template->getSource()->handler->recompiled) {
			if ($this->handler->storeCachedContent($_template, $content)) {
				$this->content = null;
				$this->timestamp = time();
				$this->exists = true;
				$this->valid = true;
				$this->cache_lifetime = $_template->cache_lifetime;
				if ($_template->getSmarty()->cache_locking) {
					$this->handler->releaseLock($_template->getSmarty(), $this);
				}
				return true;
			}
			$this->content = null;
			$this->timestamp = false;
			$this->exists = false;
			$this->valid = false;
		}
		return false;
	}

	/**
	 * Cache was invalid , so render from compiled and write to cache
	 *
	 * @param Template $_template
	 *
	 * @throws \Smarty\Exception
	 */
	private function saveCache(Template $_template) {

		$content = $_template->getCompiled()->fetch();

		if ($_template->getSmarty()->debugging) {
			$_template->getSmarty()->getDebug()->start_cache($_template);
		}

		$content = $this->removeNoCacheHash($content, $_template);

		$codeframe = (new \Smarty\Compiler\CodeFrame($_template))->create(
			$content,
			'',
			true
		);
		$this->writeCache($_template, $codeframe);

		if ($_template->_isSubTpl()) {
			// @TODO why was this needed? unifunc is no longer used, so this won't work anymore.
//			$_template->getCompiled()->unifunc = $_template->parent->getCompiled()->unifunc;
		}

		if ($_template->getSmarty()->debugging) {
			$_template->getSmarty()->getDebug()->end_cache($_template);
		}
	}

	private function getCompiledUid(): string {
		return hash(
			\PHP_VERSION_ID < 80100 ? 'sha256' : 'xxh128',
			join('_', [
				$this->getSource()->uid,
//				$this->template->compile_id, //@TODO
				$this->getSource()->getSmarty()->config_overwrite ? '1' : '0',
				$this->getSource()->getSmarty()->config_booleanize ? '1' : '0',
				$this->getSource()->getSmarty()->config_read_hidden ? '1' : '0',
				$this->getSource()->getSmarty()->caching ? '1' : '0',
			])
		);
	}

	private function getCodeFrameClassname() {
		return '__Cache_' . $this->getCompiledUid(); //@TODO use value from properties
	}

	/**
	 * Sanitize content and write it to cache resource
	 *
	 * @param string $content
	 * @param Template $_template
	 *
	 * @return string
	 */
	private function removeNoCacheHash(string $content, Template $_template): string {

		$php_pattern = '/(<%|%>|<\?php|<\?|\?>|<script\s+language\s*=\s*[\"\']?\s*php\s*[\"\']?\s*>)/';

		$hash_array = $this->hashes;
		$hash_array[$_template->getCompiled()->nocache_hash] = true;
		$hash_array = array_keys($hash_array);
		$nocache_hash = '(' . implode('|', $hash_array) . ')';

		// get text between non-cached items
		$cache_split =
			preg_split(
				"!/\*%%SmartyNocache:{$nocache_hash}%%\*\/(.+?)/\*/%%SmartyNocache:{$nocache_hash}%%\*/!s",
				$content
			);
		// get non-cached items
		preg_match_all(
			"!/\*%%SmartyNocache:{$nocache_hash}%%\*\/(.+?)/\*/%%SmartyNocache:{$nocache_hash}%%\*/!s",
			$content,
			$cache_parts
		);
		$content = '';
		// loop over items, stitch back together
		foreach ($cache_split as $curr_idx => $curr_split) {
			if (preg_match($php_pattern, $curr_split)) {
				// escape PHP tags in template content
				$php_split = preg_split(
					$php_pattern,
					$curr_split
				);
				preg_match_all(
					$php_pattern,
					$curr_split,
					$php_parts
				);
				foreach ($php_split as $idx_php => $curr_php) {
					$content .= $curr_php;
					if (isset($php_parts[0][$idx_php])) {
						$content .= "<?php echo '{$php_parts[ 1 ][ $idx_php ]}'; ?>\n";
					}
				}
			} else {
				$content .= $curr_split;
			}
			if (isset($cache_parts[0][$curr_idx])) {
				$content .= $cache_parts[2][$curr_idx];
			}
		}

		return $content;
	}

	/**
	 * @return Source|null
	 */
	public function getSource(): ?Source {
		return $this->source;
	}

	private function runCodeFrame(Template $_smarty_tpl, string $code) {


		ob_start();
		eval('?>' . $code);
		return ob_get_clean();
	}

}
