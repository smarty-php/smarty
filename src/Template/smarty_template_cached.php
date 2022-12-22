<?php
/**
 * Created by PhpStorm.
 * User: Uwe Tews
 * Date: 04.12.2014
 * Time: 06:08
 */

/**
 * Smarty Resource Data Object
 * Cache Data Container for Template Files
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Rodney Rehm
 */
class Smarty_Template_Cached extends Smarty_Template_Resource_Base
{
    /**
     * Cache Is Valid
     *
     * @var boolean
     */
    public $valid = null;

    /**
     * CacheResource Handler
     *
     * @var \Smarty\Cacheresource\Base
     */
    public $handler = null;

    /**
     * Template Cache Id (Smarty_Internal_Template::$cache_id)
     *
     * @var string
     */
    public $cache_id = null;

    /**
     * saved cache lifetime in seconds
     *
     * @var integer
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
     * @var Smarty_Template_Source
     */
    public $source = null;

    /**
     * Nocache hash codes of processed compiled templates
     *
     * @var array
     */
    public $hashes = array();

    /**
     * Flag if this is a cache resource
     *
     * @var bool
     */
    public $isCache = true;

    /**
     * create Cached Object container
     *
     * @param Smarty_Internal_Template $_template template object
     *
     * @throws \SmartyException
     */
    public function __construct(Smarty_Internal_Template $_template)
    {
        $this->compile_id = $_template->compile_id;
        $this->cache_id = $_template->cache_id;
        $this->source = $_template->source;
        $this->handler = \Smarty\Cacheresource\Base::load($_template->smarty);
    }

    /**
     * @param Smarty_Internal_Template $_template
     *
     * @return Smarty_Template_Cached
     */
    public static function load(Smarty_Internal_Template $_template)
    {
        $_template->cached = new Smarty_Template_Cached($_template);
        $_template->cached->handler->populate($_template->cached, $_template);
        // caching enabled ?
        if (!$_template->caching || $_template->source->handler->recompiled
        ) {
            $_template->cached->valid = false;
        }
        return $_template->cached;
    }

    /**
     * Render cache template
     *
     * @param \Smarty_Internal_Template $_template
     * @param bool                      $no_output_filter
     *
     * @throws \Exception
     */
    public function render(Smarty_Internal_Template $_template, $no_output_filter = true)
    {
        if ($this->isCached($_template)) {
            if ($_template->smarty->debugging) {
                if (!isset($_template->smarty->_debug)) {
                    $_template->smarty->_debug = new \Smarty\Debug();
                }
                $_template->smarty->_debug->start_cache($_template);
            }
            if (!$this->processed) {
                $this->process($_template);
            }
            $this->getRenderedTemplateCode($_template);
            if ($_template->smarty->debugging) {
                $_template->smarty->_debug->end_cache($_template);
            }
            return;
        } else {
	        $this->updateCache($_template, $no_output_filter);
        }
    }

    /**
     * Check if cache is valid, lock cache if required
     *
     * @param \Smarty_Internal_Template $_template
     *
     * @return bool flag true if cache is valid
     */
    public function isCached(Smarty_Internal_Template $_template)
    {
        if ($this->valid !== null) {
            return $this->valid;
        }
        while (true) {
            while (true) {
                if ($this->exists === false || $_template->smarty->force_compile || $_template->smarty->force_cache) {
                    $this->valid = false;
                } else {
                    $this->valid = true;
                }
                if ($this->valid && $_template->caching === \Smarty\Smarty::CACHING_LIFETIME_CURRENT
                    && $_template->cache_lifetime >= 0 && time() > ($this->timestamp + $_template->cache_lifetime)
                ) {
                    // lifetime expired
                    $this->valid = false;
                }
                if ($this->valid && $_template->compile_check === \Smarty\Smarty::COMPILECHECK_ON
                    && $_template->source->getTimeStamp() > $this->timestamp
                ) {
                    $this->valid = false;
                }
                if ($this->valid || !$_template->smarty->cache_locking) {
                    break;
                }
                if (!$this->handler->locked($_template->smarty, $this)) {
                    $this->handler->acquireLock($_template->smarty, $this);
                    break 2;
                }
                $this->handler->populate($this, $_template);
            }
            if ($this->valid) {
                if (!$_template->smarty->cache_locking || $this->handler->locked($_template->smarty, $this) === null) {
                    // load cache file for the following checks
                    if ($_template->smarty->debugging) {
                        $_template->smarty->_debug->start_cache($_template);
                    }
                    if ($this->handler->process($_template, $this) === false) {
                        $this->valid = false;
                    } else {
                        $this->processed = true;
                    }
                    if ($_template->smarty->debugging) {
                        $_template->smarty->_debug->end_cache($_template);
                    }
                } else {
                    $this->is_locked = true;
                    continue;
                }
            } else {
                return $this->valid;
            }
            if ($this->valid && $_template->caching === \Smarty\Smarty::CACHING_LIFETIME_SAVED
                && $_template->cached->cache_lifetime >= 0
                && (time() > ($_template->cached->timestamp + $_template->cached->cache_lifetime))
            ) {
                $this->valid = false;
            }
            if ($_template->smarty->cache_locking) {
                if (!$this->valid) {
                    $this->handler->acquireLock($_template->smarty, $this);
                } elseif ($this->is_locked) {
                    $this->handler->releaseLock($_template->smarty, $this);
                }
            }
            return $this->valid;
        }
        return $this->valid;
    }

    /**
     * Process cached template
     *
     * @param Smarty_Internal_Template $_template template object
     * @param bool                     $update    flag if called because cache update
     */
    public function process(Smarty_Internal_Template $_template, $update = false)
    {
        if ($this->handler->process($_template, $this, $update) === false) {
            $this->valid = false;
        }
        if ($this->valid) {
            $this->processed = true;
        } else {
            $this->processed = false;
        }
    }

    /**
     * Read cache content from handler
     *
     * @param Smarty_Internal_Template $_template template object
     *
     * @return string|false content
     */
    public function readCache(Smarty_Internal_Template $_template)
    {
        if (!$_template->source->handler->recompiled) {
            return $this->handler->retrieveCachedContent($_template);
        }
        return false;
    }

	/**
	 * Write this cache object to handler
	 *
	 * @param string                   $content   content to cache
	 *
	 * @return bool success
	 */
	public function writeCache(Smarty_Internal_Template $_template, $content)
	{
		if (!$_template->source->handler->recompiled) {
			if ($this->handler->storeCachedContent($_template, $content)) {
				$this->content = null;
				$this->timestamp = time();
				$this->exists = true;
				$this->valid = true;
				$this->cache_lifetime = $_template->cache_lifetime;
				$this->processed = false;
				if ($_template->smarty->cache_locking) {
					$this->handler->releaseLock($_template->smarty, $this);
				}
				return true;
			}
			$this->content = null;
			$this->timestamp = false;
			$this->exists = false;
			$this->valid = false;
			$this->processed = false;
		}
		return false;
	}

	/**
	 * Cache was invalid , so render from compiled and write to cache
	 *
	 * @param \Smarty_Template_Cached   $cached
	 * @param                           $no_output_filter
	 *
	 * @throws \Exception
	 */
	public function updateCache(Smarty_Internal_Template $_template, $no_output_filter)
	{
		ob_start();
		if (!isset($_template->compiled)) {
			$_template->loadCompiled();
		}
		$_template->compiled->render($_template);
		if ($_template->smarty->debugging) {
			$_template->smarty->_debug->start_cache($_template);
		}
		$this->removeNoCacheHash($_template, $no_output_filter);
		$compile_check = (int)$_template->compile_check;
		$_template->compile_check = \Smarty\Smarty::COMPILECHECK_OFF;
		if ($_template->_isSubTpl()) {
			$_template->compiled->unifunc = $_template->parent->compiled->unifunc;
		}
		if (!$_template->cached->processed) {
			$_template->cached->process($_template, true);
		}
		$_template->compile_check = $compile_check;
		$this->getRenderedTemplateCode($_template);
		if ($_template->smarty->debugging) {
			$_template->smarty->_debug->end_cache($_template);
		}
	}

	/**
	 * Sanitize content and write it to cache resource
	 *
	 * @param Smarty_Internal_Template $_template
	 * @param bool                     $no_output_filter
	 *
	 * @throws \SmartyException
	 */
	private function removeNoCacheHash(Smarty_Internal_Template $_template,	$no_output_filter) {
		$php_pattern = '/(<%|%>|<\?php|<\?|\?>|<script\s+language\s*=\s*[\"\']?\s*php\s*[\"\']?\s*>)/';
		$content = ob_get_clean();
		$hash_array = $this->hashes;
		$hash_array[ $_template->compiled->nocache_hash ] = true;
		$hash_array = array_keys($hash_array);
		$nocache_hash = '(' . implode('|', $hash_array) . ')';
		$_template->cached->has_nocache_code = false;
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
					if (isset($php_parts[ 0 ][ $idx_php ])) {
						$content .= "<?php echo '{$php_parts[ 1 ][ $idx_php ]}'; ?>\n";
					}
				}
			} else {
				$content .= $curr_split;
			}
			if (isset($cache_parts[ 0 ][ $curr_idx ])) {
				$_template->cached->has_nocache_code = true;
				$content .= $cache_parts[ 2 ][ $curr_idx ];
			}
		}
		if (
			!$no_output_filter
			&& !$_template->cached->has_nocache_code
			&& isset($_template->smarty->registered_filters[ 'output' ])
		) {
			$content = $_template->smarty->runFilter('output', $content, $_template);
		}
		// write cache file content
		$_template->writeCachedContent($content);
	}

}
