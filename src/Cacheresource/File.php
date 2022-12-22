<?php

namespace Smarty\Cacheresource;

use Smarty\Smarty;
use Smarty_Internal_Template;
use Smarty_Template_Cached;

/**
 * Smarty Internal Plugin CacheResource File
 *
 * @package    Smarty
 * @subpackage Cacher
 * @author     Uwe Tews
 * @author     Rodney Rehm
 */

/**
 * This class does contain all necessary methods for the HTML cache on file system
 * Implements the file system as resource for the HTML cache Version ussing nocache inserts.
 *
 * @package    Smarty
 * @subpackage Cacher
 */
class File extends Base
{
    /**
     * populate Cached Object with metadata from Resource
     *
     * @param Smarty_Template_Cached   $cached    cached object
     * @param Smarty_Internal_Template $_template template object
     *
     * @return void
     */
    public function populate(Smarty_Template_Cached $cached, Smarty_Internal_Template $_template)
    {
        $source = &$_template->source;
        $smarty = &$_template->smarty;
        $_compile_dir_sep = $smarty->use_sub_dirs ? DIRECTORY_SEPARATOR : '^';
        $_filepath = sha1($source->uid . $smarty->_joined_template_dir);
        $cached->filepath = $smarty->getCacheDir();
        if (isset($_template->cache_id)) {
            $cached->filepath .= preg_replace(
                                     array(
                                         '![^\w|]+!',
                                         '![|]+!'
                                     ),
                                     array(
                                         '_',
                                         $_compile_dir_sep
                                     ),
                                     $_template->cache_id
                                 ) . $_compile_dir_sep;
        }
        if (isset($_template->compile_id)) {
            $cached->filepath .= preg_replace('![^\w]+!', '_', $_template->compile_id) . $_compile_dir_sep;
        }
        // if use_sub_dirs, break file into directories
        if ($smarty->use_sub_dirs) {
            $cached->filepath .= $_filepath[ 0 ] . $_filepath[ 1 ] . DIRECTORY_SEPARATOR . $_filepath[ 2 ] .
                                 $_filepath[ 3 ] .
                                 DIRECTORY_SEPARATOR .
                                 $_filepath[ 4 ] . $_filepath[ 5 ] . DIRECTORY_SEPARATOR;
        }
        $cached->filepath .= $_filepath;
        $basename = $source->handler->getBasename($source);
        if (!empty($basename)) {
            $cached->filepath .= '.' . $basename;
        }
        if ($smarty->cache_locking) {
            $cached->lock_id = $cached->filepath . '.lock';
        }
        $cached->filepath .= '.php';
        $cached->timestamp = $cached->exists = is_file($cached->filepath);
        if ($cached->exists) {
            $cached->timestamp = filemtime($cached->filepath);
        }
    }

    /**
     * populate Cached Object with timestamp and exists from Resource
     *
     * @param Smarty_Template_Cached $cached cached object
     *
     * @return void
     */
    public function populateTimestamp(Smarty_Template_Cached $cached)
    {
        $cached->timestamp = $cached->exists = is_file($cached->filepath);
        if ($cached->exists) {
            $cached->timestamp = filemtime($cached->filepath);
        }
    }

	/**
	 * Read the cached template and process its header
	 *
	 * @param Smarty_Internal_Template $_smarty_tpl do not change variable name, is used by compiled template
	 * @param Smarty_Template_Cached|null $cached cached object
	 * @param bool $update flag if called because cache update
	 *
	 * @return boolean true or false if the cached content does not exist
	 */
    public function process(
        Smarty_Internal_Template $_smarty_tpl,
        Smarty_Template_Cached $cached = null,
        $update = false
    ) {
        $_smarty_tpl->cached->valid = false;
        if ($update && defined('HHVM_VERSION')) {
            eval('?>' . file_get_contents($_smarty_tpl->cached->filepath));
            return true;
        } else {
            return @include $_smarty_tpl->cached->filepath;
        }
    }

    /**
     * Write the rendered template output to cache
     *
     * @param Smarty_Internal_Template $_template template object
     * @param string                   $content   content to cache
     *
     * @return bool success
     * @throws \SmartyException
     */
    public function storeCachedContent(Smarty_Internal_Template $_template, $content)
    {
        if ($_template->smarty->writeFile($_template->cached->filepath, $content) === true) {
            if (function_exists('opcache_invalidate')
                && (!function_exists('ini_get') || strlen(ini_get('opcache.restrict_api'))) < 1
            ) {
                opcache_invalidate($_template->cached->filepath, true);
            } elseif (function_exists('apc_compile_file')) {
                apc_compile_file($_template->cached->filepath);
            }
            $cached = $_template->cached;
            $cached->timestamp = $cached->exists = is_file($cached->filepath);
            if ($cached->exists) {
                $cached->timestamp = filemtime($cached->filepath);
                return true;
            }
        }
        return false;
    }

    /**
     * Read cached template from cache
     *
     * @param Smarty_Internal_Template $_template template object
     *
     * @return string  content
     */
    public function retrieveCachedContent(Smarty_Internal_Template $_template)
    {
        if (is_file($_template->cached->filepath)) {
            return file_get_contents($_template->cached->filepath);
        }
        return false;
    }

    /**
     * Empty cache
     *
     * @param Smarty  $smarty
     * @param integer $exp_time expiration time (number of seconds, not timestamp)
     *
     * @return integer number of cache files deleted
     */
    public function clearAll(Smarty $smarty, $exp_time = null)
    {
        return $this->clear($smarty, null, null, null, $exp_time);
    }

    /**
     * Empty cache for a specific template
     *
     * @param Smarty  $smarty
     * @param string  $resource_name template name
     * @param string  $cache_id      cache id
     * @param string  $compile_id    compile id
     * @param integer $exp_time      expiration time (number of seconds, not timestamp)
     *
     * @return integer number of cache files deleted
     */
    public function clear(Smarty $smarty, $resource_name, $cache_id, $compile_id, $exp_time)
    {
	    $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
	    $_compile_id = isset($compile_id) ? preg_replace('![^\w]+!', '_', $compile_id) : null;
	    $_dir_sep = $smarty->use_sub_dirs ? '/' : '^';
	    $_compile_id_offset = $smarty->use_sub_dirs ? 3 : 0;
	    $_dir = $smarty->getCacheDir();
	    if ($_dir === '/') { //We should never want to delete this!
		    return 0;
	    }
	    $_dir_length = strlen($_dir);
	    if (isset($_cache_id)) {
		    $_cache_id_parts = explode('|', $_cache_id);
		    $_cache_id_parts_count = count($_cache_id_parts);
		    if ($smarty->use_sub_dirs) {
			    foreach ($_cache_id_parts as $id_part) {
				    $_dir .= $id_part . '/';
			    }
		    }
	    }
	    if (isset($resource_name)) {
		    $_save_stat = $smarty->caching;
		    $smarty->caching = \Smarty\Smarty::CACHING_LIFETIME_CURRENT;
		    $tpl = new $smarty->template_class($resource_name, $smarty);
		    $smarty->caching = $_save_stat;
		    // remove from template cache
		    $tpl->source; // have the template registered before unset()
		    if ($tpl->source->exists) {
			    $_resourcename_parts = basename(str_replace('^', '/', $tpl->cached->filepath));
		    } else {
			    return 0;
		    }
	    }
	    $_count = 0;
	    $_time = time();
	    if (file_exists($_dir)) {
		    $_cacheDirs = new RecursiveDirectoryIterator($_dir);
		    $_cache = new RecursiveIteratorIterator($_cacheDirs, RecursiveIteratorIterator::CHILD_FIRST);
		    foreach ($_cache as $_file) {
			    if (substr(basename($_file->getPathname()), 0, 1) === '.') {
				    continue;
			    }
			    $_filepath = (string)$_file;
			    // directory ?
			    if ($_file->isDir()) {
				    if (!$_cache->isDot()) {
					    // delete folder if empty
					    @rmdir($_file->getPathname());
				    }
			    } else {
				    // delete only php files
				    if (substr($_filepath, -4) !== '.php') {
					    continue;
				    }
				    $_parts = explode($_dir_sep, str_replace('\\', '/', substr($_filepath, $_dir_length)));
				    $_parts_count = count($_parts);
				    // check name
				    if (isset($resource_name)) {
					    if ($_parts[ $_parts_count - 1 ] !== $_resourcename_parts) {
						    continue;
					    }
				    }
				    // check compile id
				    if (isset($_compile_id) && (!isset($_parts[ $_parts_count - 2 - $_compile_id_offset ])
						    || $_parts[ $_parts_count - 2 - $_compile_id_offset ] !== $_compile_id)
				    ) {
					    continue;
				    }
				    // check cache id
				    if (isset($_cache_id)) {
					    // count of cache id parts
					    $_parts_count = (isset($_compile_id)) ? $_parts_count - 2 - $_compile_id_offset :
						    $_parts_count - 1 - $_compile_id_offset;
					    if ($_parts_count < $_cache_id_parts_count) {
						    continue;
					    }
					    for ($i = 0; $i < $_cache_id_parts_count; $i++) {
						    if ($_parts[ $i ] !== $_cache_id_parts[ $i ]) {
							    continue 2;
						    }
					    }
				    }
				    if (is_file($_filepath)) {
					    // expired ?
					    if (isset($exp_time)) {
						    if ($exp_time < 0) {
							    preg_match('#\'cache_lifetime\' =>\s*(\d*)#', file_get_contents($_filepath), $match);
							    if ($_time < (filemtime($_filepath) + $match[ 1 ])) {
								    continue;
							    }
						    } else {
							    if ($_time - filemtime($_filepath) < $exp_time) {
								    continue;
							    }
						    }
					    }
					    $_count += @unlink($_filepath) ? 1 : 0;
					    if (function_exists('opcache_invalidate')
						    && (!function_exists('ini_get') || strlen(ini_get("opcache.restrict_api")) < 1)
					    ) {
						    opcache_invalidate($_filepath, true);
					    } elseif (function_exists('apc_delete_file')) {
						    apc_delete_file($_filepath);
					    }
				    }
			    }
		    }
	    }
	    return $_count;
    }

    /**
     * Check is cache is locked for this template
     *
     * @param Smarty                 $smarty Smarty object
     * @param Smarty_Template_Cached $cached cached object
     *
     * @return boolean true or false if cache is locked
     */
    public function hasLock(Smarty $smarty, Smarty_Template_Cached $cached)
    {
        clearstatcache(true, $cached->lock_id ?? '');
        if (null !== $cached->lock_id && is_file($cached->lock_id)) {
            $t = filemtime($cached->lock_id);
            return $t && (time() - $t < $smarty->locking_timeout);
        } else {
            return false;
        }
    }

    /**
     * Lock cache for this template
     *
     * @param Smarty                 $smarty Smarty object
     * @param Smarty_Template_Cached $cached cached object
     *
     * @return void
     */
    public function acquireLock(Smarty $smarty, Smarty_Template_Cached $cached)
    {
        $cached->is_locked = true;
        touch($cached->lock_id);
    }

    /**
     * Unlock cache for this template
     *
     * @param Smarty                 $smarty Smarty object
     * @param Smarty_Template_Cached $cached cached object
     *
     * @return void
     */
    public function releaseLock(Smarty $smarty, Smarty_Template_Cached $cached)
    {
        $cached->is_locked = false;
        @unlink($cached->lock_id);
    }
}
