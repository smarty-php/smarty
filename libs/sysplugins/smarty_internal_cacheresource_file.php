<?php

/**
* Smarty Internal Plugin CacheResource File
* 
* Implements the file system as resource for the HTML cache
* Version ussing nocache inserts
* 
* @package Smarty
* @subpackage Cacher
* @author Uwe Tews 
*/

/**
* This class does contain all necessary methods for the HTML cache on file system
*/
class Smarty_Internal_CacheResource_File {
    function __construct($smarty)
    {
        $this->smarty = $smarty;
    } 
    /**
    * Returns the filepath of the cached template output
    * 
    * @param object $_template current template
    * @return string the cache filepath
    */
    public function getCachedFilepath($_template)
    {
        $_source_file_path = str_replace(':', '.', $_template->getTemplateFilepath());
        $_cache_id = isset($_template->cache_id) ? preg_replace('![^\w\|]+!', '_', $_template->cache_id) : null;
        $_compile_id = isset($_template->compile_id) ? preg_replace('![^\w\|]+!', '_', $_template->compile_id) : null; 
        $_filepath = $_template->templateUid; 
        // if use_sub_dirs, break file into directories
        if ($this->smarty->use_sub_dirs) {
            $_filepath = substr($_filepath, 0, 2) . DS
             . substr($_filepath, 2, 2) . DS
             . substr($_filepath, 4, 2) . DS
             . $_filepath;
        } 
        $_compile_dir_sep = $this->smarty->use_sub_dirs ? DS : '^';
        if (isset($_cache_id)) {
            $_cache_id = str_replace('|', $_compile_dir_sep, $_cache_id) . $_compile_dir_sep;
        } else {
            $_cache_id = '';
        } 
        if (isset($_compile_id)) {
            $_compile_id = $_compile_id . $_compile_dir_sep;
        } else {
            $_compile_id = '';
        } 
        $_cache_dir = $this->smarty->cache_dir;
        if (strpos('/\\', substr($_cache_dir, -1)) === false) {
            $_cache_dir .= DS;
        } 
        return $_cache_dir . $_cache_id . $_compile_id . $_filepath . '.' . basename($_source_file_path) . '.php';
    } 

    /**
    * Returns the timpestamp of the cached template output
    * 
    * @param object $_template current template
    * @return integer |booelan the template timestamp or false if the file does not exist
    */
    public function getCachedTimestamp($_template)
    { 
        // return @filemtime ($_template->getCachedFilepath());
        return ($_template->getCachedFilepath() && file_exists($_template->getCachedFilepath())) ? filemtime($_template->getCachedFilepath()) : false ;
    } 

    /**
    * Returns the cached template output
    * 
    * @param object $_template current template
    * @return string |booelan the template content or false if the file does not exist
    */
    public function getCachedContents($_template)
    {
        ob_start();
        $_smarty_tpl = $_template;
        include $_template->getCachedFilepath();
        return ob_get_clean();
    } 

    /**
    * Writes the rendered template output to cache file
    * 
    * @param object $_template current template
    * @return boolean status
    */
    public function writeCachedContent($_template, $content)
    {
        if (!$_template->resource_object->isEvaluated) {
            if (Smarty_Internal_Write_File::writeFile($_template->getCachedFilepath(), $content, $this->smarty) === true) {
                $_template->cached_timestamp = filemtime($_template->getCachedFilepath());
                return true;
            } 
        } 
        return false;
    } 

    /**
    * Empty cache folder
    * 
    * @param integer $exp_time expiration time
    * @return integer number of cache files deleted
    */
    public function clearAll($exp_time = null)
    {
        return $this->clear(null, null, null, $exp_time);
    } 
    /**
    * Empty cache for a specific template
    * 
    * @param string $resource_name template name
    * @param string $cache_id cache id
    * @param string $compile_id compile id
    * @param integer $exp_time expiration time
    * @return integer number of cache files deleted
    */
    public function clear($resource_name, $cache_id, $compile_id, $exp_time)
    {
        $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
        $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
        $_dir_sep = $this->smarty->use_sub_dirs ? '/' : '^';
        $_compile_id_offset = $this->smarty->use_sub_dirs ? 3 : 0;
        $_dir = rtrim($this->smarty->cache_dir, '/\\') . DS;
        $_dir_length = strlen($_dir);
        if (isset($_cache_id)) {
            $_cache_id_parts = explode('|', $_cache_id);
            $_cache_id_parts_count = count($_cache_id_parts);
        } 
        $_count = 0;
        $_cacheDirs = new RecursiveDirectoryIterator($_dir);
        $_cache = new RecursiveIteratorIterator($_cacheDirs, RecursiveIteratorIterator::CHILD_FIRST);
        foreach ($_cache as $_file) {
            if (strpos($_file, '.svn') !== false) continue; 
            // directory ?
            if ($_file->isDir()) {
                if (!$_cache->isDot()) {
                    // delete folder if empty
                    @rmdir($_file->getPathname());
                } 
            } else {
                $_parts = explode($_dir_sep, str_replace('\\', '/', substr((string)$_file, $_dir_length)));
                $_parts_count = count($_parts); 
                // check name
                if (isset($resource_name)) {
                    $_filename_parts = explode('.', $_parts[$_parts_count-1]);
                    $_resourcename_parts = explode('.', $resource_name . '.php');
                    if (count($_filename_parts)-1 != count($_resourcename_parts)) {
                        continue;
                    } 
                    for ($i = 0; $i < count($_resourcename_parts); $i++) {
                        if ($_filename_parts[$i + 1] != $_resourcename_parts[$i]) continue 2;
                    } 
                }
                // check compile id
                if (isset($_compile_id) && $_parts[$_parts_count-2 - $_compile_id_offset] != $_compile_id) {
                    continue;
                } 
                // check cache id
                if (isset($_cache_id)) {
                    // count of cache id parts
                    $_parts_count = (isset($_compile_id)) ? $_parts_count - 2 - $_compile_id_offset : $_parts_count - 1 - $_compile_id_offset;
                    if ($_parts_count < $_cache_id_parts_count) {
                        continue;
                    } 
                    for ($i = 0; $i < $_cache_id_parts_count; $i++) {
                        if ($_parts[$i] != $_cache_id_parts[$i]) continue 2;
                    } 
                } 
                // expired ?
                if (isset($exp_time) && time() - @filemtime($_file) < $exp_time) {
                    continue;
                } 
                $_count += @unlink((string) $_file) ? 1 : 0;
            } 
        } 
        return $_count;
    } 
} 

?>
