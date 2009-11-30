<?php

/**
* Smarty Plugin CacheResource Mysql
* 
* Implements MYSQL as resource for the HTML cache
*
*CREATE TABLE `SMARTY_CACHE` (
*  `Id` int(12) NOT NULL,
*  `CacheContents` mediumtext NOT NULL,
*  `Timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
*  `CacheId` varchar(100) NOT NULL,
*  `CompileId` varchar(100) NOT NULL,
*  `ResourceName` varchar(100) NOT NULL,
*  UNIQUE KEY `NewIndex_1` (`Id`)
*) ENGINE=MyISAM DEFAULT CHARSET=latin1* 
* @package Smarty
* @subpackage Plugins
* @author Uwe Tews 
*/

/**
* This class does contain all necessary methods for the HTML cache on MYSQL system
*/
class Smarty_CacheResource_Mysql {
    // set db host, user and pass here
    public $db_host = 'localhost';
    public $db_user = 'root';
    public $db_pass = '';
    public $db_name = 'SMARTY';

    function __construct($smarty)
    {
        $this->smarty = $smarty;
        if (!$this->link = mysql_pconnect($this->db_host, $this->db_user, $this->db_pass)) {
        throw new Exception("Cache resource unable to connect to MYSQL");
        } 
        mysql_select_db($this->db_name, $this->link);
    } 
    /**
    * Returns the filepath of the cached template output
    * 
    * @param object $template current template
    * @return string the cache filepath
    */
    public function getCachedFilepath($template)
    {
        return $this->buildCachedFilepath ($template->resource_name, $template->cache_id, $template->compile_id);
    } 

    /**
    * Returns the timpestamp of the cached template output
    * 
    * @param object $template current template
    * @return integer |booelan the template timestamp or false if the file does not exist
    */
    public function getCachedTimestamp($template)
    {
        $Id = $template->getCachedFilepath(); 
        // read cache from database
        $results = mysql_query("select UNIX_TIMESTAMP(Timestamp) from SMARTY_CACHE where Id='$Id'", $this->link);
        if (!$results) {
            $this->mysqlError();
        } 
        $row = mysql_fetch_row($results);
        return (int)$row[0];
    } 

    /**
    * Returns the cached template output
    * 
    * @param object $template current template
    * @return string |booelan the template content or false if the file does not exist
    */
    public function getCachedContents($template)
    {
        $Id = $template->getCachedFilepath(); 
        // read cache from database
        $results = mysql_query("select CacheContents from SMARTY_CACHE where Id='$Id'", $this->link);
        if (!$results) {
            $this->mysqlError();
        } 
        $row = mysql_fetch_row($results);

            $cache_content = $row[0];

        $_smarty_tpl = $template;
        ob_start();
        eval("?>" . $cache_content);
        return ob_get_clean();
    } 

    /**
    * Writes the rendered template output to cache file
    * 
    * @param object $template current template
    * @return boolean status
    */
    public function writeCachedContent($template, $content)
    {
        if (!$template->isEvaluated()) {
            $_cache_id = isset($template->cache_id) ? preg_replace('![^\w\|]+!', '_', $template->cache_id) : null;
            $_compile_id = isset($template->compile_id) ? preg_replace('![^\w\|]+!', '_', $template->compile_id) : null; 
            // save cache to database
            $Id = $template->getCachedFilepath();
            $results = mysql_query("replace into SMARTY_CACHE set Id = $Id, CacheContents = '" . addslashes($content) . "',
            CacheId = '$_cache_id',
            CompileId = '$_compile_id',
            ResourceName = '$template->resource_name'", $this->link);
            if (!$results) {
                $this->mysqlError();
            } 
            return $results;
        } else {
            return false;
        } 
    } 

    /**
    * Empty cache folder
    * 
    * @param integer $exp_time expiration time
    * @return integer number of cache files deleted
    */
    public function clearAll($exp_time = null)
    {
        if ($exp_time === null) {
            $results = mysql_query("truncate table SMARTY_CACHE", $this->link);
        } else {
            $results = mysql_query("delete ignore from SMARTY_CACHE where UNIX_TIMESTAMP(Timestamp) <= '$exp_time'", $this->link);
        } 
        if (!$results) {
            $this->mysqlError();
        } 
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

        $where = '';
        $and = '';
        if (isset($resource_name)) {
            $where = "ResourceName = '$resource_name' ";
            $and = 'and ';
        } 
        if (isset($_cache_id)) {
            $length = strlen($_cache_id);
            $where .= $and . "SUBSTRING(CacheId,1,$length) = '$_cache_id' ";
            $and = 'and ';
        } 
        if (isset($_compile_id)) {
            $where .= $and . "CompileId = '$_compile_id' ";
            $and = 'and ';
        } 
        if (isset($exp_time)) {
            $where .= $and . "UNIX_TIMESTAMP(Timestamp) <= '$exp_time' ";
        } 
        $results = mysql_query("delete ignore from SMARTY_CACHE where $where", $this->link);
        if (!$results) {
            $this->mysqlError();
        } 
        return mysql_affected_rows();
    } 

    /**
    * Get system filepath to cached file
    * 
    * @param string $resource_name template name
    * @param string $cache_id cache id
    * @param string $compile_id compile id
    * @return string filepath of cache file
    */
    private function buildCachedFilepath ($resource_name, $cache_id, $compile_id)
    {
        $_cache_id = isset($cache_id) ? preg_replace('![^\w\|]+!', '_', $cache_id) : null;
        $_compile_id = isset($compile_id) ? preg_replace('![^\w\|]+!', '_', $compile_id) : null;
        return abs(crc32($resource_name . $_cache_id . $_compile_id));
    } 

    /**
    * MYSQL Error
    */
    private function mysqlError ()
    {
        $error = mysql_error($this->link);
        throw new Exception("Cache resource MYSQL error '{$error}'");
    } 
} 

?>
