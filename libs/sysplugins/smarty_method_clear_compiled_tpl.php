<?php

/**
* Smarty method Clear_Compiled_Tpl
* 
* Deletes compiled template files
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Delete compiled template file
* 
* @param string $resource_name template name
* @param string $compile_id compile id
* @param integer $exp_time expiration time
* @return integer number of template files deleted
*/
function  Smarty_Method_Clear_Compiled_Tpl($smarty, $resource_name = null, $compile_id = null, $exp_time = null)
{
    $_compile_id =  isset($compile_id) ? preg_replace('![^\w\|]+!','_',$compile_id) : null;
    $_dir_sep = $smarty->use_sub_dirs ? DS : '^';
    if (isset($resource_name)) {
        $_resource_part_1 = $resource_name . '.php';
        $_resource_part_2 = $resource_name . '.cache' . '.php';
    } else {
        $_resource_part = '';
    } 
    $_dir = $smarty->compile_dir;
    if ($smarty->use_sub_dirs && isset($_compile_id)) {
        $_dir .= $_compile_id . $_dir_sep;
    } 
    if (isset($_compile_id)) {
        $_compile_id_part = $smarty->compile_dir . $_compile_id . $_dir_sep;
    } 
    $_count = 0;
    $_compileDirs = new RecursiveDirectoryIterator($_dir);
    $_compile = new RecursiveIteratorIterator($_compileDirs, RecursiveIteratorIterator::CHILD_FIRST);
    foreach ($_compile as $_file) {
        if (strpos($_file, '.svn') !== false) continue;
        if ($_file->isDir()) {
            if (!$_compile->isDot()) {
                // delete folder if empty
                @rmdir($_file->getPathname());
            } 
        } else {
            if ((!isset($_compile_id) || (strlen((string)$_file) > strlen($_compile_id_part) && substr_compare((string)$_file, $_compile_id_part, 0, strlen($_compile_id_part)) == 0)) &&
                    (!isset($resource_name) || (strlen((string)$_file) > strlen($_resource_part_1) && substr_compare((string)$_file, $_resource_part_1, - strlen($_resource_part_1), strlen($_resource_part_1)) == 0) ||
                        (strlen((string)$_file) > strlen($_resource_part_2) && substr_compare((string)$_file, $_resource_part_2, - strlen($_resource_part_2), strlen($_resource_part_2)) == 0))) {
                if (isset($exp_time)) {
                    if (time() - @filemtime($_file) >= $exp_time) {
                        $_count += @unlink((string) $_file) ? 1 : 0;
                    } 
                } else {
                    $_count += @unlink((string) $_file) ? 1 : 0;
                } 
            } 
        } 
    } 
    return $_count;
} 

?>
