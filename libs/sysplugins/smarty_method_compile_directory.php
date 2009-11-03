<?php

/**
* Smarty method compile_dir
* 
* Compiles all template files in an given directory
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Compile all template files
* 
* @param string $dir_name name of directories
* @return integer number of template files deleted
*/
function  Smarty_Method_Compile_Directory($smarty, $extention = '.tpl', $force_compile = false, $time_limit = 0, $max_errors = null)
{
    function _get_time()
    {
        $_mtime = microtime();
        $_mtime = explode(" ", $_mtime);
        return (double)($_mtime[1]) + (double)($_mtime[0]);
    } 
    // set default directory
    if ($dir_name === null) {
        $dir_name = $smarty->template_dir;
    } 
    // switch off time limit
    if (function_exists('set_time_limit')) {
        @set_time_limit($time_limit);
    } 
    $smarty->force_compile = $force_compile;
    $_count = 0;
    $_error_count = 0; 
    // loop over array of template directories
    foreach((array)$smarty->template_dir as $_dir) {
        $_compileDirs = new RecursiveDirectoryIterator($_dir);
        $_compile = new RecursiveIteratorIterator($_compileDirs);
        foreach ($_compile as $_fileinfo) {
            if (strpos($_fileinfo, '.svn') !== false) continue;
            $_file = $_fileinfo->getFilename();
            if (!substr_compare($_file, $extention, - strlen($extention)) == 0) continue;
            if ($_fileinfo->getPath() == substr($_dir, 0, -1)) {
                $_template_file = $_file;
            } else {
                $_template_file = substr($_fileinfo->getPath(), strlen($_dir)) . '\\' . $_file;
            } 
            echo '<br>', $_dir, '---', $_template_file;
            flush();
            $_start_time = _get_time();
            try {
                $_tpl = $smarty->createTemplate($_template_file);
                $_tpl->getCompiledTemplate();
            } 
            catch (Exception $e) {
                echo 'Error: ', $e->getMessage(), "<br><br>";
                $_error_count++;
            } 
            echo ' done in  ', _get_time() - $_start_time, ' seconds';
            if ($max_errors !== null && $_error_count == $max_errors) {
                echo '<br><br>too many errors';
                exit();
            } 
        } 
    } 
    return $_count;
} 

?>
