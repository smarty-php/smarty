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
function compile_directory($smarty, $dir_name = null, $extention = '.tpl')
{ 
    function _get_time()
    {
        $_mtime = microtime();
        $_mtime = explode(" ", $_mtime);
        return (double)($_mtime[1]) + (double)($_mtime[0]);
    } 

    // set default directory
    if ($dir_name == null) {
        $dir_name = $smarty->template_dir;
    } 
    // switch off time limit
    if (function_exists('set_time_limit')) {
        @set_time_limit(0);
    } 

    $smarty->force_compile = true;
    $_count = 0; 
    // loop over array of template directories
    foreach ($dir_name as $_dir) {
        $_compileDirs = new RecursiveDirectoryIterator($_dir);
        $_compile = new RecursiveIteratorIterator($_compileDirs);
        foreach ($_compile as $_fileinfo) {
            if (strpos($_fileinfo, '.svn') !== false) continue;
                $_file = $_fileinfo->getFilename();
                if (!substr_compare($_file, $extention, - strlen($extention)) == 0) continue;
                echo '<br>' . $_fileinfo->getPath().'\\'.$_file;
                flush();
                $_start_time = _get_time(); 
                try {
                    $_tpl = $smarty->createTemplate($_file); 
                    $_tpl->getCompiledTemplate();
                } 
                catch (Exception $e) {
                    echo 'Error: ', $e->getMessage(), "<br><br>";
                } 
                echo ' done in  ',_get_time() - $_start_time,' seconds';
            } 
    } 
    return $_count;
} 

?>
