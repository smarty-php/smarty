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
* Smarty class Clear_Compiled_Tpl
* 
* Deletes compiled template files
*/

class Smarty_Method_Clear_Compiled_Tpl extends Smarty_Internal_Base {
    /**
    * Delete compiled template file
    * 
    * @param string $resource_name template name
    * @param string $compile_id compile id
    * @param integer $exp_time expiration time
    * @return integer number of template files deleted
    */
    public function execute($resource_name = null, $compile_id = null, $exp_time = null)
    {
        $_dir_sep = $this->smarty->use_sub_dirs ? DIRECTORY_SEPARATOR : '^';
        if (isset($resource_name)) {
            $_resource_part_1 = $resource_name . $this->smarty->php_ext;
            $_resource_part_2 = $resource_name . '.cache' . $this->smarty->php_ext;
        } else {
            $_resource_part = '';
        } 
        $_dir = $this->smarty->compile_dir;
        if ($this->smarty->use_sub_dirs && isset($compile_id)) {
            $_dir .= $compile_id . $_dir_sep;
        } 
        if (isset($compile_id)) {
            $_compile_id_part = $this->smarty->compile_dir . $compile_id . $_dir_sep;
        } 
        $_count = 0;
        $_compileDirs = new RecursiveDirectoryIterator($_dir);
        $_compile = new RecursiveIteratorIterator($_compileDirs, RecursiveIteratorIterator::CHILD_FIRST);
        foreach ($_compile as $_file) {
            if (strpos($_file,'.svn') !== false) continue;
            if ($_file->isDir()) {
                if (!$_compile->isDot()) {
                    // delete folder if empty
                    @rmdir($_file->getPathname());
                }                                         
            } else {
                if ((!isset($compile_id) || substr_compare((string)$_file, $_compile_id_part, 0, strlen($_compile_id_part)) == 0) &&
                        (!isset($resource_name) || substr_compare((string)$_file, $_resource_part_1, - strlen($_resource_part_1), strlen($_resource_part_1)) == 0 ||
                            substr_compare((string)$_file, $_resource_part_2, - strlen($_resource_part_2), strlen($_resource_part_2)) == 0)) {
                    if (isset($exp_time)) {
                        if (time() - @filemtime($_file) >= $exp_time) {
                            $_count += unlink((string) $_file) ? 1 : 0;
                        } 
                    } else {
                        $_count += unlink((string) $_file) ? 1 : 0;
                    } 
                } 
            } 
        } 
        return $_count;
    } 
} 

?>
