<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * create full directory structure
 *
 * @param string $dir
 */

// $dir
 
function smarty_core_create_dir_structure($params, &$this)
{
    if (!file_exists($params['dir'])) {           
        $_new_dir = (preg_match("/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/", $params['dir']))
            ? DIRECTORY_SEPARATOR : getcwd().DIRECTORY_SEPARATOR;

        $_dir_parts = preg_split('!\\' . DIRECTORY_SEPARATOR . '+!', $params['dir'], -1, PREG_SPLIT_NO_EMPTY);

        // do not attempt to test or make directories outside of open_basedir
        $_open_basedir_ini = ini_get('open_basedir');
        if(!empty($_open_basedir_ini)) {
            $_use_open_basedir = true;
            $_open_basedir_sep = (strtoupper(substr(PHP_OS, 0, 3)) == 'WIN') ? ';' : ':';
            $_open_basedirs = explode($_open_basedir_sep, $_open_basedir_ini);
        } else {                    
            $_use_open_basedir = false;
        }

        foreach ($_dir_parts as $_dir_part) {
            $_new_dir .= $_dir_part;
            if ($_use_open_basedir) {
                $_make_new_dir = false;
                foreach ($_open_basedirs as $_open_basedir) {
                    if (substr($_new_dir.'/', 0, strlen($_open_basedir)) == $_open_basedir) {
                        $_make_new_dir = true;
                        break;
                    }
                }
            } else {
                $_make_new_dir = true;                    
            }

            if ($_make_new_dir && !file_exists($_new_dir) && !@mkdir($_new_dir, $this->_dir_perms)) {
                $this->trigger_error("problem creating directory '" . $params['dir'] . "'");
                return false;
            }
            $_new_dir .= DIRECTORY_SEPARATOR;
        }
    }
}

/* vim: set expandtab: */

?>
