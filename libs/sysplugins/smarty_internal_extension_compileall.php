<?php

/**
 * Smarty Extension CompileAll
 *
 * $smarty->clearCompiledTemplate() method
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Extension_CompileAll
{
    /**
     * Compile all template or config files
     *
     * @param  string $extension     template file name extension
     * @param  bool   $force_compile force all to recompile
     * @param  int    $time_limit    set maximum execution time
     * @param  int    $max_errors    set maximum allowed errors
     * @param  Smarty $smarty        Smarty instance
     * @param bool    $isConfig      flag true if called for config files
     *
     * @return int number of template files compiled
     */
    public static function compileAll($extension, $force_compile, $time_limit, $max_errors, Smarty $smarty, $isConfig = false)
    {
        // switch off time limit
        if (function_exists('set_time_limit')) {
            @set_time_limit($time_limit);
        }
        $_count = 0;
        $_error_count = 0;
        $sourceDir = $isConfig ? $smarty->getConfigDir() : $smarty->getTemplateDir();
        // loop over array of source directories
        foreach ($sourceDir as $_dir) {
            $_dir_1 = new RecursiveDirectoryIterator($_dir);
            $_dir_2 = new RecursiveIteratorIterator($_dir_1);
            foreach ($_dir_2 as $_fileinfo) {
                $_file = $_fileinfo->getFilename();
                if (substr(basename($_fileinfo->getPathname()), 0, 1) == '.' || strpos($_file, '.svn') !== false) {
                    continue;
                }
                if (!substr_compare($_file, $extension, - strlen($extension)) == 0) {
                    continue;
                }
                if ($_fileinfo->getPath() == !substr($_dir, 0, - 1)) {
                    $_file = substr($_fileinfo->getPath(), strlen($_dir)) . DS . $_file;
                }
                echo "\n<br>", $_dir, '---', $_file;
                flush();
                $_start_time = microtime(true);
                $_smarty = clone $smarty;
                $_smarty->force_compile = $force_compile;
                try {
                    $_tpl = new $smarty->template_class($_file, $_smarty);
                    $_tpl->caching = Smarty::CACHING_OFF;
                    $_tpl->source = $isConfig ? Smarty_Template_Config::load($_tpl) : Smarty_Template_Source::load($_tpl);
                    if ($_tpl->mustCompile()) {
                        $_tpl->compileTemplateSource();
                        $_count ++;
                        echo ' compiled in  ', microtime(true) - $_start_time, ' seconds';
                        flush();
                    } else {
                        echo ' is up to date';
                        flush();
                    }
                }
                catch (Exception $e) {
                    echo "\n<br>        ------>Error: ", $e->getMessage(), "<br><br>\n";
                    $_error_count ++;
                }
                // free memory
                unset($_tpl);
                $_smarty->template_objects = array();
                if ($max_errors !== null && $_error_count == $max_errors) {
                    echo "\n<br><br>too many errors\n";
                    exit();
                }
            }
        }
        echo "\n<br>";
        return $_count;
    }
}
