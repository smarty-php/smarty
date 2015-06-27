<?php
/**
 * Project:     Smarty: the PHP compiling template engine
 * File:        smarty_internal_utility.php
 * SVN:         $Id: $
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * For questions, help, comments, discussion, etc., please join the
 * Smarty mailing list. Send a blank e-mail to
 * smarty-discussion-subscribe@googlegroups.com
 *
 * @link       http://www.smarty.net/
 * @copyright  2008 New Digital Group, Inc.
 * @author     Monte Ohrt <monte at ohrt dot com>
 * @author     Uwe Tews
 * @package    Smarty
 * @subpackage PluginsInternal
 * @version    3-SVN$Rev: 3286 $
 */

/**
 * Utility class
 *
 * @package    Smarty
 * @subpackage Security
 */
class Smarty_Internal_Utility
{
    /**
     * private constructor to prevent calls creation of new instances
     */
    final private function __construct()
    {
        // intentionally left blank
    }

    /**
     * Compile all template files
     *
     * @param  string $extension     template file name extension
     * @param  bool   $force_compile force all to recompile
     * @param  int    $time_limit    set maximum execution time
     * @param  int    $max_errors    set maximum allowed errors
     * @param  Smarty $smarty        Smarty instance
     *
     * @return integer number of template files compiled
     */
    public static function compileAllTemplates($extension, $force_compile, $time_limit, $max_errors, Smarty $smarty)
    {
        // switch off time limit
        if (function_exists('set_time_limit')) {
            @set_time_limit($time_limit);
        }
        $smarty->force_compile = $force_compile;
        $_count = 0;
        $_error_count = 0;
        // loop over array of template directories
        foreach ($smarty->getTemplateDir() as $_dir) {
            $_compileDirs = new RecursiveDirectoryIterator($_dir);
            $_compile = new RecursiveIteratorIterator($_compileDirs);
            foreach ($_compile as $_fileinfo) {
                $_file = $_fileinfo->getFilename();
                if (substr(basename($_fileinfo->getPathname()), 0, 1) == '.' || strpos($_file, '.svn') !== false) {
                    continue;
                }
                if (!substr_compare($_file, $extension, - strlen($extension)) == 0) {
                    continue;
                }
                if ($_fileinfo->getPath() == substr($_dir, 0, - 1)) {
                    $_template_file = $_file;
                } else {
                    $_template_file = substr($_fileinfo->getPath(), strlen($_dir)) . DS . $_file;
                }
                echo '<br>', $_dir, '---', $_template_file;
                flush();
                $_start_time = microtime(true);
                try {
                    $_tpl = $smarty->createTemplate($_template_file, null, null, null, false);
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
                    echo 'Error: ', $e->getMessage(), "<br><br>";
                    $_error_count ++;
                }
                // free memory
                $smarty->template_objects = array();
                $_tpl->smarty->template_objects = array();
                $_tpl = null;
                if ($max_errors !== null && $_error_count == $max_errors) {
                    echo '<br><br>too many errors';
                    exit();
                }
            }
        }

        return $_count;
    }

    /**
     * Compile all config files
     *
     * @param  string $extension     config file name extension
     * @param  bool   $force_compile force all to recompile
     * @param  int    $time_limit    set maximum execution time
     * @param  int    $max_errors    set maximum allowed errors
     * @param  Smarty $smarty        Smarty instance
     *
     * @return integer number of config files compiled
     */
    public static function compileAllConfig($extension, $force_compile, $time_limit, $max_errors, Smarty $smarty)
    {
        // switch off time limit
        if (function_exists('set_time_limit')) {
            @set_time_limit($time_limit);
        }
        $smarty->force_compile = $force_compile;
        $_count = 0;
        $_error_count = 0;
        // loop over array of template directories
        foreach ($smarty->getConfigDir() as $_dir) {
            $_compileDirs = new RecursiveDirectoryIterator($_dir);
            $_compile = new RecursiveIteratorIterator($_compileDirs);
            foreach ($_compile as $_fileinfo) {
                $_file = $_fileinfo->getFilename();
                if (substr(basename($_fileinfo->getPathname()), 0, 1) == '.' || strpos($_file, '.svn') !== false) {
                    continue;
                }
                if (!substr_compare($_file, $extension, - strlen($extension)) == 0) {
                    continue;
                }
                if ($_fileinfo->getPath() == substr($_dir, 0, - 1)) {
                    $_config_file = $_file;
                } else {
                    $_config_file = substr($_fileinfo->getPath(), strlen($_dir)) . DS . $_file;
                }
                echo '<br>', $_dir, '---', $_config_file;
                flush();
                $_start_time = microtime(true);
                try {
                    $_config = new Smarty_Internal_Config($_config_file, $smarty);
                    if ($_config->mustCompile()) {
                        $_config->compileConfigSource();
                        $_count ++;
                        echo ' compiled in  ', microtime(true) - $_start_time, ' seconds';
                        flush();
                    } else {
                        echo ' is up to date';
                        flush();
                    }
                }
                catch (Exception $e) {
                    echo 'Error: ', $e->getMessage(), "<br><br>";
                    $_error_count ++;
                }
                if ($max_errors !== null && $_error_count == $max_errors) {
                    echo '<br><br>too many errors';
                    exit();
                }
            }
        }

        return $_count;
    }

    /**
     * Return array of tag/attributes of all tags used by an template
     *
     * @param Smarty_Internal_Template $template
     *
     * @throws Exception
     * @throws SmartyException
     * @return array                    of tag/attributes
     */
    public static function getTags(Smarty_Internal_Template $template)
    {
        $template->smarty->get_used_tags = true;
        $template->compileTemplateSource();

        return $template->used_tags;
    }
}
