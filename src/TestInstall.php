<?php

namespace Smarty;

/**
 * Smarty Internal TestInstall
 * Test Smarty installation
 *
 * @package    Smarty
 * @subpackage Utilities
 * @author     Uwe Tews
 */

/**
 * TestInstall class
 *
 * @package    Smarty
 * @subpackage Utilities
 */
class TestInstall
{
    /**
     * diagnose Smarty setup
     * If $errors is secified, the diagnostic report will be appended to the array, rather than being output.
     *
     * @param \Smarty $smarty
     * @param array   $errors array to push results into rather than outputting them
     *
     * @return bool status, true if everything is fine, false else
     */
    public static function testInstall(Smarty $smarty, &$errors = null)
    {
        $status = true;
        if ($errors === null) {
            echo "<PRE>\n";
            echo "Smarty Installation test...\n";
            echo "Testing template directory...\n";
        }
        $_stream_resolve_include_path = function_exists('stream_resolve_include_path');
        // test if all registered template_dir are accessible
        foreach ($smarty->getTemplateDir() as $template_dir) {
            $_template_dir = $template_dir;
            $template_dir = realpath($template_dir);
            // resolve include_path or fail existence
            if (!$template_dir) {
                if ($smarty->use_include_path && !preg_match('/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/', $_template_dir)) {
                    // try PHP include_path
                    if ($_stream_resolve_include_path) {
                        $template_dir = stream_resolve_include_path($_template_dir);
                    } else {
                        $template_dir = $smarty->ext->_getIncludePath->getIncludePath($_template_dir, null, $smarty);
                    }
                    if ($template_dir !== false) {
                        if ($errors === null) {
                            echo "$template_dir is OK.\n";
                        }
                        continue;
                    } else {
                        $status = false;
                        $message =
                            "FAILED: $_template_dir does not exist (and couldn't be found in include_path either)";
                        if ($errors === null) {
                            echo $message . ".\n";
                        } else {
                            $errors[ 'template_dir' ] = $message;
                        }
                        continue;
                    }
                } else {
                    $status = false;
                    $message = "FAILED: $_template_dir does not exist";
                    if ($errors === null) {
                        echo $message . ".\n";
                    } else {
                        $errors[ 'template_dir' ] = $message;
                    }
                    continue;
                }
            }
            if (!is_dir($template_dir)) {
                $status = false;
                $message = "FAILED: $template_dir is not a directory";
                if ($errors === null) {
                    echo $message . ".\n";
                } else {
                    $errors[ 'template_dir' ] = $message;
                }
            } elseif (!is_readable($template_dir)) {
                $status = false;
                $message = "FAILED: $template_dir is not readable";
                if ($errors === null) {
                    echo $message . ".\n";
                } else {
                    $errors[ 'template_dir' ] = $message;
                }
            } else {
                if ($errors === null) {
                    echo "$template_dir is OK.\n";
                }
            }
        }
        if ($errors === null) {
            echo "Testing compile directory...\n";
        }
        // test if registered compile_dir is accessible
        $__compile_dir = $smarty->getCompileDir();
        $_compile_dir = realpath($__compile_dir);
        if (!$_compile_dir) {
            $status = false;
            $message = "FAILED: {$__compile_dir} does not exist";
            if ($errors === null) {
                echo $message . ".\n";
            } else {
                $errors[ 'compile_dir' ] = $message;
            }
        } elseif (!is_dir($_compile_dir)) {
            $status = false;
            $message = "FAILED: {$_compile_dir} is not a directory";
            if ($errors === null) {
                echo $message . ".\n";
            } else {
                $errors[ 'compile_dir' ] = $message;
            }
        } elseif (!is_readable($_compile_dir)) {
            $status = false;
            $message = "FAILED: {$_compile_dir} is not readable";
            if ($errors === null) {
                echo $message . ".\n";
            } else {
                $errors[ 'compile_dir' ] = $message;
            }
        } elseif (!is_writable($_compile_dir)) {
            $status = false;
            $message = "FAILED: {$_compile_dir} is not writable";
            if ($errors === null) {
                echo $message . ".\n";
            } else {
                $errors[ 'compile_dir' ] = $message;
            }
        } else {
            if ($errors === null) {
                echo "{$_compile_dir} is OK.\n";
            }
        }
        if ($errors === null) {
            echo "Testing plugins directory...\n";
        }
        // test if all registered plugins_dir are accessible
        // and if core plugins directory is still registered
        $_core_plugins_dir = realpath(__DIR__ . '/../plugins');
        $_core_plugins_available = false;
        foreach ($smarty->getPluginsDir() as $plugin_dir) {
            $_plugin_dir = $plugin_dir;
            $plugin_dir = realpath($plugin_dir);
            // resolve include_path or fail existence
            if (!$plugin_dir) {
                if ($smarty->use_include_path && !preg_match('/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/', $_plugin_dir)) {
                    // try PHP include_path
                    if ($_stream_resolve_include_path) {
                        $plugin_dir = stream_resolve_include_path($_plugin_dir);
                    } else {
                        $plugin_dir = $smarty->ext->_getIncludePath->getIncludePath($_plugin_dir, null, $smarty);
                    }
                    if ($plugin_dir !== false) {
                        if ($errors === null) {
                            echo "$plugin_dir is OK.\n";
                        }
                        continue;
                    } else {
                        $status = false;
                        $message = "FAILED: $_plugin_dir does not exist (and couldn't be found in include_path either)";
                        if ($errors === null) {
                            echo $message . ".\n";
                        } else {
                            $errors[ 'plugins_dir' ] = $message;
                        }
                        continue;
                    }
                } else {
                    $status = false;
                    $message = "FAILED: $_plugin_dir does not exist";
                    if ($errors === null) {
                        echo $message . ".\n";
                    } else {
                        $errors[ 'plugins_dir' ] = $message;
                    }
                    continue;
                }
            }
            if (!is_dir($plugin_dir)) {
                $status = false;
                $message = "FAILED: $plugin_dir is not a directory";
                if ($errors === null) {
                    echo $message . ".\n";
                } else {
                    $errors[ 'plugins_dir' ] = $message;
                }
            } elseif (!is_readable($plugin_dir)) {
                $status = false;
                $message = "FAILED: $plugin_dir is not readable";
                if ($errors === null) {
                    echo $message . ".\n";
                } else {
                    $errors[ 'plugins_dir' ] = $message;
                }
            } elseif ($_core_plugins_dir && $_core_plugins_dir == realpath($plugin_dir)) {
                $_core_plugins_available = true;
                if ($errors === null) {
                    echo "$plugin_dir is OK.\n";
                }
            } else {
                if ($errors === null) {
                    echo "$plugin_dir is OK.\n";
                }
            }
        }
        if (!$_core_plugins_available) {
            $status = false;
            $message = "WARNING: Smarty's own libs/plugins is not available";
            if ($errors === null) {
                echo $message . ".\n";
            } elseif (!isset($errors[ 'plugins_dir' ])) {
                $errors[ 'plugins_dir' ] = $message;
            }
        }
        if ($errors === null) {
            echo "Testing cache directory...\n";
        }
        // test if all registered cache_dir is accessible
        $__cache_dir = $smarty->getCacheDir();
        $_cache_dir = realpath($__cache_dir);
        if (!$_cache_dir) {
            $status = false;
            $message = "FAILED: {$__cache_dir} does not exist";
            if ($errors === null) {
                echo $message . ".\n";
            } else {
                $errors[ 'cache_dir' ] = $message;
            }
        } elseif (!is_dir($_cache_dir)) {
            $status = false;
            $message = "FAILED: {$_cache_dir} is not a directory";
            if ($errors === null) {
                echo $message . ".\n";
            } else {
                $errors[ 'cache_dir' ] = $message;
            }
        } elseif (!is_readable($_cache_dir)) {
            $status = false;
            $message = "FAILED: {$_cache_dir} is not readable";
            if ($errors === null) {
                echo $message . ".\n";
            } else {
                $errors[ 'cache_dir' ] = $message;
            }
        } elseif (!is_writable($_cache_dir)) {
            $status = false;
            $message = "FAILED: {$_cache_dir} is not writable";
            if ($errors === null) {
                echo $message . ".\n";
            } else {
                $errors[ 'cache_dir' ] = $message;
            }
        } else {
            if ($errors === null) {
                echo "{$_cache_dir} is OK.\n";
            }
        }
        if ($errors === null) {
            echo "Testing configs directory...\n";
        }
        // test if all registered config_dir are accessible
        foreach ($smarty->getConfigDir() as $config_dir) {
            $_config_dir = $config_dir;
            // resolve include_path or fail existence
            if (!$config_dir) {
                if ($smarty->use_include_path && !preg_match('/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/', $_config_dir)) {
                    // try PHP include_path
                    if ($_stream_resolve_include_path) {
                        $config_dir = stream_resolve_include_path($_config_dir);
                    } else {
                        $config_dir = $smarty->ext->_getIncludePath->getIncludePath($_config_dir, null, $smarty);
                    }
                    if ($config_dir !== false) {
                        if ($errors === null) {
                            echo "$config_dir is OK.\n";
                        }
                        continue;
                    } else {
                        $status = false;
                        $message = "FAILED: $_config_dir does not exist (and couldn't be found in include_path either)";
                        if ($errors === null) {
                            echo $message . ".\n";
                        } else {
                            $errors[ 'config_dir' ] = $message;
                        }
                        continue;
                    }
                } else {
                    $status = false;
                    $message = "FAILED: $_config_dir does not exist";
                    if ($errors === null) {
                        echo $message . ".\n";
                    } else {
                        $errors[ 'config_dir' ] = $message;
                    }
                    continue;
                }
            }
            if (!is_dir($config_dir)) {
                $status = false;
                $message = "FAILED: $config_dir is not a directory";
                if ($errors === null) {
                    echo $message . ".\n";
                } else {
                    $errors[ 'config_dir' ] = $message;
                }
            } elseif (!is_readable($config_dir)) {
                $status = false;
                $message = "FAILED: $config_dir is not readable";
                if ($errors === null) {
                    echo $message . ".\n";
                } else {
                    $errors[ 'config_dir' ] = $message;
                }
            } else {
                if ($errors === null) {
                    echo "$config_dir is OK.\n";
                }
            }
        }
        if ($errors === null) {
            echo "Tests complete.\n";
            echo "</PRE>\n";
        }
        return $status;
    }
}
