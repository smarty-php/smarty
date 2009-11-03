<?php

/**
* Smarty plugin
* 
* @ignore 
* @package Smarty
* @subpackage plugins
*/

function  Smarty_Method_Test($smarty)
{
    echo "<PRE>\n";

    echo "Smarty Installation test...\n";

    echo "Testing template directory...\n";

    foreach((array)$smarty->template_dir as $template_dir) {
        if (!is_dir($template_dir))
            echo "FAILED: $template_dir is not a directory.\n";
        elseif (!is_readable($template_dir))
            echo "FAILED: $template_dir is not readable.\n";
        else
            echo "$template_dir is OK.\n";
    } 

    echo "Testing compile directory...\n";

    if (!is_dir($smarty->compile_dir))
        echo "FAILED: $smarty->compile_dir is not a directory.\n";
    elseif (!is_readable($smarty->compile_dir))
        echo "FAILED: $smarty->compile_dir is not readable.\n";
    elseif (!is_writable($smarty->compile_dir))
        echo "FAILED: $smarty->compile_dir is not writable.\n";
    else
        echo "{$smarty->compile_dir} is OK.\n";

    echo "Testing plugins directory...\n";

    foreach((array)$smarty->plugins_dir as $plugin_dir) {
        if (!is_dir($plugin_dir))
            echo "FAILED: $plugin_dir is not a directory.\n";
        elseif (!is_readable($plugin_dir))
            echo "FAILED: $plugin_dir is not readable.\n";
        else
            echo "$plugin_dir is OK.\n";
    } 

    echo "Testing cache directory...\n";

    if (!is_dir($smarty->cache_dir))
        echo "FAILED: $smarty->cache_dir is not a directory.\n";
    elseif (!is_readable($smarty->cache_dir))
        echo "FAILED: $smarty->cache_dir is not readable.\n";
    elseif (!is_writable($smarty->cache_dir))
        echo "FAILED: $smarty->cache_dir is not writable.\n";
    else
        echo "{$smarty->cache_dir} is OK.\n";

    echo "Testing configs directory...\n";

    if (!is_dir($smarty->config_dir))
        echo "FAILED: $smarty->config_dir is not a directory.\n";
    elseif (!is_readable($smarty->config_dir))
        echo "FAILED: $smarty->config_dir is not readable.\n";
    else
        echo "{$smarty->config_dir} is OK.\n";

    echo "Tests complete.\n";

    echo "</PRE>\n";

    return true;
} 

?>
