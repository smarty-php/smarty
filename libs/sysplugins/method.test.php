<?php

/**
 * Smarty plugin
* @ignore 
 * @package Smarty
 * @subpackage plugins
 */

class Smarty_Method_Test extends Smarty_Internal_Base {

  public function execute() {

    echo "<PRE>\n";
  
    echo "Smarty Installation test...\n";
    
    echo "Testing template directory...\n";
    
    foreach((array)$this->smarty->template_dir as $template_dir)
    {
      if(!is_dir($template_dir))
        echo "FAILED: $template_dir is not a directory.\n";
      elseif(!is_readable($template_dir))
        echo "FAILED: $template_dir is not readable.\n";
      else  
        echo "$template_dir is OK.\n";
    }

    echo "Testing compile directory...\n";
    
    if(!is_dir($this->smarty->compile_dir))
      echo "FAILED: $compile_dir is not a directory.\n";
    elseif(!is_readable($this->smarty->compile_dir))
      echo "FAILED: $compile_dir is not readable.\n";
    elseif(!is_writable($this->smarty->compile_dir))
      echo "FAILED: $compile_dir is not writable.\n";
    else
      echo "{$this->smarty->compile_dir} is OK.\n";


    echo "Testing sysplugins directory...\n";
    
    if(!is_dir($this->smarty->sysplugins_dir))
      echo "FAILED: $sysplugins_dir is not a directory.\n";
    elseif(!is_readable($this->smarty->sysplugins_dir))
      echo "FAILED: $sysplugins_dir is not readable.\n";
    else
      echo "{$this->smarty->sysplugins_dir} is OK.\n";
  
    echo "Testing plugins directory...\n";
    
    foreach((array)$this->smarty->plugins_dir as $plugin_dir)
    {
      if(!is_dir($plugin_dir))
        echo "FAILED: $plugin_dir is not a directory.\n";
      elseif(!is_readable($plugin_dir))
        echo "FAILED: $plugin_dir is not readable.\n";
      else  
        echo "$plugin_dir is OK.\n";
    }
  
    echo "Testing cache directory...\n";
    
    if(!is_dir($this->smarty->cache_dir))
      echo "FAILED: $cache_dir is not a directory.\n";
    elseif(!is_readable($this->smarty->cache_dir))
      echo "FAILED: $cache_dir is not readable.\n";
    elseif(!is_writable($this->smarty->cache_dir))
      echo "FAILED: $cache_dir is not writable.\n";
    else
      echo "{$this->smarty->cache_dir} is OK.\n";
  
    echo "Testing configs directory...\n";
    
    if(!is_dir($this->smarty->config_dir))
      echo "FAILED: $config_dir is not a directory.\n";
    elseif(!is_readable($this->smarty->config_dir))
      echo "FAILED: $config_dir is not readable.\n";
    else
      echo "{$this->smarty->config_dir} is OK.\n";
  
    echo "Tests complete.\n";
  
    echo "</PRE>\n";

    return true;  

  }

}

?>
