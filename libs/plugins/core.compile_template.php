<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * called to compile the templates
 *
 * sets $template_compiled to the compiled template
 * @param string $tpl_file
 * @param string $template_source
 * @param string $template_compiled
 * @return boolean
 */
 
//  $tpl_file, $template_source, &$template_compiled
     
function smarty_core_compile_template(&$params, &$this)
{
    if(file_exists(SMARTY_DIR . $this->compiler_file)) {
        require_once SMARTY_DIR . $this->compiler_file;            
    } else {
        // use include_path
        require_once $this->compiler_file;
    }

    $smarty_compiler = new $this->compiler_class;

    $smarty_compiler->template_dir      = $this->template_dir;
    $smarty_compiler->compile_dir       = $this->compile_dir;
    $smarty_compiler->plugins_dir       = $this->plugins_dir;
    $smarty_compiler->config_dir        = $this->config_dir;
    $smarty_compiler->force_compile     = $this->force_compile;
    $smarty_compiler->caching           = $this->caching;
    $smarty_compiler->php_handling      = $this->php_handling;
    $smarty_compiler->left_delimiter    = $this->left_delimiter;
    $smarty_compiler->right_delimiter   = $this->right_delimiter;
    $smarty_compiler->_version          = $this->_version;
    $smarty_compiler->security          = $this->security;
    $smarty_compiler->secure_dir        = $this->secure_dir;
    $smarty_compiler->security_settings = $this->security_settings;
    $smarty_compiler->trusted_dir       = $this->trusted_dir;
    $smarty_compiler->_reg_objects      = &$this->_reg_objects;
    $smarty_compiler->_plugins          = &$this->_plugins;
    $smarty_compiler->_tpl_vars         = &$this->_tpl_vars;
    $smarty_compiler->default_modifiers = $this->default_modifiers;
    $smarty_compiler->compile_id        = $this->_compile_id;

    if ($smarty_compiler->_compile_file($tpl_file, $template_source, $template_compiled)) {
        return true;
    } else {
        $this->trigger_error($smarty_compiler->_error_msg);
        return false;
    }
}


/* vim: set expandtab: */

?>
