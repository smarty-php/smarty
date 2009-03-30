<?php

/**
 * Smarty Internal Plugin Resource PHP
 * 
 * Implements the file system as resource for PHP templates
 * 
 * @package Smarty
 * @subpackage TemplateResources
 * @author Uwe Tews 
 */
/**
 * Smarty Internal Plugin Resource PHP
 */
class Smarty_Internal_Resource_PHP extends Smarty_Internal_Base {
    /**
     * Class constructor, enable short open tags
     */
    public function __construct()
    {
        ini_set('short_open_tag', '1');
    } 
    /**
     * Get filepath to template source
     * 
     * @param object $_template template object
     * @return string filepath to template source file
     */
    public function getTemplateFilepath($_template)
    {
        $_filepath                                         = $_template->buildTemplateFilepath ();
        
        if ($_template->security) {
            $_template->smarty->security_handler->isTrustedResourceDir($_filepath);
        } 

        return $_filepath;
    } 

    /**
     * Get timestamp to template source
     * 
     * @param object $_template template object
     * @return integer timestamp of template source file
     */
    public function getTemplateTimestamp($_template)
    {
        return filemtime($_template->getTemplateFilepath());
    } 

    /**
     * Read template source from file
     * 
     * @param object $_template template object
     * @return string content of template source file
     */
    public function getTemplateSource($_template)
    {
        if (file_exists($_template->getTemplateFilepath())) {
            $_template->template_source = file_get_contents($_template->getTemplateFilepath());
             return true;
        } else {
            return false;
        } 
    } 

    /**
     * Return flag that this resource not use the compiler
     * 
     * @return boolean false
     */
    public function usesCompiler()
    { 
        // does not use compiler, template is PHP
        return false;
    } 

    /**
     * Return flag that this is not evaluated
     * 
     * @return boolean false
     */
    public function isEvaluated()
    { 
        // does not use compiler, must be false
        return false;
    } 

    /**
     * Get filepath to compiled template
     * 
     * @param object $_template template object
     * @return boolean return false as compiled template is not stored
     */
    public function getCompiledFilepath($_template)
    { 
        // no filepath for PHP templates
        return false;
    } 
} 

?>
