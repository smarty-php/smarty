<?php

/**
* Smarty Internal Plugin Resource Extends
* 
* Implements the file system as resource for Smarty which does extend a chain of template files templates
* 
* @package Smarty
* @subpackage TemplateResources
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Resource Extends
*/
class Smarty_Internal_Resource_Extends {
    public function __construct($smarty)
    {
        $this->smarty = $smarty;
    } 
    // classes used for compiling Smarty templates from file resource
    public $compiler_class = 'Smarty_Internal_SmartyTemplateCompiler';
    public $template_lexer_class = 'Smarty_Internal_Templatelexer';
    public $template_parser_class = 'Smarty_Internal_Templateparser';

    /**
    * Return flag if template source is existing
    * 
    * @param object $template template object
    * @return boolean result
    */
    public function isExisting($template)
    {
        if ($template->getTemplateFilepath() === false) {
            return false;
        } else {
            return true;
        } 
    } 
    /**
    * Get filepath to template source
    * 
    * @param object $template template object
    * @return string filepath to template source file
    */
    public function getTemplateFilepath($template)
    {
        $_files = explode('|', $template->resource_name);
        $_filepath = $template->buildTemplateFilepath ($_files[count($_files)-1]);
        if ($_filepath !== false) {
            if ($template->security) {
                $template->smarty->security_handler->isTrustedResourceDir($_filepath);
            } 
        } 
        return $_filepath;
    } 

    /**
    * Get timestamp to template source
    * 
    * @param object $template template object
    * @return integer timestamp of template source file
    */
    public function getTemplateTimestamp($template)
    {
        return filemtime($template->getTemplateFilepath());
    } 

    /**
    * Read template source from file
    * 
    * @param object $template template object
    * @return string content of template source file
    */
    public function getTemplateSource($template)
    {
        $this->template = $template;
        $_files = explode('|', $template->resource_name);
        $_files = array_reverse($_files);
        foreach ($_files as $_file) {
            $_filepath = $template->buildTemplateFilepath ($_file); 
            // read template file
            if ($_filepath === false) {
                throw new Exception("Unable to load template \"file : {$_file}\"");
            } 
            if ($_file != $_files[0]) {
                $template->properties['file_dependency']['F' . abs(crc32($_filepath))] = array($_filepath, filemtime($_filepath));
            } 
            $_content = file_get_contents($_filepath);
            if ($_file != $_files[count($_files)-1]) {
                if (preg_match_all('/(' . $this->smarty->left_delimiter . 'block(.+?)' . $this->smarty->right_delimiter . ')/', $_content, $_open, PREG_OFFSET_CAPTURE) !=
                        preg_match_all('/(' . $this->smarty->left_delimiter . '\/block(.*?)' . $this->smarty->right_delimiter . ')/', $_content, $_close, PREG_OFFSET_CAPTURE)) {
                    $this->smarty->trigger_error(" unmatched {block} {/block} pairs");
                } 
                $_block_count = count($_open[0]);
                for ($_i = 0; $_i < $_block_count; $_i++) {
                    $_block_content = str_replace($this->smarty->left_delimiter . '$smarty.parent' . $this->smarty->right_delimiter, '%%%%SMARTY_PARENT%%%%',
                        substr($_content, $_open[0][$_i][1] + strlen($_open[0][$_i][0]), $_close[0][$_i][1] - $_open[0][$_i][1] - strlen($_open[0][$_i][0])));
                    $this->saveBlockData($_block_content, $_open[0][$_i][0],$this->template);
                } 
            } else {
                $template->template_source = $_content;
                return true;
            } 
        } 
    } 
    protected function saveBlockData($block_content, $block_tag,$template)
    {
        if (0 == preg_match('/(.?)(name=)([^ ]*)/', $block_tag, $_match)) {
            $this->smarty->trigger_error("\"" . $block_tag . "\" missing name attribute");
        } else {
            // compile block content
            $_tpl = $this->smarty->createTemplate('string:' . $block_content,null,null,$template);
            $_tpl->template_filepath = $this->template->getTemplateFilepath();
            $_tpl->forceNocache= true;
            $_compiled_content = $_tpl->getCompiledTemplate();
            unset($_tpl);
            $_name = trim($_match[3], "\"'}");

            if (isset($this->smarty->block_data[$_name])) {
                if (strpos($this->smarty->block_data[$_name]['compiled'], '%%%%SMARTY_PARENT%%%%') !== false) {
                    $this->smarty->block_data[$_name]['compiled'] =
                    str_replace('%%%%SMARTY_PARENT%%%%', $_compiled_content, $this->smarty->block_data[$_name]['compiled']);
                } elseif ($this->smarty->block_data[$_name]['mode'] == 'prepend') {
                    $this->smarty->block_data[$_name]['compiled'] .= $_compiled_content;
                } elseif ($this->smarty->block_data[$_name]['mode'] == 'append') {
                    $this->smarty->block_data[$_name]['compiled'] = $_compiled_content . $this->smarty->block_data[$_name]['compiled'];
                } 
            } else {
                $this->smarty->block_data[$_name]['compiled'] = $_compiled_content;
            } 
            if (preg_match('/(.?)(append=true)(.*)/', $block_tag, $_match) != 0) {
                $this->smarty->block_data[$_name]['mode'] = 'append';
            } elseif (preg_match('/(.?)(prepend=true)(.*)/', $block_tag, $_match) != 0) {
                $this->smarty->block_data[$_name]['mode'] = 'prepend';
            } else {
                $this->smarty->block_data[$_name]['mode'] = 'replace';
            } 
        } 
    } 

    /**
    * Return flag that this resource uses the compiler
    * 
    * @return boolean true
    */
    public function usesCompiler()
    { 
        // template has tags, uses compiler
        return true;
    } 

    /**
    * Return flag that this is not evaluated
    * 
    * @return boolean false
    */
    public function isEvaluated()
    { 
        // save the compiled file to disk, do not evaluate
        return false;
    } 
    /**
    * Get filepath to compiled template
    * 
    * @param object $template template object
    * @return string return path to compiled template
    */
    public function getCompiledFilepath($template)
    {
        $_compile_id =  isset($template->compile_id) ? preg_replace('![^\w\|]+!','_',$template->compile_id) : null;
        $_files = explode('|', $template->resource_name);
        $_filepath = (string)abs(crc32($template->resource_name)); 
        // if use_sub_dirs, break file into directories
        if ($template->smarty->use_sub_dirs) {
            $_filepath = substr($_filepath, 0, 2) . DS
             . substr($_filepath, 2, 2) . DS
             . substr($_filepath, 4, 2) . DS
             . $_filepath;
        } 
        $_compile_dir_sep = $template->smarty->use_sub_dirs ? DS : '^';
        if (isset($_compile_id)) {
            $_filepath = $_compile_id . $_compile_dir_sep . $_filepath;
        } 
        if ($template->caching) {
            $_cache = '.cache';
        } else {
            $_cache = '';
        } 
        $_compile_dir = $template->smarty->compile_dir;
        if (substr($_compile_dir, -1) != DS) {
            $_compile_dir .= DS;
        } 
        return $_compile_dir . $_filepath . '.' . basename($_files[count($_files)-1]) . $_cache . '.php';
    } 
} 

?>
