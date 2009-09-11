<?php

/**
* Smarty Internal Plugin Resource Extend
* 
* Implements the file system as resource for Smarty which does extend a chain of template files templates
* 
* @package Smarty
* @subpackage TemplateResources
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Resource Extend
*/
class Smarty_Internal_Resource_Extend {
    public function __construct($smarty)
    {
        $this->smarty = $smarty;
    } 
    // classes used for compiling Smarty templates from file resource
    public $compiler_class = 'Smarty_Internal_SmartyTemplateCompiler';
    public $template_lexer_class = 'Smarty_Internal_Templatelexer';
    public $template_parser_class = 'Smarty_Internal_Templateparser';

    /**
    * Get filepath to template source
    * 
    * @param object $_template template object
    * @return string filepath to template source file
    */
    public function getTemplateFilepath($_template)
    {
        $_files = explode('|', $_template->resource_name);
        $_filepath = $_template->buildTemplateFilepath ($_files[count($_files)-1]);
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
        $this->template = $_template;
        $_files = explode('|', $_template->resource_name);
        $_files = array_reverse($_files);
        foreach ($_files as $_file) {
            $_filepath = $_template->buildTemplateFilepath ($_file);
            if ($_file != $_files[0]) {
                $_template->properties['file_dependency'][] = array($_filepath, filemtime($_filepath));
            } 
            // read template file
            $_content = file_get_contents($_filepath);
            if ($_file != $_files[count($_files)-1]) {
                if (preg_match_all('/(' . $this->smarty->left_delimiter . 'block(.+?)' . $this->smarty->right_delimiter . ')/', $_content, $s, PREG_OFFSET_CAPTURE) !=
                        preg_match_all('/(' . $this->smarty->left_delimiter . '\/block(.*?)' . $this->smarty->right_delimiter . ')/', $_content, $c, PREG_OFFSET_CAPTURE)) {
                    $this->smarty->trigger_error(" unmatched {block} {/block} pairs");
                } 
                $block_count = count($s[0]);
                for ($i = 0; $i < $block_count; $i++) {
                    $block_content = substr($_content, $s[0][$i][1] + strlen($s[0][$i][0]), $c[0][$i][1] - $s[0][$i][1] - strlen($s[0][$i][0]));
                    $this->saveBlockData($block_content, $s[0][$i][0]);
                } 
            } else {
                $_template->template_source = $_content;
                return true;
            } 
        } 
    } 
    protected function saveBlockData($block_content, $block_tag)
    {
        if (0 == preg_match('/(.?)(name=)([^ ]*)/', $block_tag, $_match)) {
            $this->smarty->trigger_error("\"" . $block_tag . "\" missing name attribute");
        } else {
            // compile block content
            $_tpl = $this->smarty->createTemplate('string:' . $block_content);
            $_tpl->template_filepath = $this->template->getTemplateFilepath();
            $_tpl->suppressHeader = true;
            $_compiled_content = $_tpl->getCompiledTemplate();
            unset($_tpl);
            $_name = trim($_match[3], "\"'}");

            if (isset($this->template->block_data[$_name])) {
                if ($this->template->block_data[$_name]['mode'] == 'prepend') {
                    $this->template->block_data[$_name]['compiled'] .= $_compiled_content;
                    $this->template->block_data[$_name]['source'] .= $block_content;
                } elseif ($this->template->block_data[$_name]['mode'] == 'append') {
                    $this->template->block_data[$_name]['compiled'] = $_compiled_content . $this->template->block_data[$_name]['compiled'];
                    $this->template->block_data[$_name]['source'] = $block_content . $this->template->block_data[$_name]['source'];
                } 
            } else {
                $this->template->block_data[$_name]['compiled'] = $_compiled_content;
                $this->template->block_data[$_name]['source'] = $block_content;
            } 
            if (preg_match('/(.?)(append=true)(.*)/', $block_tag, $_match) != 0) {
                $this->template->block_data[$_name]['mode'] = 'append';
            } elseif (preg_match('/(.?)(prepend=true)(.*)/', $block_tag, $_match) != 0) {
                $this->template->block_data[$_name]['mode'] = 'prepend';
            } else {
                $this->template->block_data[$_name]['mode'] = 'replace';
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
    * @param object $_template template object
    * @return string return path to compiled template
    */
    public function getCompiledFilepath($_template)
    {
        $_files = explode('|', $_template->resource_name);
        $_filepath = (string)abs(crc32($_template->resource_name)); 
        // if use_sub_dirs, break file into directories
        if ($_template->smarty->use_sub_dirs) {
            $_filepath = substr($_filepath, 0, 3) . DS
             . substr($_filepath, 0, 2) . DS
             . substr($_filepath, 0, 1) . DS
             . $_filepath;
        } 
        $_compile_dir_sep = $_template->smarty->use_sub_dirs ? DS : '^';
        if (isset($_template->compile_id)) {
            $_filepath = $_template->compile_id . $_compile_dir_sep . $_filepath;
        } 
        if ($_template->caching) {
            $_cache = '.cache';
        } else {
            $_cache = '';
        } 
        $_compile_dir = $_template->smarty->compile_dir;
        if (substr($_compile_dir, -1) != DS) {
            $_compile_dir .= DS;
        } 
        return $_compile_dir . $_filepath . '.' . basename($_files[count($_files)-1]) . $_cache . $_template->smarty->php_ext;
    } 
} 

?>
