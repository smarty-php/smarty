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
class Smarty_Internal_Resource_Extend extends Smarty_Internal_Base {
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
        $_filepath = $_template->buildTemplateFilepath ($_files[0]);
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
                $_template->file_dependency['file_dependency'][] = array($_filepath, filemtime($_filepath));
            } 
            // read template file
            $_content = file_get_contents($_filepath);
            if ($_file != $_files[count($_files)-1]) {
                $_content = preg_replace_callback('/(' . $this->smarty->left_delimiter . 'block(.+?)' . $this->smarty->right_delimiter . ')((?:\r?\n?)(.*?)(?:\r?\n?))(' . $this->smarty->left_delimiter . '\/block(.*?)' . $this->smarty->right_delimiter . ')/is', array('Smarty_Internal_Resource_Extend', 'saveBlockData'), $_content);
            } else {
                $_template->template_source = $_content;
            } 
        } 
    } 
    protected function saveBlockData(array $matches)
    {
        if (0 == preg_match('/(.?)(name=)(.*)/', $matches[2], $_match)) {
            $this->compiler->trigger_template_error("\"" . $matches[0] . "\" missing name attribute");
        } else {
            $_name = trim($_match[3], "\"'");
            if (!isset($this->template->block_data[$_name])) {
                // check for smarty possible tags
                if (strpos($matches[3], $this->smarty->left_delimiter) === false) {
                    // output as is
                    $_output = $matches[3];
                } else {
                    // tags in $_content will be precompiled and compiled code is returnd
                    $tpl = $this->smarty->createTemplate('string:' . $matches[3]);
                    $tpl->suppressHeader = true;
                    $_output = $tpl->getCompiledTemplate();
                    $tpl->suppressHeader = false; 
                    // $_output = '<?php echo $_smarty_tpl->smarty->fetch(\'string:' . addcslashes($_content,"'") . '\', $_smarty_tpl); ? >';
                } 
                $this->template->block_data[$_name]['source'] = $matches[3];
                $this->template->block_data[$_name]['compiled'] = $_output;
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
//        $_filepath = md5($_files[0]); 
        $_filepath = (string)abs(crc32($_files[0]));
        // if use_sub_dirs, break file into directories
        if ($_template->smarty->use_sub_dirs) {
            $_filepath = substr($_filepath, 0, 3) . DIRECTORY_SEPARATOR
             . substr($_filepath, 0, 2) . DIRECTORY_SEPARATOR
             . substr($_filepath, 0, 1) . DIRECTORY_SEPARATOR
             . $_filepath;
        } 
        $_compile_dir_sep = $_template->smarty->use_sub_dirs ? DIRECTORY_SEPARATOR : '^';
        if (isset($_template->compile_id)) {
            $_filepath = $_template->compile_id . $_compile_dir_sep . $_filepath;
        } 
        if ($_template->caching) {
            $_cache = '.cache';
        } else {
            $_cache = '';
        } 
        $_compile_dir = $_template->smarty->compile_dir;
        if (substr($_compile_dir, -1) != DIRECTORY_SEPARATOR) {
            $_compile_dir .= DIRECTORY_SEPARATOR;
        } 
        return $_compile_dir . $_filepath . '.' . basename($_files[0]) . $_cache . $_template->smarty->php_ext;
    } 
} 

?>
