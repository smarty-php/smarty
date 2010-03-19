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
        $this->_rdl = preg_quote($smarty->right_delimiter);
        $this->_ldl = preg_quote($smarty->left_delimiter);
    } 
    // classes used for compiling Smarty templates from file resource
    public $compiler_class = 'Smarty_Internal_SmartyTemplateCompiler';
    public $template_lexer_class = 'Smarty_Internal_Templatelexer';
    public $template_parser_class = 'Smarty_Internal_Templateparser'; 
    // properties
    public $usesCompiler = true;
    public $isEvaluated = false;
    public $allFilepaths = array();

    /**
     * Return flag if template source is existing
     * 
     * @param object $_template template object
     * @return boolean result
     */
    public function isExisting($_template)
    {
        if ($_template->getTemplateFilepath() === false) {
            return false;
        } else {
            return true;
        } 
    } 
    /**
     * Get filepath to template source
     * 
     * @param object $_template template object
     * @return string filepath to template source file
     */
    public function getTemplateFilepath($_template)
    {
        $sha1String = '';
        $_files = explode('|', $_template->resource_name);
        foreach ($_files as $_file) {
            $_filepath = $_template->buildTemplateFilepath ($_file);
            if ($_filepath !== false) {
                if ($_template->security) {
                    $_template->smarty->security_handler->isTrustedResourceDir($_filepath);
                } 
            } 
            $sha1String .= $_filepath;
            $this->allFilepaths[] = $_filepath;
        } 
        $_template->templateUid = sha1($sha1String);
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
        $_files = array_reverse($this->allFilepaths);
        foreach ($_files as $_filepath) {
            // read template file
            if ($_filepath === false) {
                throw new Exception("Unable to load template 'file : {$_file}'");
            } 
            if ($_filepath != $_files[0]) {
                $_template->properties['file_dependency'][sha1($_filepath)] = array($_filepath, filemtime($_filepath));
            } 
            $_template->template_filepath = $_filepath;
            $_content = file_get_contents($_filepath);
            if ($_filepath != $_files[count($_files)-1]) {
                if (preg_match_all("!({$this->_ldl}block(.+?){$this->_rdl})!", $_content, $_open) !=
                        preg_match_all("!({$this->_ldl}/block(.*?){$this->_rdl})!", $_content, $_close)) {
                    $this->smarty->trigger_error("unmatched {block} {/block} pairs in file '$_filepath'");
                } 
                preg_match_all("!{$this->_ldl}block(.+?){$this->_rdl}|{$this->_ldl}/block(.*?){$this->_rdl}!", $_content, $_result, PREG_OFFSET_CAPTURE);
                $_result_count = count($_result[0]);
                $_start = 0;
                while ($_start < $_result_count) {
                    $_end = 0;
                    $_level = 1;
                    while ($_level != 0) {
                        $_end++;
                        if (!strpos($_result[0][$_start + $_end][0], '/')) {
                            $_level++;
                        } else {
                            $_level--;
                        } 
                    } 
                    $_block_content = str_replace($this->smarty->left_delimiter . '$smarty.block.parent' . $this->smarty->right_delimiter, '%%%%SMARTY_PARENT%%%%',
                        substr($_content, $_result[0][$_start][1] + strlen($_result[0][$_start][0]), $_result[0][$_start + $_end][1] - $_result[0][$_start][1] - + strlen($_result[0][$_start][0])));
                    $this->saveBlockData($_block_content, $_result[0][$_start][0], $_filepath);
                    $_start = $_start + $_end + 1;
                } 
            } else {
                $_template->template_source = $_content;
                return true;
            } 
        } 
        // $_template->template_filepath = $saved_filepath;
    } 
    protected function saveBlockData($block_content, $block_tag, $_filepath)
    {
        if (0 == preg_match("!(.?)(name=)(.*?)(?=(\s|{$this->_rdl}))!", $block_tag, $_match)) {
            $this->smarty->trigger_error("'{$block_tag}' missing name attribute in file '$_filepath'");
        } else {
            $_name = trim($_match[3], '\'"');
	   // replace {$smarty.block.child} 
            if (strpos($block_content, $this->smarty->left_delimiter . '$smarty.block.child' . $this->smarty->right_delimiter) !== false) {
                if (isset($this->smarty->block_data[$_name])) {
                    $block_content = str_replace($this->smarty->left_delimiter . '$smarty.block.child' . $this->smarty->right_delimiter,
                        $this->smarty->block_data[$_name]['source'], $block_content);
                    unset($this->smarty->block_data[$_name]);
                } else {
                    $block_content = str_replace($this->smarty->left_delimiter . '$smarty.block.child' . $this->smarty->right_delimiter,
                        '', $block_content);
                } 
            } 
            if (isset($this->smarty->block_data[$_name])) {
                if (strpos($this->smarty->block_data[$_name]['source'], '%%%%SMARTY_PARENT%%%%') !== false) {
                    $this->smarty->block_data[$_name]['source'] =
                    str_replace('%%%%SMARTY_PARENT%%%%', $block_content, $this->smarty->block_data[$_name]['source']);
                } elseif ($this->smarty->block_data[$_name]['mode'] == 'prepend') {
                    $this->smarty->block_data[$_name]['source'] .= $block_content;
                } elseif ($this->smarty->block_data[$_name]['mode'] == 'append') {
                    $this->smarty->block_data[$_name]['source'] = $block_content . $this->smarty->block_data[$_name]['source'];
                } 
            } else {
                $this->smarty->block_data[$_name]['source'] = $block_content;
            } 
            if (preg_match('/(.?)(append)(.*)/', $block_tag, $_match) != 0) {
                $this->smarty->block_data[$_name]['mode'] = 'append';
            } elseif (preg_match('/(.?)(prepend)(.*)/', $block_tag, $_match) != 0) {
                $this->smarty->block_data[$_name]['mode'] = 'prepend';
            } else {
                $this->smarty->block_data[$_name]['mode'] = 'replace';
            } 
            $this->smarty->block_data[$_name]['file'] = $_filepath;
        } 
    } 

    /**
     * Get filepath to compiled template
     * 
     * @param object $_template template object
     * @return string return path to compiled template
     */
    public function getCompiledFilepath($_template)
    {
        $_compile_id = isset($_template->compile_id) ? preg_replace('![^\w\|]+!', '_', $_template->compile_id) : null;
        $_files = explode('|', $_template->resource_name); 
        // calculate Uid if not already done
        if ($_template->templateUid == '') {
            $_template->getTemplateFilepath();
        } 
        $_filepath = $_template->templateUid; 
        // if use_sub_dirs, break file into directories
        if ($_template->smarty->use_sub_dirs) {
            $_filepath = substr($_filepath, 0, 2) . DS
             . substr($_filepath, 2, 2) . DS
             . substr($_filepath, 4, 2) . DS
             . $_filepath;
        } 
        $_compile_dir_sep = $_template->smarty->use_sub_dirs ? DS : '^';
        if (isset($_compile_id)) {
            $_filepath = $_compile_id . $_compile_dir_sep . $_filepath;
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
        return $_compile_dir . $_filepath . '.' . $_template->resource_type . '.' . basename($_files[count($_files)-1]) . $_cache . '.php';
    } 
} 

?>