<?php

/**
 * Smarty Internal Plugin Compile extend
 * 
 * Compiles the {extends} tag
 * 
 * @package Smarty
 * @subpackage Compiler
 * @author Uwe Tews 
 */
/**
 * Smarty Internal Plugin Compile extend Class
 */
class Smarty_Internal_Compile_Extends extends Smarty_Internal_CompileBase {
    /**
     * Compiles code for the {extends} tag
     * 
     * @param array $args array with attributes from parser
     * @param object $compiler compiler object
     * @return string compiled code
     */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->smarty = $compiler->smarty;
        $this->_rdl = preg_quote($this->smarty->right_delimiter);
        $this->_ldl = preg_quote($this->smarty->left_delimiter);
        $this->required_attributes = array('file'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);
        $_smarty_tpl = $compiler->template; 
        // $include_file = '';
        eval('$include_file = ' . $_attr['file'] . ';'); 
        // create template object
        $_template = new $compiler->smarty->template_class($include_file, $this->smarty, $compiler->template); 
        // save file dependency
        $compiler->template->properties['file_dependency'][sha1($_template->getTemplateFilepath())] = array($_template->getTemplateFilepath(), $_template->getTemplateTimestamp());
        $_content = $compiler->template->template_source;
        if (preg_match_all("!({$this->_ldl}block(.+?){$this->_rdl})!", $_content, $s) !=
                preg_match_all("!({$this->_ldl}/block(.*?){$this->_rdl})!", $_content, $c)) {
            $this->compiler->trigger_template_error('unmatched {block} {/block} pairs');
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
            $this->saveBlockData($_block_content, $_result[0][$_start][0], $compiler->template);
            $_start = $_start + $_end + 1;
        } 
        $compiler->template->template_source = $_template->getTemplateSource();
        $compiler->template->template_filepath = $_template->getTemplateFilepath();
        $compiler->abort_and_recompile = true;
        return ' ';
    } 

    protected function saveBlockData($block_content, $block_tag, $template)
    {
        if (0 == preg_match("!(.?)(name=)(.*?)(?=(\s|{$this->_rdl}))!", $block_tag, $_match)) {
            $this->compiler->trigger_template_error("\"" . $block_tag . "\" missing name attribute");
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
            $this->smarty->block_data[$_name]['file'] = $template->getTemplateFilepath();
        } 
    } 
} 

?>