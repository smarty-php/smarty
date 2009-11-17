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
        $this->smarty =$compiler->smarty;
        $this->required_attributes = array('file'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);
        $_smarty_tpl = $compiler->template; 
        // $include_file = '';
        eval('$include_file = ' . $_attr['file'] . ';'); 
        // create template object
        $_template = new Smarty_Template ($include_file, $this->smarty, $compiler->template); 
        // save file dependency
        $compiler->template->properties['file_dependency']['F'.abs(crc32($_template->getTemplateFilepath()))] = array($_template->getTemplateFilepath(), $_template->getTemplateTimestamp());
        $_old_source = $compiler->template->template_source;
        if (preg_match_all('/(' . $this->smarty->left_delimiter . 'block(.+?)' . $this->smarty->right_delimiter . ')/', $_old_source, $s, PREG_OFFSET_CAPTURE) !=
                preg_match_all('/(' . $this->smarty->left_delimiter . '\/block(.*?)' . $this->smarty->right_delimiter . ')/', $_old_source, $c, PREG_OFFSET_CAPTURE)) {
            $this->compiler->trigger_template_error(" unmatched {block} {/block} pairs");
        } 
        $block_count = count($s[0]);
        for ($i = 0; $i < $block_count; $i++) {
            $block_content = str_replace($this->smarty->left_delimiter . '$smarty.parent' . $this->smarty->right_delimiter, '%%%%SMARTY_PARENT%%%%',
                substr($_old_source, $s[0][$i][1] + strlen($s[0][$i][0]), $c[0][$i][1] - $s[0][$i][1] - strlen($s[0][$i][0])));
            $this->saveBlockData($block_content, $s[0][$i][0],$compiler->template);
        } 
        $compiler->template->template_source = $_template->getTemplateSource();
        $compiler->abort_and_recompile = true;
        return ' ';
    } 

    protected function saveBlockData($block_content, $block_tag,$template)
    {
        if (0 == preg_match('/(.?)(name=)([^ ]*)/', $block_tag, $_match)) {
            $this->compiler->trigger_template_error("\"" . $block_tag . "\" missing name attribute");
        } else {
            // compile block content
            $_tpl = $this->smarty->createTemplate('string:' . $block_content,null,null,$template);
            $_tpl->template_filepath = $this->compiler->template->getTemplateFilepath();
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
} 

?>
