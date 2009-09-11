<?php

/**
* Smarty Internal Plugin Compile extend
* 
* Compiles the {extend} tag
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile extend Class
*/
class Smarty_Internal_Compile_Extend extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {extend} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return string compiled code
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('file'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);
        $_smarty_tpl = $compiler->template; 
        // $include_file = '';
        eval('$include_file = ' . $_attr['file'] . ';'); 
        // create template object
        $_template = new Smarty_Template ($include_file, $this->compiler->smarty, $compiler->template); 
        // save file dependency
        $compiler->template->properties['file_dependency'][] = array($_template->getTemplateFilepath(), $_template->getTemplateTimestamp()); 
        // $_old_source = preg_replace ('/' . $this->smarty->left_delimiter . 'extend\s+(?:file=)?\s*(\S+?|(["\']).+?\2)' . $this->smarty->right_delimiter . '/i', '' , $compiler->template->template_source, 1);
        $_old_source = $compiler->template->template_source;
        if (preg_match_all('/(' . $this->compiler->smarty->left_delimiter . 'block(.+?)' . $this->compiler->smarty->right_delimiter . ')/', $_old_source, $s, PREG_OFFSET_CAPTURE) !=
                preg_match_all('/(' . $this->compiler->smarty->left_delimiter . '\/block(.*?)' . $this->compiler->smarty->right_delimiter . ')/', $_old_source, $c, PREG_OFFSET_CAPTURE)) {
            $this->compiler->trigger_template_error(" unmatched {block} {/block} pairs");
        } 
        $block_count = count($s[0]);
        for ($i = 0; $i < $block_count; $i++) {
//            $block_content = substr($_old_source, $s[0][$i][1], $c[0][$i][1] + strlen($c[0][$i][0]) - $s[0][$i][1]);
            $block_content = substr($_old_source, $s[0][$i][1] + strlen($s[0][$i][0]), $c[0][$i][1]  - $s[0][$i][1] - strlen($s[0][$i][0]));
            $this->saveBlockData($block_content, $s[0][$i][0]);
        } 
        $compiler->template->template_source = $_template->getTemplateSource();
        $compiler->abort_and_recompile = true;
        return ' ';
    } 

    protected function saveBlockData($block_content, $block_tag)
    {
        if (0 == preg_match('/(.?)(name=)([^ ]*)/', $block_tag, $_match)) {
            $this->compiler->trigger_template_error("\"" . $block_tag . "\" missing name attribute");
        } else {
            // compile block content
            $_tpl = $this->compiler->smarty->createTemplate('string:' . $block_content);
            $_tpl->template_filepath = $this->compiler->template->getTemplateFilepath();
            $_tpl->suppressHeader = true;
            $_compiled_content = $_tpl->getCompiledTemplate();
            unset($_tpl);
            $_name = trim($_match[3], "\"'}");

            if (isset($this->compiler->template->block_data[$_name])) {
                if ($this->compiler->template->block_data[$_name]['mode'] == 'prepend') {
                    $this->compiler->template->block_data[$_name]['compiled'] .= $_compiled_content;
                    $this->compiler->template->block_data[$_name]['source'] .= $block_content;
                } elseif ($this->compiler->template->block_data[$_name]['mode'] == 'append') {
                    $this->compiler->template->block_data[$_name]['compiled'] = $_compiled_content . $this->compiler->template->block_data[$_name]['compiled'];
                    $this->compiler->template->block_data[$_name]['source'] = $block_content . $this->compiler->template->block_data[$_name]['source'];
                } 
            } else {
                $this->compiler->template->block_data[$_name]['compiled'] = $_compiled_content;
                $this->compiler->template->block_data[$_name]['source'] = $block_content;
            } 
            if (preg_match('/(.?)(append=true)(.*)/', $block_tag, $_match) != 0) {
                $this->compiler->template->block_data[$_name]['mode'] = 'append';
            } elseif (preg_match('/(.?)(prepend=true)(.*)/', $block_tag, $_match) != 0) {
                $this->compiler->template->block_data[$_name]['mode'] = 'prepend'; 
                // }
                // }
            } else {
                $this->compiler->template->block_data[$_name]['mode'] = 'replace';
            } 
        } 
    } 
} 

?>
