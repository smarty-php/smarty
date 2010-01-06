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
        $_old_source = $compiler->template->template_source;
        if (preg_match_all("!({$this->_ldl}block(.+?){$this->_rdl})!", $_old_source, $s, PREG_OFFSET_CAPTURE) !=
                preg_match_all("!({$this->_ldl}/block(.*?){$this->_rdl})!", $_old_source, $c, PREG_OFFSET_CAPTURE)) {
            $this->compiler->trigger_template_error('unmatched {block} {/block} pairs');
        } 
        $block_count = count($s[0]);
        for ($i = 0; $i < $block_count; $i++) {
            $block_content = str_replace($this->smarty->left_delimiter . '$smarty.parent' . $this->smarty->right_delimiter, '%%%%SMARTY_PARENT%%%%',
                substr($_old_source, $s[0][$i][1] + strlen($s[0][$i][0]), $c[0][$i][1] - $s[0][$i][1] - strlen($s[0][$i][0])));
            $this->saveBlockData($block_content, $s[0][$i][0], $compiler->template);
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
