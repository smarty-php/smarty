<?php
/**
* Smarty Internal Plugin Compile Function
* 
* Compiles the {function} {/function} tags
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Compile Function Class
*/
class Smarty_Internal_Compile_Function extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {function} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return boolean true
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->required_attributes = array('name');
        $this->optional_attributes = array('_any'); 
        // check and get attributes
        $_attr = $this->_get_attributes($args);
        $save = array($_attr, $compiler->template->extracted_compiled_code, $compiler->template->extract_code,
            $compiler->template->has_nocache_code, $compiler->template->required_plugins);
        $this->_open_tag('function', $save);
        $_name = trim($_attr['name'], "'\"");
        unset($_attr['name']);
        foreach ($_attr as $_key => $_data) {
            $compiler->template->properties['function'][$_name]['parameter'][$_key] = $_data;
        } 
        // make function known for recursive calls
        $this->compiler->smarty->template_functions[$_name]['compiled'] = ''; 
        // Init temporay context
        $compiler->template->required_plugins = array('compiled' => array(), 'nocache' => array());
        $compiler->template->extract_code = true;
        $compiler->template->extracted_compiled_code = '';
        $compiler->template->has_nocache_code = false;
        $compiler->has_code = false;
        return true;
    } 
} 

/**
* Smarty Internal Plugin Compile Functionclose Class
*/
class Smarty_Internal_Compile_Functionclose extends Smarty_Internal_CompileBase {
    /**
    * Compiles code for the {/function} tag
    * 
    * @param array $args array with attributes from parser
    * @param object $compiler compiler object
    * @return boolean true
    */
    public function compile($args, $compiler)
    {
        $this->compiler = $compiler;
        $this->compiler->has_code = false;
        $_attr = $this->_get_attributes($args);
        $saved_data = $this->_close_tag(array('function'));
        $_name = trim($saved_data[0]['name'], "'"); 
        // build plugin include code
        $plugins_string = '';
        if (!empty($compiler->template->required_plugins['compiled'])) {
            $plugins_string = '<?php ';
            foreach($compiler->template->required_plugins['compiled'] as $tmp) {
                foreach($tmp as $data) {
                    $plugins_string .= "if (!is_callable('{$data['function']}')) include '{$data['file']}';\n";
                } 
            } 
            $plugins_string .= '?>';
        } 
        if (!empty($compiler->template->required_plugins['nocache'])) {
            $plugins_string .= "<?php echo '/*%%SmartyNocache:{$compiler->template->properties['nocache_hash']}%%*/<?php ";
            foreach($compiler->template->required_plugins['nocache'] as $tmp) {
                foreach($tmp as $data) {
                    $plugins_string .= "if (!is_callable(\'{$data['function']}\')) include \'{$data['file']}\';\n";
                } 
            } 
            $plugins_string .= "?>/*/%%SmartyNocache:{$compiler->template->properties['nocache_hash']}%%*/';?>\n";
        } 
        $compiler->template->properties['function'][$_name]['compiled'] = $plugins_string . $compiler->template->extracted_compiled_code;
        $compiler->template->properties['function'][$_name]['nocache_hash'] = $compiler->template->properties['nocache_hash'];
        $compiler->template->properties['function'][$_name]['has_nocache_code'] = $compiler->template->has_nocache_code; 
        $this->compiler->smarty->template_functions[$_name] = $compiler->template->properties['function'][$_name]; 
        // restore old compiler status
        $compiler->template->extracted_compiled_code = $saved_data[1];
        $compiler->template->extract_code = $saved_data[2];
        $compiler->template->has_nocache_code = $saved_data[3];
        $compiler->template->required_plugins = $saved_data[4];
        return true;
    } 
} 

?>