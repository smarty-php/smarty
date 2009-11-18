<?php
/**
* Smarty Internal Plugin Smarty Template Compiler Base
* 
* This file contains the basic classes and methodes for compiling Smarty templates with lexer/parser
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Main compiler class
*/
class Smarty_Internal_TemplateCompilerBase {
    // compile tag objects
    static $_tag_objects = array(); 
    // tag stack
    public $_tag_stack = array(); 
    // current template
    public $template = null;

    /**
    * Initialize compiler
    */
    public function __construct()
    {
    } 
    // abstract function doCompile($_content);
    /**
    * Methode to compile a Smarty template
    * 
    * @param  $template template object to compile
    * @return bool true if compiling succeeded, false if it failed
    */
    public function compileTemplate($template)
    {
        /* here is where the compiling takes place. Smarty
       tags in the templates are replaces with PHP code,
       then written to compiled files. */
        if (!is_object($template->cacher_object)) {
            $this->smarty->loadPlugin($template->cacher_class);
            $template->cacher_object = new $template->cacher_class($this->smarty);
        } 
        // flag for nochache sections
        $this->nocache = false;
        $this->tag_nocache = false; 
        // assume successfull compiling
        $this->compile_error = false; 
        // save template object in compiler class
        $this->template = $template;
        $this->smarty->_current_file = $this->template->getTemplateFilepath(); 
        // template header code
        $template_header = '';
        if (!$template->suppressHeader) {
            $template_header .= "<?php /* Smarty version " . Smarty::$_version . ", created on " . strftime("%Y-%m-%d %H:%M:%S") . "\n";
            $template_header .= "         compiled from \"" . $this->template->getTemplateFilepath() . "\" */ ?>\n";
        } 

        do {
            // flag for aborting current and start recompile
            $this->abort_and_recompile = false; 
            // get template source
            $_content = $template->getTemplateSource(); 
            // run prefilter if required
            if (isset($this->smarty->autoload_filters['pre']) || isset($this->smarty->registered_filters['pre'])) {
                $_content = $this->smarty->filter_handler->execute('pre', $_content);
            } 
            // on empty template just return header
            if ($_content == '') {
                if ($template->suppressFileDependency) {
                    $template->compiled_template = '';
                } else {
                    $template->compiled_template = $template->createPropertyHeader() . $template_header;
                } 
                return true;
            } 
            // init cacher plugin
            $template->cacher_object->initCacher($this); 
            // call compiler
            $_compiled_code = $this->doCompile($_content);
        } while ($this->abort_and_recompile);

        if (!$this->compile_error) {
            // close cacher and return compiled template
            if ($template->suppressFileDependency) {
                $template->compiled_template = $template->cacher_object->closeCacher($this, $_compiled_code);
            } else {
                $template->compiled_template = $template->createPropertyHeader() . $template_header . $template->cacher_object->closeCacher($this, $_compiled_code);
            } 
            // run postfilter if required
            if (isset($this->smarty->autoload_filters['post']) || isset($this->smarty->registered_filters['post'])) {
                $template->compiled_template = $this->smarty->filter_handler->execute('post', $template->compiled_template);
            } 
            return true;
        } else {
            // compilation error
            return false;
        } 
    } 

    /**
    * Compile Tag
    * 
    * This is a call back from the lexer/parser
    * It executes the required compile plugin for the Smarty tag
    * 
    * @param string $tag tag name
    * @param array $args array with tag attributes
    * @return string compiled code
    */
    public function compileTag($tag, $args)
    { 
        // $args contains the attributes parsed and compiled by the lexer/parser
        // assume that tag does compile into code, but creates no HTML output
        $this->has_code = true;
        $this->has_output = false; 
        // compile the smarty tag (required compile classes to compile the tag are autoloaded)
        if (($_output = $this->generateCode($tag, $args)) === false) {
            if (isset($this->smarty->template_functions[$tag])) {
                // template defined by {template} tag
                $args['name'] = $tag;
                $tag = 'function_call';
                $_output = $this->generateCode($tag, $args);
            } 
        } 
        if ($_output !== false) {
            if ($_output !== true) {
                // did we get compiled code
                if ($this->has_code) {
                    // Does it create output?
                    if ($this->has_output) {
                        $_output .= "\n";
                    } 
                    // return compiled code
                    return $_output;
                } 
            } 
            // tag did not produce compiled code
            return '';
        } else {
            // not an internal compiler tag
            // check if tag is a registered object
            if (isset($this->smarty->registered_objects[$tag]) && isset($args['object_methode'])) {
                $methode = $args['object_methode'];
                unset ($args['object_methode']);
                if (!in_array($methode, $this->smarty->registered_objects[$tag][3]) &&
                        (empty($this->smarty->registered_objects[$tag][1]) || in_array($methode, $this->smarty->registered_objects[$tag][1]))) {
                    return $this->generateCode('object_function',$args, $tag, $methode);
                } elseif (in_array($methode, $this->smarty->registered_objects[$tag][3])) {
                    return $this->generateCode('object_block_function',$args, $tag, $methode);
                } else {
                    return $this->trigger_template_error ('unallowed methode "' . $methode . '" in registered object "' . $tag . '"');
                } 
            } 
            // check if tag is registered or is Smarty plugin
            $this->smarty->plugin_handler->loadSmartyPlugin($tag, $this->smarty->plugin_search_order);
            if (isset($this->smarty->registered_plugins[$tag])) {
                // if compiler function plugin call it now
                if ($this->smarty->registered_plugins[$tag][0] == 'compiler') {
                    if (!$this->smarty->registered_plugins[$tag][2]) {
                        $this->tag_nocache = true;
                    } 
                    return call_user_func($this->smarty->registered_plugins[$tag][1], $args, $this);
                } 
                // compile function or block plugin
                $plugin_type = $this->smarty->registered_plugins[$tag][0] . '_plugin';
                return $this->generateCode($plugin_type, $args, $tag);
            } 
            // compile closing tag of block function
            if (strlen($tag) > 5 && substr_compare($tag, 'close', -5, 5) == 0) {
                $base_tag = substr($tag, 0, -5); 
                // check if closing tag is a registered object
                if (isset($this->smarty->registered_objects[$base_tag]) && isset($args['object_methode'])) {
                    $methode = $args['object_methode'];
                    unset ($args['object_methode']);
                    if (in_array($methode, $this->smarty->registered_objects[$base_tag][3])) {
                        return $this->generateCode('object_block_function', $args, $tag, $methode);
                    } else {
                        return $this->trigger_template_error ('unallowed closing tag methode "' . $methode . '" in registered object "' . $base_tag . '"');
                    } 
                } 
                // plugin ?
                if (isset($this->smarty->registered_plugins[$base_tag]) && $this->smarty->registered_plugins[$base_tag][0] == 'block') {
                    return $this->generateCode('block_plugin',$args, $tag);
                } 
            } 
            $this->trigger_template_error ("unknown tag \"" . $tag . "\"");
        } 
    } 

    /**
    * lazy loads internal compile plugin for tag and calls the compile methode
    * 
    * compile objects cached for reuse.
    * class name format:  Smarty_Internal_Compile_TagName
    * plugin filename format: Smarty_Internal_Tagname.php
    * 
    * @param  $tag string tag name
    * @param  $args array with tag attributes
    * @param  $subtag optional tag name at plugins
    * @param  $method string optional method on object tags
    * @return string compiled code
    */
    public function generateCode($tag, $args, $subtag = null, $method = null)
    { 
        // re-use object if already exists
        if (isset(self::$_tag_objects[$tag])) {
            // compile this tag
            return call_user_func(array(self::$_tag_objects[$tag], 'compile'), $args, $this, $subtag, $method);
        } 
        // lazy load internal compiler plugin
        $class_name = 'Smarty_Internal_Compile_' . $tag;
        if ($this->smarty->loadPlugin($class_name)) {
            // use plugin if found
            self::$_tag_objects[$tag] = new $class_name; 
            // compile this tag
            return call_user_func(array(self::$_tag_objects[$tag], 'compile'), $args, $this, $subtag, $method);
        } 
        // no internal compile plugin for this tag
        return false;
    } 

    /**
    * display compiler error messages without dying
    * 
    * If parameter $args is empty it is a parser detected syntax error.
    * In this case the parser is called to obtain information about expected tokens.
    * 
    * If parameter $args contains a string this is used as error message
    * 
    * @todo output exact position of parse error in source line
    * @param  $args string individual error message or null
    */
    public function trigger_template_error($args = null)
    {
        $this->lex = Smarty_Internal_Templatelexer::instance();
        $this->parser = Smarty_Internal_Templateparser::instance(); 
        // get template source line which has error
        $line = $this->lex->line;
        if (isset($args)) {
            // $line--;
        } 
        $match = preg_split("/\n/", $this->lex->data);
        $error_text = 'Syntax Error in template "' . $this->template->getTemplateFilepath() . '"  on line ' . $line . ' "' . $match[$line-1] . '" ';

        if (isset($args)) {
            // individual error message
            $error_text .= $args;
        } else {
            // expected token from parser
            foreach ($this->parser->yy_get_expected_tokens($this->parser->yymajor) as $token) {
                $exp_token = $this->parser->yyTokenName[$token];
                if (isset($this->lex->smarty_token_names[$exp_token])) {
                    // token type from lexer
                    $expect[] = '"' . $this->lex->smarty_token_names[$exp_token] . '"';
                } else {
                    // otherwise internal token name
                    $expect[] = $this->parser->yyTokenName[$token];
                } 
            } 
            // output parser error message
            $error_text .= ' - Unexpected "' . $this->lex->value . '", expected one of: ' . implode(' , ', $expect);
        } 
        throw new Exception($error_text); 
        // set error flag
        $this->compile_error = true;
    } 
} 

?>
