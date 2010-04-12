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
    // hash for nocache sections
    private $nocache_hash = null; 
    // suppress generation of nocache code
    public $suppressNocacheProcessing = false; 
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
        $this->nocache_hash = str_replace('.', '-', uniqid(rand(), true));
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
        if (empty($template->properties['nocache_hash'])) {
            $template->properties['nocache_hash'] = $this->nocache_hash;
        } else {
            $this->nocache_hash = $template->properties['nocache_hash'];
        } 
        /* here is where the compiling takes place. Smarty
       tags in the templates are replaces with PHP code,
       then written to compiled files. */
        // flag for nochache sections
        $this->nocache = false;
        $this->tag_nocache = false; 
        // save template object in compiler class
        $this->template = $template;
        $this->smarty->_current_file = $this->template->getTemplateFilepath(); 
        // template header code
        $template_header = '';
        if (!$template->suppressHeader) {
            $template_header .= "<?php /* Smarty version " . Smarty::SMARTY_VERSION . ", created on " . strftime("%Y-%m-%d %H:%M:%S") . "\n";
            $template_header .= "         compiled from \"" . $this->template->getTemplateFilepath() . "\" */ ?>\n";
        } 

        do {
            // flag for aborting current and start recompile
            $this->abort_and_recompile = false; 
            // get template source
            $_content = $template->getTemplateSource(); 
            // run prefilter if required
            if (isset($this->smarty->autoload_filters['pre']) || isset($this->smarty->registered_filters['pre'])) {
                $_content = Smarty_Internal_Filter_Handler::runFilter('pre', $_content, $this->smarty, $template);
            } 
            // on empty template just return header
            if ($_content == '') {
                if ($template->suppressFileDependency) {
                    $template->compiled_template = '';
                } else {
                    $template->compiled_template = $template_header . $template->createPropertyHeader();
                } 
                return true;
            } 
            // call compiler
            $_compiled_code = $this->doCompile($_content);
        } while ($this->abort_and_recompile); 
        // return compiled code to template object
        if ($template->suppressFileDependency) {
            $template->compiled_template = $_compiled_code;
        } else {
            $template->compiled_template = $template_header . $template->createPropertyHeader() . $_compiled_code;
        } 
        // run postfilter if required
        if (isset($this->smarty->autoload_filters['post']) || isset($this->smarty->registered_filters['post'])) {
            $template->compiled_template = Smarty_Internal_Filter_Handler::runFilter('post', $template->compiled_template, $this->smarty, $template);
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
        if (($_output = $this->callTagCompiler($tag, $args)) === false) {
            if (isset($this->smarty->template_functions[$tag])) {
                // template defined by {template} tag
                $args['name'] = "'" . $tag . "'";
                $_output = $this->callTagCompiler('call', $args);
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
            if (strlen($tag) < 6 || substr_compare($tag, 'close', -5, 5) != 0) {
                // check if tag is a registered object
                if (isset($this->smarty->registered_objects[$tag]) && isset($args['object_methode'])) {
                    $methode = $args['object_methode'];
                    unset ($args['object_methode']);
                    if (!in_array($methode, $this->smarty->registered_objects[$tag][3]) &&
                            (empty($this->smarty->registered_objects[$tag][1]) || in_array($methode, $this->smarty->registered_objects[$tag][1]))) {
                        return $this->callTagCompiler('private_object_function', $args, $tag, $methode);
                    } elseif (in_array($methode, $this->smarty->registered_objects[$tag][3])) {
                        return $this->callTagCompiler('private_object_block_function', $args, $tag, $methode);
                    } else {
                        return $this->trigger_template_error ('unallowed methode "' . $methode . '" in registered object "' . $tag . '"', $this->lex->taglineno);
                    } 
                } 
                // check if tag is registered
                foreach (array('compiler', 'function', 'block') as $type) {
                    if (isset($this->smarty->registered_plugins[$type][$tag])) {
                        // if compiler function plugin call it now
                        if ($type == 'compiler') {
                            if (!$this->smarty->registered_plugins[$type][$tag][1]) {
                                $this->tag_nocache = true;
                            } 
                            return call_user_func_array($this->smarty->registered_plugins[$type][$tag][0], array($args, $this));
                        } 
                        // compile registered function or block function
                        if ($type == 'function' || $type == 'block') {
                            return $this->callTagCompiler('private_registered_' . $type, $args, $tag);
                        } 
                    } 
                } 
                // check plugins from plugins folder
                foreach ($this->smarty->plugin_search_order as $plugin_type) {
                    if ($plugin_type == 'compiler' && $this->smarty->loadPlugin('smarty_compiler_' . $tag)) {
                        $plugin = 'smarty_compiler_' . $tag;
                        if (is_callable($plugin)) {
                            return $plugin($args, $this->smarty);
                        } 
                        if (class_exists($plugin, false)) {
                            $plugin_object = new $plugin;
                            if (method_exists($plugin_object, 'compile')) {
                                return $plugin_object->compile($args, $this);
                            } 
                        } 
                        throw new Exception("Plugin \"{$tag}\" not callable");
                    } else {
                        if ($function = $this->getPlugin($tag, $plugin_type)) {
                            return $this->callTagCompiler('private_' . $plugin_type . '_plugin', $args, $tag, $function);
                        } 
                    } 
                } 
            } else {
                // compile closing tag of block function
                $base_tag = substr($tag, 0, -5); 
                // check if closing tag is a registered object
                if (isset($this->smarty->registered_objects[$base_tag]) && isset($args['object_methode'])) {
                    $methode = $args['object_methode'];
                    unset ($args['object_methode']);
                    if (in_array($methode, $this->smarty->registered_objects[$base_tag][3])) {
                        return $this->callTagCompiler('private_object_block_function', $args, $tag, $methode);
                    } else {
                        return $this->trigger_template_error ('unallowed closing tag methode "' . $methode . '" in registered object "' . $base_tag . '"', $this->lex->taglineno);
                    } 
                } 
                // registered block tag ?
                if (isset($this->smarty->registered_plugins['block'][$base_tag])) {
                    return $this->callTagCompiler('private_registered_block', $args, $tag);
                } 
                // block plugin?
                if ($function = $this->getPlugin($base_tag, 'block')) {
                    return $this->callTagCompiler('private_block_plugin', $args, $tag, $function);
                } 
                if ($this->smarty->loadPlugin('smarty_compiler_' . $tag)) {
                    $plugin = 'smarty_compiler_' . $tag;
                    if (is_callable($plugin)) {
                        return $plugin($args, $this->smarty);
                    } 
                    if (class_exists($plugin, false)) {
                        $plugin_object = new $plugin;
                        if (method_exists($plugin_object, 'compile')) {
                            return $plugin_object->compile($args, $this);
                        } 
                    } 
                    throw new Exception("Plugin \"{$tag}\" not callable");
                } 
            } 
            $this->trigger_template_error ("unknown tag \"" . $tag . "\"", $this->lex->taglineno);
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
     * @param  $param1 optional parameter
     * @param  $param2 optional parameter
     * @param  $param3 optional parameter
     * @return string compiled code
     */
    public function callTagCompiler($tag, $args, $param1 = null, $param2 = null, $param3 = null)
    { 
        // re-use object if already exists
        if (isset(self::$_tag_objects[$tag])) {
            // compile this tag
            return self::$_tag_objects[$tag]->compile($args, $this, $param1, $param2, $param3);
        } 
        // lazy load internal compiler plugin
        $class_name = 'Smarty_Internal_Compile_' . $tag;
        if ($this->smarty->loadPlugin($class_name)) {
            // use plugin if found
            self::$_tag_objects[$tag] = new $class_name; 
            // compile this tag
            return self::$_tag_objects[$tag]->compile($args, $this, $param1, $param2, $param3);
        } 
        // no internal compile plugin for this tag
        return false;
    } 

    /**
     * Check for plugins and return function name
     * 
     * @param  $pugin_name string name of plugin or function
     * @param  $type string type of plugin
     * @return string call name of function
     */
    public function getPlugin($plugin_name, $type)
    {
        $function = null;
        if ($this->template->caching && ($this->nocache || $this->tag_nocache)) {
            if (isset($this->template->required_plugins['nocache'][$plugin_name][$type])) {
                $function = $this->template->required_plugins['nocache'][$plugin_name][$type]['function'];
            } else if (isset($this->template->required_plugins['compiled'][$plugin_name][$type])) {
                $this->template->required_plugins['nocache'][$plugin_name][$type] = $this->template->required_plugins['compiled'][$plugin_name][$type];
                $function = $this->template->required_plugins['nocache'][$plugin_name][$type]['function'];
            } 
        } else {
            if (isset($this->template->required_plugins['compiled'][$plugin_name][$type])) {
                $function = $this->template->required_plugins['compiled'][$plugin_name][$type]['function'];
            } else if (isset($this->template->required_plugins['compiled'][$plugin_name][$type])) {
                $this->template->required_plugins['compiled'][$plugin_name][$type] = $this->template->required_plugins['nocache'][$plugin_name][$type];
                $function = $this->template->required_plugins['compiled'][$plugin_name][$type]['function'];
            } 
        } 
        if (isset($function)) {
            if ($type == 'modifier') {
                $this->template->saved_modifier[$plugin_name] = true;
            } 
            return $function;
        } 
        /**
         * if (isset($this->template->required_plugins_call[$plugin_name][$type])) {
         * if ($this->template->caching && ($this->nocache || $this->tag_nocache)) {
         * if (isset($this->template->required_plugins['compiled'][$plugin_name])) {
         * $this->template->required_plugins['cache'][$plugin_name] = $this->template->required_plugins['compiled'][$plugin_name];
         * } 
         * } else {
         * if (isset($this->template->required_plugins['cache'][$plugin_name])) {
         * $this->template->required_plugins['compiled'][$plugin_name] = $this->template->required_plugins['cache'][$plugin_name];
         * } 
         * } 
         * if ($type == 'modifier') {
         * $this->template->saved_modifier[$plugin_name] = true;
         * } 
         * return $this->template->required_plugins_call[$plugin_name][$type];
         * }
         */
        // loop through plugin dirs and find the plugin
        $function = 'smarty_' . $type . '_' . $plugin_name;
        $found = false;
        foreach((array)$this->smarty->plugins_dir as $_plugin_dir) {
            $file = rtrim($_plugin_dir, '/\\') . DS . $type . '.' . $plugin_name . '.php';
            if (file_exists($file)) {
                // require_once($file);
                $found = true;
                break;
            } 
        } 
        if ($found) {
            // if (is_callable($plugin)) {
            if ($this->template->caching && ($this->nocache || $this->tag_nocache)) {
                $this->template->required_plugins['nocache'][$plugin_name][$type]['file'] = $file;
                $this->template->required_plugins['nocache'][$plugin_name][$type]['function'] = $function;
            } else {
                $this->template->required_plugins['compiled'][$plugin_name][$type]['file'] = $file;
                $this->template->required_plugins['compiled'][$plugin_name][$type]['function'] = $function;
            } 
            /**
             * $this->template->required_plugins_call[$plugin_name][$type] = $plugin;
             * if ($this->template->caching && ($this->nocache || $this->tag_nocache)) {
             * $this->template->required_plugins['cache'][$plugin_name]['file'] = $file;
             * $this->template->required_plugins['cache'][$plugin_name]['type'] = $type;
             * } else {
             * $this->template->required_plugins['compiled'][$plugin_name]['file'] = $file;
             * $this->template->required_plugins['compiled'][$plugin_name]['type'] = $type;
             * }
             */
            if ($type == 'modifier') {
                $this->template->saved_modifier[$plugin_name] = true;
            } 
            return $function;
        } 
        if (is_callable($function)) {
            // plugin function is defined in the script
            return $function;
        } 
        return false;
    } 
    /**
     * Inject inline code for nocache template sections
     * 
     * This method gets the content of each template element from the parser.
     * If the content is compiled code and it should be not cached the code is injected
     * into the rendered output.
     * 
     * @param string $content content of template element
     * @param boolean $tag_nocache true if the parser detected a nocache situation
     * @param boolean $is_code true if content is compiled code
     * @return string content
     */
    public function processNocacheCode ($content, $is_code)
    { 
        // If the template is not evaluated and we have a nocache section and or a nocache tag
        if ($is_code && !empty($content)) {
            // generate replacement code
            if ((!$this->template->resource_object->isEvaluated || $this->template->forceNocache) && $this->template->caching && !$this->suppressNocacheProcessing &&
                    ($this->nocache || $this->tag_nocache || $this->template->forceNocache == 2)) {
                $this->tag_nocache = false;
                $this->template->has_nocache_code = true;
                $_output = str_replace("'", "\'", $content);
                $_output = "<?php echo '/*%%SmartyNocache:{$this->nocache_hash}%%*/" . $_output . "/*/%%SmartyNocache:{$this->nocache_hash}%%*/';?>"; 
                // make sure we include modifer plugins for nocache code
                if (isset($this->template->saved_modifier)) {
                    foreach ($this->template->saved_modifier as $plugin_name => $dummy) {
                        if (isset($this->template->required_plugins['compiled'][$plugin_name]['modifier'])) {
                            $this->template->required_plugins['nocache'][$plugin_name]['modifier'] = $this->template->required_plugins['compiled'][$plugin_name]['modifier'];
                        } 
                    } 
                    $this->template->saved_modifier = null;
                } 
            } else {
                $_output = $content;
            } 
        } else {
            $_output = $content;
        } 
        $this->suppressNocacheProcessing = false;
        return $_output;
    } 
    /**
     * display compiler error messages without dying
     * 
     * If parameter $args is empty it is a parser detected syntax error.
     * In this case the parser is called to obtain information about expected tokens.
     * 
     * If parameter $args contains a string this is used as error message
     * 
     * @param  $args string individual error message or null
     */
    public function trigger_template_error($args = null, $line = null)
    { 
        // get template source line which has error
        if (!isset($line)) {
            $line = $this->lex->line;
        } 
        $match = preg_split("/\n/", $this->lex->data);
        $error_text = 'Syntax Error in template "' . $this->template->getTemplateFilepath() . '"  on line ' . $line . ' "' . htmlspecialchars($match[$line-1]) . '" ';
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
    } 
} 

?>