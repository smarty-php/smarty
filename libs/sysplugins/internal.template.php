<?php

/**
* Smarty Internal Plugin Template
* 
* This files contains the Smarty template engine
* 
* @package Smarty
* @subpackage Templates
* @author Uwe Tews 
*/

/**
* Main class with template data structures and methods
*/
class Smarty_Internal_Template extends Smarty_Internal_TemplateBase {
    // object cache
    public $resource_objects = array();
    public $compiler_object = null;
    public $cacher_object = null; 
    // Smarty parameter
    public $cache_id = null;
    public $compile_id = null;
    public $caching = null;
    public $caching_lifetime = null;
    public $cacher_class = null;
    public $caching_type = null;
    public $force_compile = null; 
    // Template resource
    public $template_resource = null;
    public $resource_type = null;
    public $resource_name = null;
    private $usesCompiler = null;
    private $isEvaluated = null; 
    // Template source
    private $template_filepath = null;
    public $template_source = null;
    private $template_timestamp = null; 
    // Compiled template
    private $compiled_filepath = null;
    public $compiled_template = null;
    private $compiled_timestamp = null;
    public $compile_time = 0;
    public $mustCompile = null;
    public $suppressHeader = false;
    public $extract_code = false;
    public $extracted_compiled_code = ''; 
    // Rendered content
    public $rendered_content = null; 
    // Cache file
    private $cached_filepath = null;
    private $cached_timestamp = null;
    private $isCached = null;
    public $cache_time = 0; 
    // template variables
    public $tpl_vars = array();
    public $parent = null;
    public $config_vars = array(); 
    // storage for plugin
    public $plugin_data = array(); 
    // files template is depending from
    public $file_dependency = array(); 
    // storage for block data
    public $block_data = array();

    public $render_time = 0;

    /**
    * Create template data object
    * 
    * Some of the global Smarty settings copied to template scope
    * It load the required template resources and cacher plugins
    * 
    * @param string $template_resource template resource string
    * @param object $_parent back pointer to parent object with variables or null
    * @param mixed $_cache_id cache id or null
    * @param mixed $_compile_id compile id or null
    */
    public function __construct($template_resource, $_parent = null, $_cache_id = null, $_compile_id = null)
    {
        $this->smarty = Smarty::instance(); 
        // Smarty parameter
        $this->cache_id = $_cache_id === null ? $this->smarty->cache_id : $_cache_id;
        $this->compile_id = $_compile_id === null ? $this->smarty->compile_id : $_compile_id;
        $this->force_compile = $this->smarty->force_compile;
        $this->caching = $this->smarty->caching;
        $this->caching_lifetime = $this->smarty->caching_lifetime;
        $this->cacher_class = $this->smarty->cacher_class;
        $this->caching_type = $this->smarty->default_caching_type;
        $this->security = $this->smarty->security;
        $this->cache_resource_class = 'Smarty_Internal_CacheResource_' . ucfirst($this->caching_type);
        $this->parent = $_parent;
        $this->tpl_vars['smarty'] = new Smarty_Variable; 
        // Template resource
        $this->template_resource = $template_resource; 
        // parse resource name
        if (!$this->parseResourceName ($template_resource)) {
            throw new Exception ("Unable to parse resource name \"{$template_resource}\"");
        } 
        // load cacher
        if ($this->caching) {
            $this->smarty->loadPlugin($this->cacher_class);
            $this->cacher_object = new $this->cacher_class;
        } 
        // load cache resource
        if (!$this->isEvaluated() && $this->caching && !isset($this->smarty->cache_resource_objects[$this->caching_type])) {
            $this->smarty->loadPlugin($this->cache_resource_class);
            $this->smarty->cache_resource_objects[$this->caching_type] = new $this->cache_resource_class;
        } 
        if ($this->smarty->direct_access_security) {
            $this->dir_acc_sec_string = "<?php if(!defined('SMARTY_DIR')) exit('no direct access allowed'); ?>\n";
        } else {
            $this->dir_acc_sec_string = '';
        } 
    } 

    /**
    * Returns the template filepath
    * 
    * The template filepath is determined by the actual resource handler
    * 
    * @return string the template filepath
    */
    public function getTemplateFilepath ()
    {
        return $this->template_filepath === null ?
        $this->template_filepath = $this->resource_objects[$this->resource_type]->getTemplateFilepath($this) :
        $this->template_filepath;
    } 

    /**
    * Returns the timpestamp of the template source
    * 
    * The template timestamp is determined by the actual resource handler
    * 
    * @return integer the template timestamp
    */
    public function getTemplateTimestamp ()
    {
        return $this->template_timestamp === null ?
        $this->template_timestamp = $this->resource_objects[$this->resource_type]->getTemplateTimestamp($this) :
        $this->template_timestamp;
    } 

    /**
    * Returns the template source code
    * 
    * The template source is being read by the actual resource handler
    * 
    * @return string the template source
    */
    public function getTemplateSource ()
    {
        if ($this->template_source === null) {
            $this->resource_objects[$this->resource_type]->getTemplateSource($this);
        } 
        return $this->template_source;
    } 

    /**
    * Returns if the template resource uses the Smarty compiler
    * 
    * The status is determined by the actual resource handler
    * 
    * @return boolean true if the template will use the compiler
    */
    public function usesCompiler ()
    {
        return $this->usesCompiler === null ?
        $this->usesCompiler = $this->resource_objects[$this->resource_type]->usesCompiler() :
        $this->usesCompiler;
    } 

    /**
    * Returns if the compiled template is stored or just evaluated in memory
    * 
    * The status is determined by the actual resource handler
    * 
    * @return boolean true if the compiled template has to be evaluated
    */
    public function isEvaluated ()
    {
        return $this->isEvaluated === null ?
        $this->isEvaluated = $this->resource_objects[$this->resource_type]->isEvaluated() :
        $this->isEvaluated;
    } 

    /**
    * Returns if the current template must be compiled by the Smarty compiler
    * 
    * It does compare the timestamps of template source and the compiled templates and checks the force compile configuration
    * 
    * @return boolean true if the template must be compiled
    */
    public function mustCompile ()
    {
        if ($this->mustCompile === null) {
            $this->mustCompile = ($this->usesCompiler() && ($this->force_compile || $this->isEvaluated() || ($this->smarty->compile_check && $this->getCompiledTimestamp () !== $this->getTemplateTimestamp ()))); 
            // read compiled template
            if ($this->compiled_template !== true && file_exists($this->getCompiledFilepath())) {
                $this->compiled_template = !$this->isEvaluated() ? file_get_contents($this->getCompiledFilepath()):'';
                $found = preg_match('~\<\?php /\*(.*)\*/ \?\>~', $this->compiled_template, $matches);
                if ($found) {
                    $this->file_dependency = unserialize($matches[1]);
                    if (!empty($this->file_dependency)) {
                        foreach ($this->file_dependency['file_dependency'] as $file_to_check) {
                            If (filemtime($file_to_check[0]) != $file_to_check[1]) {
                                $this->mustCompile = true;
                                return $this->mustCompile;
                            } 
                        } 
                    } 
                } 
            } 
        } 
        return $this->mustCompile;
    } 

    /**
    * Returns the compiled template filepath
    * 
    * @return string the template filepath
    */
    public function getCompiledFilepath ()
    {
        return $this->compiled_filepath === null ?
        ($this->compiled_filepath = !$this->isEvaluated() ? $this->resource_objects[$this->resource_type]->getCompiledFilepath($this) : false) :
        $this->compiled_filepath;
    } 

    /**
    * Returns the timpestamp of the compiled template
    * 
    * @return integer the template timestamp
    */
    public function getCompiledTimestamp ()
    {
        return $this->compiled_timestamp === null ?
        ($this->compiled_timestamp = (!$this->isEvaluated() && file_exists($this->getCompiledFilepath())) ? filemtime($this->getCompiledFilepath()) : false) :
        $this->compiled_timestamp;
    } 

    /**
    * Returns the compiled template 
    * 
    * It checks if the template must be compiled or just read from the template resource
    * 
    * @return string the compiled template
    */
    public function getCompiledTemplate ()
    {
        if ($this->compiled_template === null) {
            // see if template needs compiling.
            if ($this->mustCompile()) {
                $this->compileTemplateSource();
            } else {
                $this->compiled_template = !$this->isEvaluated() && $this->usesCompiler() ? file_get_contents($this->getCompiledFilepath()) : false;
            } 
        } 
        return $this->compiled_template;
    } 

    /**
    * Compiles the template
    * 
    * If the template is not evaluated the compiled template is saved on disk
    */
    public function compileTemplateSource ()
    {
        $_start_time = $this->_get_time(); 
        // compile template
        if (!is_object($this->compiler_object)) {
            // load compiler
            $this->smarty->loadPlugin('Smarty_Internal_CompileBase');
            $this->smarty->loadPlugin('Smarty_Internal_TemplateCompilerBase');
            $this->smarty->loadPlugin($this->resource_objects[$this->resource_type]->compiler_class);
            $this->compiler_object = new $this->resource_objects[$this->resource_type]->compiler_class($this->resource_objects[$this->resource_type]->template_lexer_class, $this->resource_objects[$this->resource_type]->template_parser_class);
        } 
        if (!is_object($this->smarty->write_file_object)) {
            $this->smarty->loadPlugin("Smarty_Internal_Write_File");
            $this->smarty->write_file_object = new Smarty_Internal_Write_File;
        } 
        // call compiler
        if ($this->compiler_object->compileTemplate($this)) {
            // compiling succeded
            if (!$this->isEvaluated()) {
                // build file dependency string
                $this->file_dependency_string = '<?php /*' . serialize($this->file_dependency) . "*/ ?>\n"; 
                // write compiled template
                $this->smarty->write_file_object->writeFile($this->getCompiledFilepath(), $this->file_dependency_string . $this->dir_acc_sec_string . $this->getCompiledTemplate()); 
                // make template and compiled file timestamp match
                touch($this->getCompiledFilepath(), $this->getTemplateTimestamp());
            } 
        } else {
            // error compiling template
            throw new Exception("Error compiling template {$this->getTemplateFilepath ()}");
            return false;
        } 
        $this->compile_time = $this->_get_time() - $_start_time;
    } 

    /**
    * Returns the filepath of the cached template output
    * 
    * The filepath is determined by the actual resource handler of the cacher
    * 
    * @return string the cache filepath
    */
    public function getCachedFilepath ()
    {
        return $this->cached_filepath === null ?
        $this->cached_filepath = ($this->isEvaluated() || !$this->caching) ? false : $this->smarty->cache_resource_objects[$this->caching_type]->getCachedFilepath($this) :
        $this->cached_filepath;
    } 

    /**
    * Returns the timpestamp of the cached template output
    * 
    * The timestamp is determined by the actual resource handler of the cacher
    * 
    * @return integer the template timestamp
    */
    public function getCachedTimestamp ()
    {
        return $this->cached_timestamp === null ?
        $this->cached_timestamp = ($this->isEvaluated() || !$this->caching) ? false : $this->smarty->cache_resource_objects[$this->caching_type]->getCachedTimestamp($this) :
        $this->cached_timestamp;
    } 

    /**
    * Returns the cached template output
    * 
    * @return string |booelan the template content or false if the file does not exist
    */
    public function getCachedContent ()
    {
        return $this->rendered_content === null ?
        $this->rendered_content = ($this->isEvaluated() || !$this->caching) ? false : $this->cacher_object->getCachedContents($this) :
        $this->rendered_content;
    } 

    /**
    * Writes the cached template output
    */
    public function writeCachedContent ()
    { 
        // build file dependency string
        $this->file_dependency_string = '<?php /*' . serialize($this->file_dependency) . "*/ ?>\n";
        $this->rendered_content = $this->file_dependency_string . $this->dir_acc_sec_string . $this->rendered_content;
        return ($this->isEvaluated() || !$this->caching) ? false : $this->cacher_object->writeCachedContent($this);
    } 

    /**
    * Checks of a valid version redered HTML output is in the cache
    * 
    * If the cache is valid the contents is stored in the template object
    * 
    * @return boolean true if cache is valid
    */
    public function isCached ()
    {
        if ($this->isCached === null) {
            $this->isCached = false;
            if ($this->caching && !$this->isEvaluated()) {
                if (!$this->mustCompile() && $this->getTemplateTimestamp() <= $this->getCachedTimestamp() && ((time() <= $this->getCachedTimestamp() + $this->caching_lifetime) || $this->caching_lifetime < 0)) {
                    $this->rendered_content = $this->cacher_object->getCachedContents($this);
                    $this->isCached = true;
                } 
            } 
        } 
        return $this->isCached;
    } 

    /**
    * Render the output using the compiled template or the PHP template source
    * 
    * The rendering process is accomplished by just including the PHP files.
    * The only exceptions are evaluated templates (string template). Their code has 
    * to be evaluated
    */
    public function renderTemplate ()
    {
        if ($this->usesCompiler()) {
            if ($this->mustCompile()) {
                $this->compileTemplateSource();
            } 
            $_smarty_tpl = $this;
            $_start_time = $this->_get_time();
            ob_start();
            if ($this->isEvaluated()) {
                eval("?>" . $this->compiled_template);
            } else {
                include($this->getCompiledFilepath ());
            } 
        } else {
            // PHP template
            $_start_time = $this->_get_time(); 
            // Smarty variables as objects extract as objects
            $this->smarty->loadPlugin('Smarty_Internal_PHPVariableObjects');
            $_ptr = $this;
            do {
                foreach ($_ptr->tpl_vars as $_smarty_var => $_var_object) {
                    if (isset($_var_object->value)) {
                        $$_smarty_var = Smarty_Internal_PHPVariableObjects::createPHPVarObj($_var_object->value);
                    } 
                } 
                $_ptr = $_ptr->parent;
            } while ($_ptr != null);
            unset ($_smarty_var, $_smarty_value, $_ptr); 
            // special object for handling functions in PHP
            $_f = Smarty_Internal_PHPVariableObjects::createPHPVarObj(new PHP_Function_Handler($this), true);
            ob_start(); 
            // include PHP template
            include($this->getTemplateFilepath ());
        } 
        $this->render_time = $this->_get_time() - $_start_time;
        $this->rendered_content = ob_get_clean(); 
        // write to cache when nessecary
        if (!$this->isEvaluated() && $this->caching) {
            // write rendered template
            $this->writeCachedContent($this);
        } 
    } 

    /**
    * Returns the rendered HTML output 
    * 
    * If the cache is valid the cached content is used, otherwise
    * the output is rendered from the compiled template or PHP template source
    * 
    * @return string rendered HTML output
    */
    public function getRenderedTemplate ()
    { 
        // disable caching for evaluated code
        if ($this->isEvaluated()) {
            $this->caching = false;
        } 
        // checks if template exists
        $this->getTemplateFilepath(); 
        // read from cache or render
        if ($this->rendered_content === null && !$this->isCached()) {
            // render template (not loaded and not in cache)
            $this->renderTemplate();
        } 
        if ($this->caching && $this->usesCompiler()) {
            // cached output could contain nocache code
            $_start_time = $this->_get_time();
            $_smarty_tpl = $this;
            ob_start();
            eval("?>" . $this->rendered_content);
            $this->updateParentVariables();
            $this->cache_time = $this->_get_time() - $_start_time;
            return (isset($this->smarty->autoload_filters['output']) || isset($this->smarty->registered_filters['output']))?
            $this->smarty->filter_handler->execute('output', ob_get_clean()) : ob_get_clean();
        } else {
            $this->updateParentVariables();
            return (isset($this->smarty->autoload_filters['output']) || isset($this->smarty->registered_filters['output']))?
            $this->smarty->filter_handler->execute('output', $this->rendered_content) : $this->rendered_content;
        } 
    } 

    /**
    * parse a template resource in its name and type
    * 
    * @param string $template_resource template resource specification
    */
    private function parseResourceName($template_resource)
    {
        if (empty($template_resource))
            return false;
        if (strpos($template_resource, ':') === false) {
            // no resource given, use default
            $this->resource_type = $this->smarty->default_resource_type;
            $this->resource_name = $template_resource;
        } else {
            // get type and name from path
            list($this->resource_type, $this->resource_name) = explode(':', $template_resource, 2);
            if (strlen($this->resource_type) == 1) {
                // 1 char is not resource type, but part of filepath
                $this->resource_type = $this->smarty->default_resource_type;
                $this->resource_name = $template_resource;
            } else {
                $this->resource_type = strtolower($this->resource_type);
            } 
        } 
        // load resource handler if required
        if (!isset($this->resource_objects[$this->resource_type])) {
            // try sysplugins dir first
            $_resource_class = "Smarty_Internal_Resource_{$this->resource_type}";
            if ($this->smarty->loadPlugin($_resource_class)) {
                $this->resource_objects[$this->resource_type] = new $_resource_class;
            } else {
                // try plugins dir
                $_resource_class = "Smarty_Resource_{$this->resource_type}";
                if ($this->smarty->loadPlugin($_resource_class)) {
                    $this->resource_objects[$this->resource_type] = new $_resource_class;
                } else {
                    // try streams
                    $_known_stream = stream_get_wrappers();
                    if (in_array($this->resource_type, $_known_stream)) {
                        // is known stream
                        if (!isset($this->resource_objects['stream'])) {
                            $this->smarty->loadPlugin('Smarty_Internal_Resource_Stream');
                            $this->resource_objects['stream'] = new Smarty_Internal_Resource_Stream;
                        } 
                        $this->resource_type = 'stream';
                        $this->resource_name = str_replace(':', '://', $template_resource);
                    } else {
                        throw new Exception('Unkown resource type \'' . $this->resource_type . '\'');
                    } 
                } 
            } 
        } 
        // cache template object under a unique ID
        // do not cache string resources
        if ($this->resource_type != 'string') {
            Smarty::$template_objects[$this->buildTemplateId ($this->template_resource, $this->cache_id, $this->compile_id)] = $this;
        } 
        return true;
    } 

    /**
    * get system filepath to template
    */
    public function buildTemplateFilepath ($file = null)
    {
        if ($file == null) {
            $file = $this->resource_name;
        } 
        foreach((array)$this->smarty->template_dir as $_template_dir) {
            if (substr($_template_dir, -1) != DIRECTORY_SEPARATOR) {
                $_template_dir .= DIRECTORY_SEPARATOR;
            } 

            $_filepath = $_template_dir . $file;
            if (file_exists($_filepath))
                return $_filepath;
        } 
        if (file_exists($file)) return $file; 
        // no tpl file found
        if (!empty($this->smarty->default_template_handler_func)) {
            if (!is_callable($this->smarty->default_template_handler_func)) {
                throw new Exception("Default template handler not callable");
            } else {
                $_return = call_user_func_array($this->smarty->default_template_handler_func,
                    array($this->resource_type, $this->resource_name, &$this->template_source, &$this->template_timestamp, &$this));
                if ($_return == true) {
                    return $_filepath;
                } 
            } 
        } 
        throw new Exception("Unable to load template \"{$file}\"");
        return false;
    } 

    /**
    * Update Smarty variables in parent variable object
    */
    public function updateParentVariables ($scope = SMARTY_LOCAL_SCOPE)
    {
        foreach ($this->tpl_vars as $_key => $_value) {
            // copy global vars back to parent
            if (isset($this->parent) && ($scope == SMARTY_PARENT_SCOPE || $this->tpl_vars[$_key]->scope == SMARTY_PARENT_SCOPE)) {
                if (isset($this->parent->tpl_vars[$_key])) {
                    // variable is already defined in parent, copy value
                    $this->parent->tpl_vars[$_key]->value = $this->tpl_vars[$_key]->value;
                } else {
                    // create variable in parent
                    $this->parent->tpl_vars[$_key] = clone $_value;
                    $this->smarty->tpl_vars[$_key]->scope = SMARTY_LOCAL_SCOPE;
                } 
            } 
            if ($scope == SMARTY_ROOT_SCOPE || $this->tpl_vars[$_key]->scope == SMARTY_ROOT_SCOPE) {
                $_ptr = $this; 
                // find  root
                while ($_ptr->parent != null) {
                    $_ptr = $_ptr->parent;
                } 
                if (isset($_ptr->tpl_vars[$_key])) {
                    // variable is already defined in root, copy value
                    $_ptr->tpl_vars[$_key]->value = $this->tpl_vars[$_key]->value;
                } else {
                    // create variable in root
                    $_ptr->tpl_vars[$_key] = clone $_value;
                    $_ptr->tpl_vars[$_key]->scope = SMARTY_LOCAL_SCOPE;
                } 
            } 
            if ($scope == SMARTY_GLOBAL_SCOPE || $this->tpl_vars[$_key]->scope == SMARTY_GLOBAL_SCOPE) {
                if (isset($this->smarty->global_tpl_vars[$_key])) {
                    // variable is already defined in root, copy value
                    $this->smarty->global_tpl_vars[$_key]->value = $this->tpl_vars[$_key]->value;
                } else {
                    // create variable in root
                    $this->smarty->global_tpl_vars[$_key] = clone $_value;
                } 
                $this->smarty->global_tpl_vars[$_key]->scope = SMARTY_LOCAL_SCOPE;
            } 
        } 
    } 
} 

/**
* wrapper for template class
*/
class Smarty_Template extends Smarty_Internal_Template {
} 

?>
