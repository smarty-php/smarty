<?php

/**
* Smarty Internal Plugin Template
* 
* This file contains the Smarty template engine
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
    public $compiler_object = null;
    public $cacher_object = null; 
    // Smarty parameter
    public $cache_id = null;
    public $compile_id = null;
    public $caching = null;
    public $cache_lifetime = null;
    public $cacher_class = null;
    public $caching_type = null;
    public $force_compile = null; 
    public $forceNocache = false; 
    // Template resource
    public $template_resource = null;
    public $resource_type = null;
    public $resource_name = null;
    private $resource_object = null;
    private $usesCompiler = null;
    private $isEvaluated = null;
    private $isExisting = null; 
    // Template source
    public $template_filepath = null;
    public $template_source = null;
    private $template_timestamp = null; 
    // Compiled template
    private $compiled_filepath = null;
    public $compiled_template = null;
    private $compiled_timestamp = null;
    public $mustCompile = null;
    public $suppressHeader = false;
    public $suppressFileDependency = false;
    public $extract_code = false;
    public $extracted_compiled_code = ''; 
    // Rendered content
    public $rendered_content = null; 
    // Cache file
    private $cached_filepath = null;
    private $cached_timestamp = null;
    private $isCached = null;
    private $cache_resource_object = null; 
    // template variables
    public $tpl_vars = array();
    public $parent = null;
    public $config_vars = array(); 
    // storage for plugin
    public $plugin_data = array(); 
    // special properties
    public $properties = array(); 
    // storage for block data
    public $block_data = array();

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
    public function __construct($template_resource, $smarty, $_parent = null, $_cache_id = null, $_compile_id = null)
    {
        $this->smarty = $smarty; 
        // Smarty parameter
        $this->cache_id = $_cache_id === null ? $this->smarty->cache_id : $_cache_id;
        $this->compile_id = $_compile_id === null ? $this->smarty->compile_id : $_compile_id;
        $this->force_compile = $this->smarty->force_compile;
        $this->caching = $this->smarty->caching;
        $this->cache_lifetime = $this->smarty->cache_lifetime;
        $this->force_cache = $this->smarty->force_cache;
        $this->cacher_class = $this->smarty->cacher_class;
        $this->caching_type = $this->smarty->default_caching_type;
        $this->security = $this->smarty->security;
        $this->cache_resource_class = 'Smarty_Internal_CacheResource_' . ucfirst($this->caching_type);
        $this->parent = $_parent;
        $this->properties['file_dependency'] = array(); 
        // dummy local smarty variable
        $this->tpl_vars['smarty'] = new Smarty_Variable; 
        // Template resource
        $this->template_resource = $template_resource; 
        // parse resource name
        if (!$this->parseResourceName ($template_resource, $this->resource_type, $this->resource_name, $this->resource_object)) {
            throw new Exception ("Unable to parse resource name \"{$template_resource}\"");
        } 
        // load cache resource
        if (!$this->isEvaluated() && $this->caching) {
            $this->cache_resource_object = new $this->cache_resource_class($this->smarty);
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
        $this->template_filepath = $this->resource_object->getTemplateFilepath($this) :
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
        $this->template_timestamp = $this->resource_object->getTemplateTimestamp($this) :
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
            if (!$this->resource_object->getTemplateSource($this)) {
                throw new Exception("Unable to read template {$this->resource_type} '{$this->resource_name}'");
            } 
        } 
        return $this->template_source;
    } 

    /**
    * Returns if the  template is existing
    * 
    * The status is determined by the actual resource handler
    * 
    * @return boolean true if the template exists
    */
    public function isExisting ($error = false)
    {
        if ($this->isExisting === null) {
            $this->isExisting = $this->resource_object->isExisting($this);
        } 
        if (!$this->isExisting && $error) {
            throw new Exception("Unable to load template {$this->resource_type} '{$this->resource_name}'");
        } 
        return $this->isExisting;
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
        $this->usesCompiler = $this->resource_object->usesCompiler() :
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
        $this->isEvaluated = $this->resource_object->isEvaluated() :
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
        $this->isExisting(true);
        if ($this->mustCompile === null) {
            $this->mustCompile = ($this->usesCompiler() && ($this->force_compile || $this->isEvaluated() || $this->getCompiledTimestamp () === false ||
//                    ($this->smarty->compile_check && $this->getCompiledTimestamp () !== $this->getTemplateTimestamp ())));
                    ($this->smarty->compile_check && $this->getCompiledTimestamp () < $this->getTemplateTimestamp ())));
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
        ($this->compiled_filepath = !$this->isEvaluated() ? $this->resource_object->getCompiledFilepath($this) : false) :
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
                if ($this->compiled_template === null) {
                    $this->compiled_template = !$this->isEvaluated() && $this->usesCompiler() ? file_get_contents($this->getCompiledFilepath()) : false;
                } 
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
        if (!$this->isEvaluated) {
            $this->properties['file_dependency']['F' . abs(crc32($this->getTemplateFilepath()))] = array($this->getTemplateFilepath(), $this->getTemplateTimestamp());
        } 
        if ($this->smarty->debugging) {
            Smarty_Internal_Debug::start_compile($this);
        } 
        // compile template
        if (!is_object($this->compiler_object)) {
            // load compiler
            $this->smarty->loadPlugin($this->resource_object->compiler_class);
            $this->compiler_object = new $this->resource_object->compiler_class($this->resource_object->template_lexer_class, $this->resource_object->template_parser_class, $this->smarty); 
            // load cacher
            if ($this->caching) {
                $this->smarty->loadPlugin($this->cacher_class);
                $this->cacher_object = new $this->cacher_class($this->smarty);
            } 
        } 
        // call compiler
        if ($this->compiler_object->compileTemplate($this)) {
            // compiling succeded
            if (!$this->isEvaluated()) {
                // write compiled template
                Smarty_Internal_Write_File::writeFile($this->getCompiledFilepath(), $this->compiled_template, $this->smarty); 
                // make template and compiled file timestamp match
/**
                $this->compiled_timestamp = null;
                touch($this->getCompiledFilepath(), $this->getTemplateTimestamp()); 
                // daylight saving time problem on windows
                if ($this->template_timestamp != $this->getCompiledTimestamp()) {
                    touch($this->getCompiledFilepath(), 2 * $this->template_timestamp - $this->compiled_timestamp);
                }
**/ 
            } 
        } else {
            // error compiling template
            throw new Exception("Error compiling template {$this->getTemplateFilepath ()}");
            return false;
        } 
        if ($this->smarty->debugging) {
            Smarty_Internal_Debug::end_compile($this);
        } 
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
        $this->cached_filepath = ($this->isEvaluated() || !$this->caching) ? false : $this->cache_resource_object->getCachedFilepath($this) :
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
        $this->cached_timestamp = ($this->isEvaluated() || !$this->caching) ? false : $this->cache_resource_object->getCachedTimestamp($this) :
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
        $this->rendered_content = ($this->isEvaluated() || !$this->caching) ? false : $this->cache_resource_object->getCachedContents($this) :
        $this->rendered_content;
    } 

    /**
    * Writes the cached template output
    */
    public function writeCachedContent ()
    { 
        // build file dependency string
        $this->properties['cache_lifetime'] = $this->cache_lifetime;
        return ($this->isEvaluated() || !$this->caching) ? false : $this->cache_resource_object->writeCachedContent($this, $this->createPropertyHeader() . $this->rendered_content);
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
            if ($this->caching && !$this->isEvaluated() && !$this->force_compile && !$this->force_cache) {
                if ($this->getCachedTimestamp() === false) {
                    return $this->isCached;
                } 
                if ($this->caching === SMARTY_CACHING_LIFETIME_SAVED || ($this->caching == SMARTY_CACHING_LIFETIME_CURRENT && (time() <= ($this->getCachedTimestamp() + $this->cache_lifetime) || $this->cache_lifetime < 0))) {
                    if ($this->smarty->debugging) {
                        Smarty_Internal_Debug::start_cache($this);
                    } 
                    $this->rendered_content = $this->cache_resource_object->getCachedContents($this);
                    if ($this->smarty->debugging) {
                        Smarty_Internal_Debug::end_cache($this);
                    } 
                    if ($this->caching === SMARTY_CACHING_LIFETIME_SAVED && $this->properties['cache_lifetime'] > 0 && (time() > ($this->getCachedTimestamp() + $this->properties['cache_lifetime']))) {
                        $this->rendered_content = null;
                        return $this->isCached;
                    } 
                    if (!empty($this->properties['file_dependency']) && $this->smarty->compile_check) {
                        foreach ($this->properties['file_dependency'] as $_file_to_check) {
                            $this->getResourceTypeName($_file_to_check[0], $resource_type, $resource_name);
                            If ($resource_type == 'file') {
                                $mtime = filemtime($_file_to_check[0]);
                            } else {
                                $resource_handler = $this->loadTemplateResourceHandler($resource_type);
                                $mtime = $resource_handler->getTemplateTimestampTypeName($resource_type, $resource_name);
                            } 
                            // If ($mtime > $this->getCachedTimestamp()) {
                            If ($mtime > $_file_to_check[1]) {
                                $this->rendered_content = null;
                                $this->properties['file_dependency'] = array();
                                return $this->isCached;
                            } 
                        } 
                    } 
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
            if ($this->mustCompile() && $this->compiled_template === null) {
                $this->compileTemplateSource();
            } 
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::start_render($this);
            } 
            $_smarty_tpl = $this;
            ob_start();
            if ($this->isEvaluated()) {
                eval("?>" . $this->compiled_template);
            } else {
                include($this->getCompiledFilepath ()); 
                // check file dependencies at compiled code
                if ($this->smarty->compile_check) {
                    if (!empty($this->properties['file_dependency'])) {
                        $this->mustCompile = false;
                        foreach ($this->properties['file_dependency'] as $_file_to_check) {
                            $this->getResourceTypeName($_file_to_check[0], $resource_type, $resource_name);
                            If ($resource_type == 'file') {
                                $mtime = filemtime($_file_to_check[0]);
                            } else {
                                $resource_handler = $this->loadTemplateResourceHandler($resource_type);
                                $mtime = $resource_handler->getTemplateTimestampTypeName($resource_type, $resource_name);
                            } 
//                            If ($mtime != $_file_to_check[1]) {
                            If ($mtime > $_file_to_check[1]) {
                                $this->properties['file_dependency'] = array();
                                $this->mustCompile = true;
                                break;
                            } 
                        } 
                        if ($this->mustCompile) {
                            // recompile and render again
                            ob_get_clean();
                            $this->compileTemplateSource();
                            ob_start();
                            include($this->getCompiledFilepath ());
                        } 
                    } 
                } 
            } 
        } else {
            if (is_callable(array($this->resource_object, 'renderUncompiled'))) {
                if ($this->smarty->debugging) {
                    Smarty_Internal_Debug::start_render($this);
                } 
                ob_start();
                $this->resource_object->renderUncompiled($this);
            } else {
                throw new Exception("Resource '$this->resource_type' must have 'renderUncompiled' methode");
            } 
        } 
        $this->rendered_content = ob_get_clean();
        if (!$this->isEvaluated) {
            $this->properties['file_dependency']['F' . abs(crc32($this->getTemplateFilepath()))] = array($this->getTemplateFilepath(), $this->getTemplateTimestamp());
        } 
        if ($this->parent instanceof Smarty_Template or $this->parent instanceof Smarty_Internal_Template) {
            // var_dump('merge ', $this->parent->getTemplateFilepath(), $this->parent->properties['file_dependency'], $this->getTemplateFilepath(), $this->properties['file_dependency']);
            $this->parent->properties['file_dependency'] = array_merge($this->parent->properties['file_dependency'], $this->properties['file_dependency']);
        } 
        if ($this->smarty->debugging) {
            Smarty_Internal_Debug::end_render($this);
        } 
        // write to cache when nessecary
        if (!$this->isEvaluated() && ($this->caching == SMARTY_CACHING_LIFETIME_SAVED || $this->caching == SMARTY_CACHING_LIFETIME_CURRENT)) {
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::start_cache($this);
            } 
            // write rendered template
            $this->writeCachedContent($this); 
            // cache file may contain nocache code. read it back for processing
            $this->rendered_content = $this->cache_resource_object->getCachedContents($this);
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::end_cache($this);
            } 
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
        $this->isExisting(true); 
        // read from cache or render
        if ($this->rendered_content === null && !$this->isCached()) {
            // render template (not loaded and not in cache)
            $this->renderTemplate();
        } 
        $this->updateParentVariables();
        return $this->rendered_content;
    } 

    /**
    * Parse a template resource in its name and type
    * Load required resource handler
    * 
    * @param string $template_resource template resource specification
    * @param string $resource_type return resource type
    * @param string $resource_name return resource name
    * @param object $resource_handler return resource handler object
    */
    public function parseResourceName($template_resource, &$resource_type, &$resource_name, &$resource_handler)
    {
        if (empty($template_resource))
            return false;
        $this->getResourceTypeName($template_resource, $resource_type, $resource_name);
        $resource_handler = $this->loadTemplateResourceHandler($resource_type); 
        // cache template object under a unique ID
        // do not cache string resources
        // *****        if ($resource_type != 'string' && $this->smarty->caching) {
        if ($resource_type != 'string') {
            $this->smarty->template_objects[$this->buildTemplateId ($this->template_resource, $this->cache_id, $this->compile_id)] = $this;
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
            if (strpos('/\\', substr($_template_dir, -1)) === false) {
                $_template_dir .= DS;
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
                    array($this->resource_type, $this->resource_name, &$this->template_source, &$this->template_timestamp, $this));
                if ($_return == true) {
                    return $file;
                } elseif (is_string($_return)) {
                    return $_return;
                }
            } 
        } 
        // throw new Exception("Unable to load template \"{$file}\"");
        return false;
    } 

    /**
    * Decode saved properties from compiled template and cache files
    */
    public function decodeProperties ($properties)
    {
        $prop = unserialize($properties);
        if (isset($prop['cache_lifetime'])) {
            $this->properties['cache_lifetime'] = $prop['cache_lifetime'];
        } 
        if (isset($prop['file_dependency'])) {
            $this->properties['file_dependency'] = array_merge($this->properties['file_dependency'], $prop['file_dependency']); 
            // $this->properties['file_dependency'] = array_unique($this->properties['file_dependency']);
        } 
        if (!empty($prop['function'])) {
            foreach ($prop['function'] as $_name => $_data) {
                $this->smarty->template_functions[$_name]['compiled'] = str_replace('_%n', "\n", $_data['compiled']);
                $this->smarty->template_functions[$_name]['parameter'] = $_data['parameter'];
            } 
        } 
    } 

    /**
    * Update Smarty variables in parent variable object
    */
    public function updateParentVariables ($scope = SMARTY_LOCAL_SCOPE)
    {
        foreach ($this->tpl_vars as $_key => $_variable) {
            // copy global vars back to parent
            if (isset($this->parent) && ($scope == SMARTY_PARENT_SCOPE || $this->tpl_vars[$_key]->scope == SMARTY_PARENT_SCOPE)) {
                if (isset($this->parent->tpl_vars[$_key])) {
                    // variable is already defined in parent, copy value
                    $this->parent->tpl_vars[$_key]->value = $this->tpl_vars[$_key]->value;
                } else {
                    // create variable in parent
                    $this->parent->tpl_vars[$_key] = clone $_variable;
                    $this->parent->tpl_vars[$_key]->scope = SMARTY_LOCAL_SCOPE;
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
                    $_ptr->tpl_vars[$_key] = clone $_variable;
                    $_ptr->tpl_vars[$_key]->scope = SMARTY_LOCAL_SCOPE;
                } 
            } 
            if ($scope == SMARTY_GLOBAL_SCOPE || $this->tpl_vars[$_key]->scope == SMARTY_GLOBAL_SCOPE) {
                if (isset($this->smarty->global_tpl_vars[$_key])) {
                    // variable is already defined in root, copy value
                    $this->smarty->global_tpl_vars[$_key]->value = $this->tpl_vars[$_key]->value;
                } else {
                    // create variable in root
                    $this->smarty->global_tpl_vars[$_key] = clone $_variable;
                } 
                $this->smarty->global_tpl_vars[$_key]->scope = SMARTY_LOCAL_SCOPE;
            } 
        } 
    } 

    /**
    * Split a template resource in its name and type
    * 
    * @param string $template_resource template resource specification
    * @param string $resource_type return resource type
    * @param string $resource_name return resource name
    */
    private function getResourceTypeName ($template_resource, &$resource_type, &$resource_name)
    {
        if (strpos($template_resource, ':') === false) {
            // no resource given, use default
            $resource_type = $this->smarty->default_resource_type;
            $resource_name = $template_resource;
        } else {
            // get type and name from path
            list($resource_type, $resource_name) = explode(':', $template_resource, 2);
            if (strlen($resource_type) == 1) {
                // 1 char is not resource type, but part of filepath
                $resource_type = 'file';
                $resource_name = $template_resource;
            } else {
                $resource_type = strtolower($resource_type);
            } 
        } 
    } 

    /**
    * Load template resource handler by type
    * 
    * @param string $resource_type template resource type
    * @return object resource handler object
    */
    private function loadTemplateResourceHandler ($resource_type)
    { 
        // load resource handler if required
        if (!isset($this->smarty->resource_objects[$resource_type])) {
            // try registered resource
            if (isset($this->smarty->_plugins['resource'][$resource_type])) {
                return $this->smarty->resource_objects[$resource_type] = new Smarty_Internal_Resource_Registered($this->smarty);
            } else {
                // try sysplugins dir
                $_resource_class = 'Smarty_Internal_Resource_' . $resource_type;
                if ($this->smarty->loadPlugin($_resource_class)) {
                    return $this->smarty->resource_objects[$resource_type] = new $_resource_class($this->smarty);
                } else {
                    // try plugins dir
                    $_resource_class = 'Smarty_Resource_' . $resource_type;
                    if ($this->smarty->loadPlugin($_resource_class)) {
                        if (class_exists($_resource_class, false)) {
                            return $this->smarty->resource_objects[$resource_type] = new $_resource_class($this->smarty);
                        } else {
                            $this->smarty->register_resource($resource_type,
                                array("smarty_resource_{$resource_type}_source",
                                    "smarty_resource_{$resource_type}_timestamp",
                                    "smarty_resource_{$resource_type}_secure",
                                    "smarty_resource_{$resource_type}_trusted"));
                            return $this->smarty->resource_objects[$resource_type] = new Smarty_Internal_Resource_Registered($this->smarty);
                        } 
                    } else {
                        // try streams
                        $_known_stream = stream_get_wrappers();
                        if (in_array($resource_type, $_known_stream)) {
                            // is known stream
                            if ($this->smarty->security) {
                                $this->smarty->security_handler->isTrustedStream($resource_type);
                            } 
                            return $this->smarty->resource_objects[$resource_type] = new Smarty_Internal_Resource_Stream($this->smarty);
                        } else {
                            throw new Exception('Unkown resource type \'' . $resource_type . '\'');
                        } 
                    } 
                } 
            } 
        } else {
            return $this->smarty->resource_objects[$resource_type];
        } 
    } 

    /**
    * Create property header
    */
    public function createPropertyHeader ()
    {
        $directory_security = $this->smarty->direct_access_security ? "<?php if(!defined('SMARTY_DIR')) exit('no direct access allowed'); ?>\n" : '';
        $properties_string = "<?php \$_smarty_tpl->decodeProperties('" . str_replace("'", '"', (serialize($this->properties))) . "'); ?>\n";
        return $directory_security . $properties_string;
    } 

    /**
    * wrapper for display
    */
    public function display ()
    {
        return $this->smarty->display($this);
    } 

    /**
    * wrapper for fetch
    */
    public function fetch ()
    {
        return $this->smarty->fetch($this);
    } 
    /**
    * wrapper for is_cached
    */
    public function is_cached ()
    {
        return $this->iscached($this);
    } 
} 

/**
* wrapper for template class
*/
class Smarty_Template extends Smarty_Internal_Template {
} 

?>
