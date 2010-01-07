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
class Smarty_Internal_Template extends Smarty_Internal_Data {
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
    public $resource_object = null;
    private $isExisting = null;
    public $templateUid = '';
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
    public $has_nocache_code = false; 
    // Rendered content
    public $rendered_content = null; 
    // Cache file
    private $cached_filepath = null;
    public $cached_timestamp = null;
    private $isCached = null;
    private $cache_resource_object = null;
    private $cacheFileChecked = false; 
    // template variables
    public $tpl_vars = array();
    public $parent = null;
    public $config_vars = array(); 
    // storage for plugin
    public $plugin_data = array(); 
    // special properties
    public $properties = array ('file_dependency' => array(),
        'nocache_hash' => '',
        'function' => array()); 
    // storage for block data
    public $block_data = array(); 
    // required plugins
    public $required_plugins = array('compiled' => array(), 'cache' => array());

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
    public function __construct($template_resource, $smarty, $_parent = null, $_cache_id = null, $_compile_id = null, $_caching = null, $_cache_lifetime = null)
    {
        $this->smarty = &$smarty; 
        // Smarty parameter
        $this->cache_id = $_cache_id === null ? $this->smarty->cache_id : $_cache_id;
        $this->compile_id = $_compile_id === null ? $this->smarty->compile_id : $_compile_id;
        $this->force_compile = $this->smarty->force_compile;
        $this->caching = $_caching === null ? $this->smarty->caching : $_caching;
        if ($this->caching === true) $this->caching = SMARTY_CACHING_LIFETIME_CURRENT;
        $this->cache_lifetime = $_cache_lifetime === null ?$this->smarty->cache_lifetime : $_cache_lifetime;
        $this->force_cache = $this->smarty->force_cache;
        $this->security = $this->smarty->security;
        $this->parent = $_parent; 
        // dummy local smarty variable
        $this->tpl_vars['smarty'] = new Smarty_Variable; 
        // Template resource
        $this->template_resource = $template_resource; 
        // parse resource name
        if (!$this->parseResourceName ($template_resource, $this->resource_type, $this->resource_name, $this->resource_object)) {
            throw new Exception ("Unable to parse resource name \"{$template_resource}\"");
        } 
        // load cache resource
        if (!$this->resource_object->isEvaluated && ($this->caching == SMARTY_CACHING_LIFETIME_CURRENT || $this->caching == SMARTY_CACHING_LIFETIME_SAVED)) {
            $this->cache_resource_object = $this->smarty->loadCacheResource();
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
            $this->mustCompile = ($this->resource_object->usesCompiler && ($this->force_compile || $this->resource_object->isEvaluated || $this->getCompiledTimestamp () === false || 
                    // ($this->smarty->compile_check && $this->getCompiledTimestamp () !== $this->getTemplateTimestamp ())));
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
        ($this->compiled_filepath = !$this->resource_object->isEvaluated ? $this->resource_object->getCompiledFilepath($this) : false) :
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
        ($this->compiled_timestamp = (!$this->resource_object->isEvaluated && file_exists($this->getCompiledFilepath())) ? filemtime($this->getCompiledFilepath()) : false) :
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
                    $this->compiled_template = !$this->resource_object->isEvaluated && $this->resource_object->usesCompiler ? file_get_contents($this->getCompiledFilepath()) : false;
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
        if (!$this->resource_object->isEvaluated) {
            $this->properties['file_dependency'] = array();
            $this->properties['file_dependency'][$this->templateUid] = array($this->getTemplateFilepath(), $this->getTemplateTimestamp());
        } 
        if ($this->smarty->debugging) {
            Smarty_Internal_Debug::start_compile($this);
        } 
        // compile template
        if (!is_object($this->compiler_object)) {
            // load compiler
            $this->smarty->loadPlugin($this->resource_object->compiler_class);
            $this->compiler_object = new $this->resource_object->compiler_class($this->resource_object->template_lexer_class, $this->resource_object->template_parser_class, $this->smarty);
        } 
        // call compiler
        if ($this->compiler_object->compileTemplate($this)) {
            // compiling succeded
            if (!$this->resource_object->isEvaluated) {
                // write compiled template
                Smarty_Internal_Write_File::writeFile($this->getCompiledFilepath(), $this->compiled_template, $this->smarty); 
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
    * The filepath is determined by the actual cache resource
    * 
    * @return string the cache filepath
    */
    public function getCachedFilepath ()
    {
        return $this->cached_filepath === null ?
        $this->cached_filepath = ($this->resource_object->isEvaluated || !($this->caching == SMARTY_CACHING_LIFETIME_CURRENT || $this->caching == SMARTY_CACHING_LIFETIME_SAVED)) ? false : $this->cache_resource_object->getCachedFilepath($this) :
        $this->cached_filepath;
    } 

    /**
    * Returns the timpestamp of the cached template output
    * 
    * The timestamp is determined by the actual cache resource
    * 
    * @return integer the template timestamp
    */
    public function getCachedTimestamp ()
    {
        return $this->cached_timestamp === null ?
        $this->cached_timestamp = ($this->resource_object->isEvaluated || !($this->caching == SMARTY_CACHING_LIFETIME_CURRENT || $this->caching == SMARTY_CACHING_LIFETIME_SAVED)) ? false : $this->cache_resource_object->getCachedTimestamp($this) :
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
        $this->rendered_content = ($this->resource_object->isEvaluated || !($this->caching == SMARTY_CACHING_LIFETIME_CURRENT || $this->caching == SMARTY_CACHING_LIFETIME_SAVED)) ? false : $this->cache_resource_object->getCachedContents($this) :
        $this->rendered_content;
    } 

    /**
    * Writes the cached template output
    */
    public function writeCachedContent ($content)
    {
        if ($this->resource_object->isEvaluated || !($this->caching == SMARTY_CACHING_LIFETIME_CURRENT || $this->caching == SMARTY_CACHING_LIFETIME_SAVED)) {
            // don't write cache file
            return false;
        } 
        $this->properties['cache_lifetime'] = $this->cache_lifetime;
        return $this->cache_resource_object->writeCachedContent($this, $this->createPropertyHeader(true) . $content);
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
            if (($this->caching == SMARTY_CACHING_LIFETIME_CURRENT || $this->caching == SMARTY_CACHING_LIFETIME_SAVED) && !$this->resource_object->isEvaluated && !$this->force_compile && !$this->force_cache) {
                $cachedTimestamp = $this->getCachedTimestamp();
                if ($cachedTimestamp === false) {
                    return $this->isCached;
                } 
                if ($this->caching === SMARTY_CACHING_LIFETIME_SAVED || ($this->caching == SMARTY_CACHING_LIFETIME_CURRENT && (time() <= ($cachedTimestamp + $this->cache_lifetime) || $this->cache_lifetime < 0))) {
                    if ($this->smarty->debugging) {
                        Smarty_Internal_Debug::start_cache($this);
                    } 
                    $this->rendered_content = $this->cache_resource_object->getCachedContents($this);
                    if ($this->smarty->debugging) {
                        Smarty_Internal_Debug::end_cache($this);
                    } 
                    if ($this->cacheFileChecked) {
                        $this->isCached = true;
                        return $this->isCached;
                    } 
                    $this->cacheFileChecked = true;
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
        if ($this->resource_object->usesCompiler) {
            if ($this->mustCompile() && $this->compiled_template === null) {
                $this->compileTemplateSource();
            } 
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::start_render($this);
            } 
            $_smarty_tpl = $this;
            ob_start();
            if ($this->resource_object->isEvaluated) {
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
                            // If ($mtime != $_file_to_check[1]) {
                            If ($mtime > $_file_to_check[1]) {
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
        if (!$this->resource_object->isEvaluated && empty($this->properties['file_dependency'][$this->templateUid])) {
            $this->properties['file_dependency'][$this->templateUid] = array($this->getTemplateFilepath(), $this->getTemplateTimestamp());
        } 
        if ($this->parent instanceof Smarty_Template or $this->parent instanceof Smarty_Internal_Template) {
            $this->parent->properties['file_dependency'] = array_merge($this->parent->properties['file_dependency'], $this->properties['file_dependency']);
            $this->parent->required_plugins['compiled'] = array_merge($this->parent->required_plugins['compiled'], $this->required_plugins['compiled']);
            $this->parent->required_plugins['cache'] = array_merge($this->parent->required_plugins['cache'], $this->required_plugins['cache']);
        } 
        if ($this->smarty->debugging) {
            Smarty_Internal_Debug::end_render($this);
        } 
        // write to cache when nessecary
        if (!$this->resource_object->isEvaluated && ($this->caching == SMARTY_CACHING_LIFETIME_SAVED || $this->caching == SMARTY_CACHING_LIFETIME_CURRENT)) {
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::start_cache($this);
            } 
            $this->properties['has_nocache_code'] = false; 
            // get text between non-cached items
            $cache_split = preg_split("!/\*%%SmartyNocache:{$this->properties['nocache_hash']}%%\*\/(.+?)/\*/%%SmartyNocache:{$this->properties['nocache_hash']}%%\*/!s", $this->rendered_content); 
            // get non-cached items
            preg_match_all("!/\*%%SmartyNocache:{$this->properties['nocache_hash']}%%\*\/(.+?)/\*/%%SmartyNocache:{$this->properties['nocache_hash']}%%\*/!s", $this->rendered_content, $cache_parts);
            $output = ''; 
            // loop over items, stitch back together
            foreach($cache_split as $curr_idx => $curr_split) {
                // escape PHP tags in template content
                $output .= preg_replace('/(<%|%>|<\?php|<\?|\?>)/', '<?php echo \'$1\'; ?>', $curr_split);
                if (isset($cache_parts[0][$curr_idx])) {
                    $this->properties['has_nocache_code'] = true; 
                    // remove nocache tags from cache output
                    $output .= preg_replace("!/\*/?%%SmartyNocache:{$this->properties['nocache_hash']}%%\*/!", '', $cache_parts[0][$curr_idx]);
                } 
            } 
            // rendering (must be done before writing cache file because of {function} nocache handling)
            $_smarty_tpl = $this;
            ob_start();
            eval("?>" . $output);
            $this->rendered_content = ob_get_clean(); 
            // write cache file content
            $this->writeCachedContent($output); 
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::end_cache($this);
            } 
        } else {
            // var_dump('renderTemplate', $this->has_nocache_code, $this->template_resource, $this->properties['nocache_hash'], $this->parent->properties['nocache_hash'], $this->rendered_content);
            if ($this->has_nocache_code && !empty($this->properties['nocache_hash']) && !empty($this->parent->properties['nocache_hash'])) {
                // replace nocache_hash
                $this->rendered_content = preg_replace("/{$this->properties['nocache_hash']}/", $this->parent->properties['nocache_hash'], $this->rendered_content);
                $this->parent->has_nocache_code = $this->has_nocache_code;
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
        if ($this->resource_object->isEvaluated) {
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
        $this->isCached = null;
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
            $this->smarty->template_objects[crc32($this->template_resource . $this->cache_id . $this->compile_id)] = $this;
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
                if (is_string($_return)) {
                    return $_return;
                } elseif ($_return === true) {
                    return $file;
                } 
            } 
        } 
        // throw new Exception("Unable to load template \"{$file}\"");
        return false;
    } 

    /**
    * Update Smarty variables in other scopes
    */
    public function updateParentVariables ($scope = SMARTY_LOCAL_SCOPE)
    {
        $has_root = false;
        foreach ($this->tpl_vars as $_key => $_variable) {
            $_variable_scope = $this->tpl_vars[$_key]->scope;
            if ($scope == SMARTY_LOCAL_SCOPE && $_variable_scope == SMARTY_LOCAL_SCOPE) {
                continue;
            } 
            if (isset($this->parent) && ($scope == SMARTY_PARENT_SCOPE || $_variable_scope == SMARTY_PARENT_SCOPE)) {
                if (isset($this->parent->tpl_vars[$_key])) {
                    // variable is already defined in parent, copy value
                    $this->parent->tpl_vars[$_key]->value = $this->tpl_vars[$_key]->value;
                } else {
                    // create variable in parent
                    $this->parent->tpl_vars[$_key] = clone $_variable;
                    $this->parent->tpl_vars[$_key]->scope = SMARTY_LOCAL_SCOPE;
                } 
            } 
            if ($scope == SMARTY_ROOT_SCOPE || $_variable_scope == SMARTY_ROOT_SCOPE) {
                if ($this->parent == null) {
                    continue;
                } 
                if (!$has_root) {
                    // find  root
                    $root_ptr = $this;
                    while ($root_ptr->parent != null) {
                        $root_ptr = $root_ptr->parent;
                        $has_root = true;
                    } 
                } 
                if (isset($root_ptr->tpl_vars[$_key])) {
                    // variable is already defined in root, copy value
                    $root_ptr->tpl_vars[$_key]->value = $this->tpl_vars[$_key]->value;
                } else {
                    // create variable in root
                    $root_ptr->tpl_vars[$_key] = clone $_variable;
                    $root_ptr->tpl_vars[$_key]->scope = SMARTY_LOCAL_SCOPE;
                } 
            } 
            if ($scope == SMARTY_GLOBAL_SCOPE || $_variable_scope == SMARTY_GLOBAL_SCOPE) {
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
        // try registered resource
        if (isset($this->smarty->_plugins['resource'][$resource_type])) {
            return new Smarty_Internal_Resource_Registered($this->smarty);
        } else {
            // try sysplugins dir
            if (in_array($resource_type, array('file', 'string', 'extends', 'php', 'registered', 'stream'))) {
                $_resource_class = 'Smarty_Internal_Resource_' . $resource_type;
                return new $_resource_class($this->smarty);
            } else {
                // try plugins dir
                $_resource_class = 'Smarty_Resource_' . $resource_type;
                if ($this->smarty->loadPlugin($_resource_class)) {
                    if (class_exists($_resource_class, false)) {
                        return new $_resource_class($this->smarty);
                    } else {
                        $this->smarty->register_resource($resource_type,
                            array("smarty_resource_{$resource_type}_source",
                                "smarty_resource_{$resource_type}_timestamp",
                                "smarty_resource_{$resource_type}_secure",
                                "smarty_resource_{$resource_type}_trusted"));
                        return new Smarty_Internal_Resource_Registered($this->smarty);
                    } 
                } else {
                    // try streams
                    $_known_stream = stream_get_wrappers();
                    if (in_array($resource_type, $_known_stream)) {
                        // is known stream
                        if ($this->smarty->security) {
                            $this->smarty->security_handler->isTrustedStream($resource_type);
                        } 
                        return new Smarty_Internal_Resource_Stream($this->smarty);
                    } else {
                        throw new Exception('Unkown resource type \'' . $resource_type . '\'');
                    } 
                } 
            } 
        } 
    } 

    /**
    * Create property header
    */
    public function createPropertyHeader ($cache = false)
    {
        $plugins_string = ''; 
        // include code for plugins
        if (!$cache) {
            if (!empty($this->required_plugins['compiled'])) {
                $plugins_string = '<?php ';
                foreach($this->required_plugins['compiled'] as $plugin_name => $data) {
                    $plugin = 'smarty_' . $data['type'] . '_' . $plugin_name;
                    $plugins_string .= "if (!is_callable('{$plugin}')) include '{$data['file']}';\n";
                } 
                $plugins_string .= '?>';
            } 
            if (!empty($this->required_plugins['cache'])) {
                $this->has_nocache_code = true;
                $plugins_string .= "<?php echo '/*%%SmartyNocache:{$this->properties['nocache_hash']}%%*/<?php ";
                foreach($this->required_plugins['cache'] as $plugin_name => $data) {
                    $plugin = 'smarty_' . $data['type'] . '_' . $plugin_name;
                    $plugins_string .= "if (!is_callable(\'{$plugin}\')) include \'{$data['file']}\';\n";
                } 
                $plugins_string .= "?>/*/%%SmartyNocache:{$this->properties['nocache_hash']}%%*/';?>\n";
            } 
        } 
        // build property code
        $this->properties['has_nocache_code'] = $this->has_nocache_code;
        $properties_string = "<?php /*%%SmartyHeaderCode:{$this->properties['nocache_hash']}%%*/" ;
        if ($this->smarty->direct_access_security) {
            $properties_string .= "if(!defined('SMARTY_DIR')) exit('no direct access allowed');\n";
        } 
        if ($cache) {
            // remove compiled code of{function} definition
            unset($this->properties['function']);
            if (!empty($this->smarty->template_functions)) {
                // copy code of {function} tags called in nocache mode
                foreach ($this->smarty->template_functions as $name => $function_data) {
                    if (isset($function_data['called_nocache'])) {
                        unset($function_data['called_nocache'], $this->smarty->template_functions[$name]['called_nocache']);
                        $this->properties['function'][$name] = $function_data;
                    } 
                } 
            } 
        } 
        $properties_string .= "\$_smarty_tpl->decodeProperties(" . var_export($this->properties, true) . "); /*/%%SmartyHeaderCode%%*/?>\n";
        return $properties_string . $plugins_string;
    } 

    /**
    * Decode saved properties from compiled template and cache files
    */
    public function decodeProperties ($properties)
    {
        $this->has_nocache_code = $properties['has_nocache_code'];
        $this->properties['nocache_hash'] = $properties['nocache_hash'];
        if (isset($properties['cache_lifetime'])) {
            $this->properties['cache_lifetime'] = $properties['cache_lifetime'];
        } 
        if (isset($properties['file_dependency'])) {
            $this->properties['file_dependency'] = array_merge($this->properties['file_dependency'], $properties['file_dependency']);
        } 
        if (!empty($properties['function'])) {
            $this->properties['function'] = array_merge($this->properties['function'], $properties['function']);
            $this->smarty->template_functions = array_merge($this->smarty->template_functions, $properties['function']);
        } 
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
