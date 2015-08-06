<?php

/**
 * Smarty Resource Data Object
 * Meta Data Container for Template Files
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Rodney Rehm
 * @property string $content compiled content
 */
class Smarty_Template_Compiled
{
    /**
     * Compiled Filepath
     *
     * @var string
     */
    public $filepath = null;

    /**
     * Compiled Timestamp
     *
     * @var integer
     */
    public $timestamp = null;

    /**
     * Compiled Existence
     *
     * @var boolean
     */
    public $exists = false;

    /**
     * Compiled Content Loaded
     *
     * @var boolean
     */
    public $processed = false;

    /**
     * Code of recompiled template resource
     *
     * @var string|null
     */
    public $code = null;

    /**
     * unique function name for compiled template code
     *
     * @var string
     */
    public $unifunc = '';

    /**
     * create Compiled Object container
     */
    public function __construct()
    {
    }

    /**
     * get a Compiled Object of this source
     *
     * @param  Smarty_Internal_Template $_template template object
     *
     * @return Smarty_Template_Compiled compiled object
     */
    static function load($_template)
    {
        $smarty = $_template->smarty;
        $source = $_template->source;
        // check runtime cache
        if (!$source->recompiled && $smarty->resource_caching) {
            $_cache_key = $source->unique_resource . '#';
            if ($_template->caching) {
                $_cache_key .= 'caching#';
            }
            $_cache_key .= $_template->compile_id;
            if (isset($source->compileds[$_cache_key])) {
                return $source->compileds[$_cache_key];
            }
        }
        $compiled = new Smarty_Template_Compiled();
        if (method_exists($source->handler, 'populateCompiledFilepath')) {
            $source->handler->populateCompiledFilepath($compiled, $_template);
        } else {
            $compiled->populateCompiledFilepath($_template);
        }
        // runtime cache
        if (!$source->recompiled && $smarty->resource_caching) {
            $source->compileds[$_cache_key] = $compiled;
        }
        return $compiled;
    }

    /**
     * populate Compiled Object with compiled filepath
     *
     * @param Smarty_Internal_Template $_template template object
     **/
    public function populateCompiledFilepath(Smarty_Internal_Template $_template)
    {
        $_compile_id = isset($_template->compile_id) ? preg_replace('![^\w]+!', '_', $_template->compile_id) : null;
        if ($_template->source->isConfig) {
            $_flag = '_' .
                ((int) $_template->smarty->config_read_hidden + (int) $_template->smarty->config_booleanize * 2 +
                    (int) $_template->smarty->config_overwrite * 4);
        } else {
            $_flag = '_' .
                ((int) $_template->smarty->merge_compiled_includes + (int) $_template->smarty->escape_html * 2);
        }
        $_filepath = $_template->source->uid . $_flag;
        // if use_sub_dirs, break file into directories
        if ($_template->smarty->use_sub_dirs) {
            $_filepath = substr($_filepath, 0, 2) . DS . substr($_filepath, 2, 2) . DS . substr($_filepath, 4, 2) . DS .
                $_filepath;
        }
        $_compile_dir_sep = $_template->smarty->use_sub_dirs ? DS : '^';
        if (isset($_compile_id)) {
            $_filepath = $_compile_id . $_compile_dir_sep . $_filepath;
        }
        // caching token
        if ($_template->caching) {
            $_cache = '.cache';
        } else {
            $_cache = '';
        }
        $_compile_dir = $_template->smarty->getCompileDir();
        // set basename if not specified
        $_basename = $_template->source->handler->getBasename($_template->source);
        if ($_basename === null) {
            $_basename = basename(preg_replace('![^\w]+!', '_', $_template->source->name));
        }
        // separate (optional) basename by dot
        if ($_basename) {
            $_basename = '.' . $_basename;
        }

        $this->filepath = $_compile_dir . $_filepath . '.' . $_template->source->type . $_basename . $_cache . '.php';
        $this->exists = is_file($this->filepath);
        if (!$this->exists) {
            $this->timestamp = false;
        }
    }

    /**
     * load compiled template or compile from source
     *
     * @param Smarty_Internal_Template $_template
     *
     * @throws Exception
     */
    public function process(Smarty_Internal_Template $_template)
    {
        $_smarty_tpl = $_template;
        if ($_template->source->recompiled || !$_template->compiled->exists || $_template->smarty->force_compile ||
            ($_template->smarty->compile_check &&
                $_template->source->getTimeStamp() > $_template->compiled->getTimeStamp())
        ) {
            $this->compileTemplateSource($_template);
            $compileCheck = $_template->smarty->compile_check;
            $_template->smarty->compile_check = false;
            if ($_template->source->recompiled) {
                $level = ob_get_level();
                ob_start();
                try {
                    eval("?>" . $this->code);
                }
                catch (Exception $e) {
                    while (ob_get_level() > $level) {
                        ob_end_clean();
                    }
                    throw $e;
                }
                ob_get_clean();
                $this->code = null;
            } else {
                $this->loadCompiledTemplate($_template);
            }
            $_template->smarty->compile_check = $compileCheck;
        } else {
            include($_template->compiled->filepath);
            if ($_template->mustCompile) {
                $this->compileTemplateSource($_template);
                $compileCheck = $_template->smarty->compile_check;
                $_template->smarty->compile_check = false;
                $this->loadCompiledTemplate($_template);
                $_template->smarty->compile_check = $compileCheck;
            }
        }
        $this->unifunc = $_template->properties['unifunc'];
        $this->processed = true;
    }

    /**
     * Load fresh compiled template by including the PHP file
     * HHVM requires a work around because of a PHP incompatibility
     *
     * @param \Smarty_Internal_Template $_template
     */
    private function loadCompiledTemplate(Smarty_Internal_Template $_template)
    {
        if (function_exists('opcache_invalidate')) {
            opcache_invalidate($_template->compiled->filepath);
        }
        $_smarty_tpl = $_template;
        if (strpos(phpversion(), 'hhvm') !== false) {
            Smarty_Internal_Extension_Hhvm::includeHhvm($_template, $_template->compiled->filepath);
        } else {
            include($_template->compiled->filepath);
        }
    }

    /**
     * render compiled template code
     *
     * @param Smarty_Internal_Template $_template
     *
     * @return string
     * @throws Exception
     */
    public function render(Smarty_Internal_Template $_template)
    {

        if (!$this->processed) {
            $this->process($_template);
        }
        $_template->properties['unifunc'] = $this->unifunc;
        return $_template->getRenderedTemplateCode();
    }

    /**
     * compile template from source
     *
     * @param Smarty_Internal_Template $_template
     *
     * @return string
     * @throws Exception
     */
    public function compileTemplateSource(Smarty_Internal_Template $_template)
    {
        $_template->source->compileds = array();
        if (!$_template->source->recompiled) {
            $_template->properties['file_dependency'] = array();
        }
        // compile locking
        if (!$_template->source->recompiled) {
            if ($saved_timestamp = $_template->compiled->getTimeStamp()) {
                touch($_template->compiled->filepath);
            }
        }
        // call compiler
        try {
            $_template->loadCompiler();
            $code = $_template->compiler->compileTemplate($_template);
        }
        catch (Exception $e) {
            // restore old timestamp in case of error
            if (!$_template->source->recompiled && $saved_timestamp) {
                touch($_template->compiled->filepath, $saved_timestamp);
            }
            throw $e;
        }
        // compiling succeeded
        if ($_template->compiler->write_compiled_code) {
            // write compiled template
            $this->write($_template, $code);
            $code = '';
        }
        // release compiler object to free memory
        unset($_template->compiler);
        return $code;
    }

    /**
     * Write compiled code by handler
     *
     * @param Smarty_Internal_Template $_template template object
     * @param string                   $code      compiled code
     *
     * @return boolean success
     */
    public function write(Smarty_Internal_Template $_template, $code)
    {
        if (!$_template->source->recompiled) {
            $obj = new Smarty_Internal_Write_File();
            if ($obj->writeFile($this->filepath, $code, $_template->smarty) === true) {
                $this->timestamp = $this->exists = is_file($this->filepath);
                if ($this->exists) {
                    $this->timestamp = filemtime($this->filepath);
                    return true;
                }
            }
            return false;
        } else {
            $this->code = $code;
        }
        $this->timestamp = time();
        $this->exists = true;
        return true;
    }

    /**
     * Read compiled content from handler
     *
     * @param Smarty_Internal_Template $_template template object
     *
     * @return string content
     */
    public function read(Smarty_Internal_Template $_template)
    {
        if (!$_template->source->recompiled) {
            return file_get_contents($this->filepath);
        }
        return isset($this->content) ? $this->content : false;
    }

    /**
     * Get compiled time stamp
     *
     * @return int
     */
    public function getTimeStamp()
    {
        if ($this->exists && !isset($this->timestamp)) {
            $this->timestamp = @filemtime($this->filepath);
        }
        return $this->timestamp;
    }
}
