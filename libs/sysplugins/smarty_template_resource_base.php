<?php

/**
 * Smarty Template Resource Base Object
 * Meta Data Container for Template Resource Files
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Rodney Rehm
 */
abstract class Smarty_Template_Resource_Base
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
     * Template Compile Id (Smarty_Internal_Template::$compile_id)
     *
     * @var string
     */
    public $compile_id = null;

    /**
     * Compiled Content Loaded
     *
     * @var boolean
     */
    public $processed = false;

    /**
     * unique function name for compiled template code
     *
     * @var string
     */
    public $unifunc = '';

    /**
     * flag if template does contain nocache code sections
     *
     * @var bool
     */
    public $has_nocache_code = false;

    /**
     * resource file dependency
     *
     * @var array
     */
    public $file_dependency = array();

    /**
     * Content buffer
     *
     * @var string
     */
    public $content = null;

    /**
     * required plugins
     *
     * @var array
     */
    public $required_plugins = array();

    /**
     * Known template functions
     *
     * @var array
     */
    public $tpl_function = array();

    /**
     * Process resource
     *
     * @param Smarty_Internal_Template $_template template object
     */
    abstract public function process(Smarty_Internal_Template $_template);

    /**
     * render template code
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
        return $_template->getRenderedTemplateCode($this->unifunc);
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
            $this->content = $code;
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
