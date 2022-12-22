<?php

namespace Smarty\Resource;

/**
 * Smarty Internal Plugin Resource Extends
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Uwe Tews
 * @author     Rodney Rehm
 */

/**
 * Smarty Internal Plugin Resource Extends
 * Implements the file system as resource for Smarty which {extend}s a chain of template files templates
 *
 * @package    Smarty
 * @subpackage TemplateResources
 */
class ExtendsPlugin extends BasePlugin
{

    /**
     * populate Source Object with metadata from Resource
     *
     * @param \Smarty\Template\Source   $source    source object
     * @param \Smarty\Template $_template template object
     *
     * @throws \SmartyException
     */
    public function populate(\Smarty\Template\Source $source, \Smarty\Template $_template = null)
    {
        $uid = '';
        $sources = array();
        $components = explode('|', $source->name);
        $smarty = &$source->smarty;
        $exists = true;
        foreach ($components as $component) {
            /* @var \Smarty\Template\Source $_s */
            $_s = \Smarty\Template\Source::load(null, $smarty, $component);
            if ($_s->type === 'php') {
                throw new \SmartyException("Resource type {$_s->type} cannot be used with the extends resource type");
            }
            $sources[ $_s->uid ] = $_s;
            $uid .= $_s->filepath;
            if ($_template) {
                $exists = $exists && $_s->exists;
            }
        }
        $source->components = $sources;
        $source->filepath = $_s->filepath;
        $source->uid = sha1($uid . $source->smarty->_joined_template_dir);
        $source->exists = $exists;
        if ($_template) {
            $source->timestamp = $_s->timestamp;
        }
    }

    /**
     * populate Source Object with timestamp and exists from Resource
     *
     * @param \Smarty\Template\Source $source source object
     */
    public function populateTimestamp(\Smarty\Template\Source $source)
    {
        $source->exists = true;
        /* @var \Smarty\Template\Source $_s */
        foreach ($source->components as $_s) {
            $source->exists = $source->exists && $_s->exists;
        }
        $source->timestamp = $source->exists ? $_s->getTimeStamp() : false;
    }

    /**
     * Load template's source from files into current template object
     *
     * @param \Smarty\Template\Source $source source object
     *
     * @return string template source
     * @throws \SmartyException if source cannot be loaded
     */
    public function getContent(\Smarty\Template\Source $source)
    {
        if (!$source->exists) {
            throw new \SmartyException("Unable to load template '{$source->type}:{$source->name}'");
        }
        $_components = array_reverse($source->components);
        $_content = '';
        /* @var \Smarty\Template\Source $_s */
        foreach ($_components as $_s) {
            // read content
            $_content .= $_s->getContent();
        }
        return $_content;
    }

    /**
     * Determine basename for compiled filename
     *
     * @param \Smarty\Template\Source $source source object
     *
     * @return string resource's basename
     */
    public function getBasename(\Smarty\Template\Source $source)
    {
        return str_replace(':', '.', basename($source->filepath));
    }

    /*
      * Disable timestamp checks for extends resource.
      * The individual source components will be checked.
      *
      * @return bool
      */
    /**
     * @return bool
     */
    public function checkTimestamps()
    {
        return false;
    }
}
