<?php

namespace Smarty\Resource;

use Smarty\Smarty;

/**
 * Smarty Internal Plugin Resource Eval
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Uwe Tews
 * @author     Rodney Rehm
 */

/**
 * Smarty Internal Plugin Resource Eval
 * Implements the strings as resource for Smarty template
 * {@internal unlike string-resources the compiled state of eval-resources is NOT saved for subsequent access}}
 *
 * @package    Smarty
 * @subpackage TemplateResources
 */
class StringEval extends RecompiledPlugin
{
    /**
     * populate Source Object with meta data from Resource
     *
     * @param \Smarty\Template\Source   $source    source object
     * @param \Smarty\Template $_template template object
     *
     * @return void
     */
    public function populate(\Smarty\Template\Source $source, \Smarty\Template $_template = null)
    {
        $source->uid = $source->filepath = sha1($source->name);
        $source->timestamp = $source->exists = true;
    }

    /**
     * Load template's source from $resource_name into current template object
     *
     * @param \Smarty\Template\Source $source source object
     *
     * @return string                 template source
     *@uses decode() to decode base64 and urlencoded template_resources
     *
     */
    public function getContent(\Smarty\Template\Source $source)
    {
        return $this->decode($source->name);
    }

    /**
     * decode base64 and urlencode
     *
     * @param string $string template_resource to decode
     *
     * @return string decoded template_resource
     */
    protected function decode($string)
    {
        // decode if specified
        if (($pos = strpos($string, ':')) !== false) {
            if (!strncmp($string, 'base64', 6)) {
                return base64_decode(substr($string, 7));
            } elseif (!strncmp($string, 'urlencode', 9)) {
                return urldecode(substr($string, 10));
            }
        }
        return $string;
    }

    /**
     * modify resource_name according to resource handlers specifications
     *
     * @param Smarty  $smarty        Smarty instance
     * @param string  $resource_name resource_name to make unique
     * @param boolean $isConfig      flag for config resource
     *
     * @return string unique resource name
     */
    public function buildUniqueResourceName(Smarty $smarty, $resource_name, $isConfig = false)
    {
        return get_class($this) . '#' . $this->decode($resource_name);
    }

    /**
     * Determine basename for compiled filename
     *
     * @param \Smarty\Template\Source $source source object
     *
     * @return string                 resource's basename
     */
    public function getBasename(\Smarty\Template\Source $source)
    {
        return '';
    }
}
