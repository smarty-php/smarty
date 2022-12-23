<?php

namespace Smarty\Resource;

use Smarty\Smarty;

/**
 * Smarty Resource Plugin
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Rodney Rehm
 */

/**
 * Smarty Resource Plugin
 * Base implementation for resource plugins
 *
 * @package    Smarty
 * @subpackage TemplateResources
 *
 * @method renderUncompiled(\Smarty\Template\Source $source, \Smarty\Template $_template)
 * @method populateCompiledFilepath(\Smarty\Template\Compiled $compiled, \Smarty\Template $_template)
 * @method process(\Smarty\Template $_smarty_tpl)
 */
abstract class BasePlugin
{
    /**
     * resource types provided by the core
     *
     * @var array
     */
    public static $sysplugins = [
        'file'    => FilePlugin::class,
        'string'  => StringPlugin::class,
        'extends' => ExtendsPlugin::class,
        'stream'  => StreamPlugin::class,
        'eval'    => StringEval::class,
        'php'     => PhpPlugin::class,
    ];

    /**
     * Source is bypassing compiler
     *
     * @var boolean
     */
    public $uncompiled = false;

    /**
     * Source must be recompiled on every occasion
     *
     * @var boolean
     */
    public $recompiled = false;

    /**
     * Flag if resource does implement populateCompiledFilepath() method
     *
     * @var bool
     */
    public $hasCompiledHandler = false;

    /**
     * Load Resource Handler
     *
     * @param \Smarty $smarty smarty object
     * @param string $type   name of the resource
     *
     * @return BasePlugin Resource Handler
     *@throws \Smarty\Exception
     */
    public static function load(Smarty $smarty, $type)
    {
        // try smarty's cache
        if (isset($smarty->_resource_handlers[ $type ])) {
            return $smarty->_resource_handlers[ $type ];
        }
        // try registered resource
        if (isset($smarty->registered_resources[ $type ])) {
            return $smarty->_resource_handlers[ $type ] = $smarty->registered_resources[ $type ];
        }
        // try sysplugins dir
        if (isset(self::$sysplugins[ $type ])) {
            $_resource_class = self::$sysplugins[ $type ];
            return $smarty->_resource_handlers[ $type ] = new $_resource_class();
        }
        // try plugins dir
        $_resource_class = 'Smarty_Resource_' . \smarty_ucfirst_ascii($type);
        if (class_exists($_resource_class, false)) {
            return $smarty->_resource_handlers[ $type ] = new $_resource_class();
        } else {
            $smarty->registerResource(
                $type,
                array(
                    "smarty_resource_{$type}_source", "smarty_resource_{$type}_timestamp",
                    "smarty_resource_{$type}_secure", "smarty_resource_{$type}_trusted"
                )
            );
            // give it another try, now that the resource is registered properly
            return self::load($smarty, $type);
        }
        // try streams
        $_known_stream = stream_get_wrappers();
        if (in_array($type, $_known_stream)) {
            // is known stream
            if (is_object($smarty->security_policy)) {
                $smarty->security_policy->isTrustedStream($type);
            }
            return $smarty->_resource_handlers[ $type ] = new Stream();
        }
        // TODO: try default_(template|config)_handler
        // give up
        throw new \Smarty\Exception("Unknown resource type '{$type}'");
    }

    /**
     * extract resource_type and resource_name from template_resource and config_resource
     *
     * @note "C:/foo.tpl" was forced to file resource up till Smarty 3.1.3 (including).
     *
     * @param string $resource_name    template_resource or config_resource to parse
     * @param string $default_resource the default resource_type defined in $smarty
     *
     * @return array with parsed resource name and type
     */
    public static function parseResourceName($resource_name, $default_resource)
    {
        if (preg_match('/^([A-Za-z0-9_\-]{2,})[:]/', $resource_name, $match)) {
            $type = $match[ 1 ];
            $name = substr($resource_name, strlen($match[ 0 ]));
        } else {
            // no resource given, use default
            // or single character before the colon is not a resource type, but part of the filepath
            $type = $default_resource;
            $name = $resource_name;
        }
        return array($name, $type);
    }

    /**
     * modify template_resource according to resource handlers specifications
     *
     * @param \Smarty\Template|\Smarty $obj               Smarty instance
     * @param string                            $template_resource template_resource to extract resource handler and
     *                                                             name of
     *
     * @return string unique resource name
     * @throws \Smarty\Exception
     */
    public static function getUniqueTemplateName($obj, $template_resource)
    {
        $smarty = $obj->_getSmartyObj();
        [$name, $type] = self::parseResourceName($template_resource, $smarty->default_resource_type);
        // TODO: optimize for Smarty's internal resource types
        $resource = BasePlugin::load($smarty, $type);
        // go relative to a given template?
        $_file_is_dotted = $name[ 0 ] === '.' && ($name[ 1 ] === '.' || $name[ 1 ] === '/');
        if ($obj->_isTplObj() && $_file_is_dotted
            && ($obj->source->type === 'file' || $obj->parent->source->type === 'extends')
        ) {
            $name = $smarty->_realpath(dirname($obj->parent->source->filepath) . DIRECTORY_SEPARATOR . $name);
        }
        return $resource->buildUniqueResourceName($smarty, $name);
    }

    /**
     * initialize Source Object for given resource
     * wrapper for backward compatibility to versions < 3.1.22
     * Either [$_template] or [$smarty, $template_resource] must be specified
     *
     * @param \Smarty\Template $_template         template object
     * @param \Smarty                   $smarty            smarty object
     * @param string                   $template_resource resource identifier
     *
     * @return \Smarty\Template\Source Source Object
     * @throws \Smarty\Exception
     */
    public static function source(
	    \Smarty\Template $_template = null,
	    \Smarty          $smarty = null,
	                     $template_resource = null
    ) {
        return \Smarty\Template\Source::load($_template, $smarty, $template_resource);
    }

    /**
     * Load template's source into current template object
     *
     * @param \Smarty\Template\Source $source source object
     *
     * @return string                 template source
     * @throws \Smarty\Exception        if source cannot be loaded
     */
    abstract public function getContent(\Smarty\Template\Source $source);

    /**
     * populate Source Object with meta data from Resource
     *
     * @param \Smarty\Template\Source   $source    source object
     * @param \Smarty\Template $_template template object
     */
    abstract public function populate(\Smarty\Template\Source $source, \Smarty\Template $_template = null);

    /**
     * populate Source Object with timestamp and exists from Resource
     *
     * @param \Smarty\Template\Source $source source object
     */
    public function populateTimestamp(\Smarty\Template\Source $source)
    {
        // intentionally left blank
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
        if ($isConfig) {
            if (!isset($smarty->_joined_config_dir)) {
                $smarty->getTemplateDir(null, true);
            }
            return get_class($this) . '#' . $smarty->_joined_config_dir . '#' . $resource_name;
        } else {
            if (!isset($smarty->_joined_template_dir)) {
                $smarty->getTemplateDir();
            }
            return get_class($this) . '#' . $smarty->_joined_template_dir . '#' . $resource_name;
        }
    }

    /*
     * Check if resource must check time stamps when when loading complied or cached templates.
     * Resources like 'extends' which use source components my disable timestamp checks on own resource.
     *
     * @return bool
     */
    /**
     * Determine basename for compiled filename
     *
     * @param \Smarty\Template\Source $source source object
     *
     * @return string                 resource's basename
     */
    public function getBasename(\Smarty\Template\Source $source)
    {
        return basename(preg_replace('![^\w]+!', '_', $source->name));
    }

    /**
     * @return bool
     */
    public function checkTimestamps()
    {
        return true;
    }
}
