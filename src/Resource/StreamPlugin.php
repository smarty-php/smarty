<?php
/**
 * Smarty Internal Plugin Resource Stream
 * Implements the streams as resource for Smarty template
 *
 * @package    Smarty
 * @subpackage TemplateResources
 * @author     Uwe Tews
 * @author     Rodney Rehm
 */

namespace Smarty\Resource;

use Smarty\Smarty;
use Smarty\Template;
use Smarty\Template\Source;

/**
 * Smarty Internal Plugin Resource Stream
 * Implements the streams as resource for Smarty template
 *
 * @link       https://php.net/streams
 * @package    Smarty
 * @subpackage TemplateResources
 */
class StreamPlugin extends RecompiledPlugin {

	/**
	 * populate Source Object with meta data from Resource
	 *
	 * @param Source $source source object
	 * @param Template $_template template object
	 *
	 * @return void
	 */
	public function populate(Source $source, Template $_template = null) {
		if (strpos($source->resource, '://') !== false) {
			$source->filepath = $source->resource;
		} else {
			$source->filepath = str_replace(':', '://', $source->resource);
		}
		$source->uid = false;
		$source->content = $this->getContent($source);
		$source->timestamp = $source->exists = !!$source->content;
	}

	/**
	 * Load template's source from stream into current template object
	 *
	 * @param Source $source source object
	 *
	 * @return string template source
	 */
	public function getContent(Source $source) {
		$t = '';
		// the availability of the stream has already been checked in Smarty\Resource\Base::fetch()
		$fp = fopen($source->filepath, 'r+');
		if ($fp) {
			while (!feof($fp) && ($current_line = fgets($fp)) !== false) {
				$t .= $current_line;
			}
			fclose($fp);
			return $t;
		} else {
			return false;
		}
	}

	/**
	 * modify resource_name according to resource handlers specifications
	 *
	 * @param Smarty $smarty Smarty instance
	 * @param string $resource_name resource_name to make unique
	 * @param boolean $isConfig flag for config resource
	 *
	 * @return string unique resource name
	 */
	public function buildUniqueResourceName(Smarty $smarty, $resource_name, $isConfig = false) {
		return get_class($this) . '#' . $resource_name;
	}
}
