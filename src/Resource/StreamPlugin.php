<?php
/**
 * Smarty Internal Plugin Resource Stream
 * Implements the streams as resource for Smarty template
 *


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
	public function populate(Source $source, ?Template $_template = null) {
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

		if (strpos($source->getResourceName(), '://') !== false) {
			$filepath = $source->getResourceName();
		} else {
			$filepath = str_replace(':', '://', $source->getFullResourceName());
		}

		// Validate the underlying stream wrapper against the security policy.
		// When the built-in "stream" resource type is used (e.g.
		// stream:php://filter/...), BasePlugin::load() matches the "stream"
		// sysplugin before the stream_get_wrappers()/isTrustedStream() check,
		// so the nested wrapper ("php" here) is never validated. Parse the
		// wrapper scheme from the resolved path and check it explicitly so that
		// e.g. Security::$streams = null blocks it before fopen() (CWE-22/-441).
		$smarty = $source->getSmarty();
		if (is_object($smarty->security_policy) && ($_pos = strpos($filepath, '://')) !== false) {
			$smarty->security_policy->isTrustedStream(strtolower(substr($filepath, 0, $_pos)));
		}

		$t = '';
		$fp = fopen($filepath, 'r+');
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

}
