<?php

namespace Smarty\Template;

use Smarty\Smarty;
use Smarty\Template;
use Smarty\Exception;

/**
 * Meta-data Container for Template source files
 * @author     Rodney Rehm
 */
class Source {

	/**
	 * Unique Template ID
	 *
	 * @var string
	 */
	public $uid = null;

	/**
	 * Template Resource (\Smarty\Template::$template_resource)
	 *
	 * @var string
	 */
	public $resource = null;

	/**
	 * Resource Type
	 *
	 * @var string
	 */
	public $type = null;

	/**
	 * Resource Name
	 *
	 * @var string
	 */
	public $name = null;

	/**
	 * Source Filepath
	 *
	 * @var string
	 */
	public $filepath = null;

	/**
	 * Source Timestamp
	 *
	 * @var int
	 */
	public $timestamp = null;

	/**
	 * Source Existence
	 *
	 * @var boolean
	 */
	public $exists = false;

	/**
	 * Source File Base name
	 *
	 * @var string
	 */
	public $basename = null;

	/**
	 * The Components an extended template is made of
	 *
	 * @var \Smarty\Template\Source[]
	 */
	public $components = null;

	/**
	 * Resource Handler
	 *
	 * @var \Smarty\Resource\BasePlugin
	 */
	public $handler = null;

	/**
	 * Smarty instance
	 *
	 * @var Smarty
	 */
	protected $smarty = null;

	/**
	 * Resource is source
	 *
	 * @var bool
	 */
	public $isConfig = false;

	/**
	 * Template source content eventually set by default handler
	 *
	 * @var string
	 */
	public $content = null;

	/**
	 * create Source Object container
	 *
	 * @param Smarty $smarty Smarty instance this source object belongs to
	 * @param string $resource full template_resource
	 * @param string $type type of resource
	 * @param string $name resource name
	 *
	 * @throws   \Smarty\Exception
	 * @internal param \Smarty\Resource\Base $handler Resource Handler this source object communicates with
	 */
	public function __construct(Smarty $smarty, $resource, $type, $name) {
		$this->handler = \Smarty\Resource\BasePlugin::load($smarty, $type);

		$this->smarty = $smarty;
		$this->resource = $resource;
		$this->type = $type;
		$this->name = $name;
	}

	/**
	 * initialize Source Object for given resource
	 * Either [$_template] or [$smarty, $template_resource] must be specified
	 *
	 * @param Template|null $_template template object
	 * @param Smarty|null $smarty smarty object
	 * @param null $template_resource resource identifier
	 *
	 * @return Source Source Object
	 * @throws Exception
	 */
	public static function load(
		Template $_template = null,
		Smarty   $smarty = null,
		         $template_resource = null
	) {
		if ($_template) {
			$smarty = $_template->getSmarty();
			$template_resource = $_template->template_resource;
		}
		if (empty($template_resource)) {
			throw new Exception('Source: Missing  name');
		}
		// parse resource_name, load resource handler, identify unique resource name
		if (preg_match('/^([A-Za-z0-9_\-]{2,})[:]([\s\S]*)$/', $template_resource, $match)) {
			$type = $match[1];
			$name = $match[2];
		} else {
			// no resource given, use default
			// or single character before the colon is not a resource type, but part of the filepath
			$type = $smarty->default_resource_type;
			$name = $template_resource;
		}
		// create new source  object
		$source = new Source($smarty, $template_resource, $type, $name);
		$source->handler->populate($source, $_template);
		if (!$source->exists && $_template && isset($_template->getSmarty()->default_template_handler_func)) {
			$source->_getDefaultTemplate($_template->getSmarty()->default_template_handler_func);
			$source->handler->populate($source, $_template);
		}
		return $source;
	}

	/**
	 * Get source time stamp
	 *
	 * @return int
	 */
	public function getTimeStamp() {
		if (!isset($this->timestamp)) {
			$this->handler->populateTimestamp($this);
		}
		return $this->timestamp;
	}

	/**
	 * Get source content
	 *
	 * @return string
	 * @throws \Smarty\Exception
	 */
	public function getContent() {
		return $this->content ?? $this->handler->getContent($this);
	}

	/**
	 * get default content from template or config resource handler
	 *
	 * @throws \Smarty\Exception
	 */
	public function _getDefaultTemplate($default_handler) {
		$_content = $_timestamp = null;
		$_return = call_user_func_array(
			$default_handler,
			[$this->type, $this->name, &$_content, &$_timestamp, $this->smarty]
		);
		if (is_string($_return)) {
			$this->exists = is_file($_return);
			if ($this->exists) {
				$this->timestamp = filemtime($_return);
			} else {
				throw new Exception(
					'Default handler: Unable to load ' .
					($this->isConfig ? 'config' : 'template') .
					" default file '{$_return}' for '{$this->type}:{$this->name}'"
				);
			}
			$this->name = $this->filepath = $_return;
			$this->uid = sha1($this->filepath);
		} elseif ($_return === true) {
			$this->content = $_content;
			$this->exists = true;
			$this->uid = $this->name = sha1($_content);
			$this->handler = \Smarty\Resource\BasePlugin::load($this->smarty, 'eval');
		} else {
			$this->exists = false;
			throw new Exception(
				'Default handler: No ' . ($this->isConfig ? 'config' : 'template') .
				" default content for '{$this->type}:{$this->name}'"
			);
		}
	}

	public function createCompiler(): \Smarty\Compiler\BaseCompiler {
		return new \Smarty\Compiler\Template($this->smarty);
	}

	public function getSmarty() {
		return $this->smarty;
	}

}
