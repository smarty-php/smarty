<?php

use Smarty\Resource\FilePlugin;
use Smarty\Smarty;
use Smarty\Template;
use Smarty\Template\Source;

/**
 * Ambiguous Filename Custom Resource Example
 *

 * @author  Rodney Rehm
 */
class Smarty_Resource_AmbiguousPlugin extends FilePlugin
{
    protected $directory;
    protected $segment;

    public function __construct($directory)
    {
        $this->directory = rtrim($directory ?? '', "/\\") . DIRECTORY_SEPARATOR;
        //        parent::__construct();
    }

    public function setSegment($segment)
    {
        $this->segment = $segment;
    }

    /**
     * populate Source Object with meta data from Resource
     *
     * @param Source   $source    source object
     * @param Template $_template template object
     */
    public function populate(Source $source, Template $_template = null)
    {
        $segment = '';
        if ($this->segment) {
            $segment = rtrim($this->segment, "/\\") . DIRECTORY_SEPARATOR;
        }

        $source->filepath = $this->directory . $segment . $source->name;
        $source->uid = sha1($source->filepath);
        if ($_template->getSmarty()->getCompileCheck() && !isset($source->timestamp)) {
            $source->timestamp = @filemtime($source->filepath);
            $source->exists = !!$source->timestamp;
        }
    }
}
