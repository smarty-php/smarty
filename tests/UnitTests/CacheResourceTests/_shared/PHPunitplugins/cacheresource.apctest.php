<?php

use Smarty\Smarty;
use Smarty\Template;
use Smarty\Template\Cached;

require_once SMARTY_DIR . '../demo/plugins/cacheresource.apc.php';

class Smarty_CacheResource_Apctest extends My_CacheResource_Apc
{
    public $lockTime = 0;

    public function hasLock(Smarty $smarty, Cached $cached)
    {
        if ($this->lockTime) {
            $this->lockTime--;
            if (!$this->lockTime) {
                $this->releaseLock($smarty, $cached);
            }
        }
        return parent::hasLock($smarty, $cached);
    }

    public function get(Template $_template)
    {
        $this->contents = array();
        $this->timestamps = array();
        $t = $this->getContent($_template);

        return $t ? $t : null;
    }

    public function __sleep()
    {
        return array();
    }

    public function __wakeup()
    {
        $this->__construct();
    }
}
