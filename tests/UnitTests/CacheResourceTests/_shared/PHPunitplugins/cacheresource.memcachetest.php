<?php

use Smarty\Template;
use Smarty\Template\Cached;

require_once __DIR__ . '/../../../__shared/cacheresources/cacheresource.memcache.php';

class Smarty_CacheResource_Memcachetest extends Smarty_CacheResource_Memcache
{
    public $lockTime = 0;

    public function hasLock(\Smarty\Smarty $smarty, Cached $cached)
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

}
