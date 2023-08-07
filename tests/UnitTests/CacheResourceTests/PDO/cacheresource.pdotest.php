<?php

use Smarty\Template\Cached;

require_once __DIR__ . '/../../__shared/cacheresources/cacheresource.pdo.php';

class Smarty_CacheResource_Pdotest extends Smarty_CacheResource_Pdo
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
}
