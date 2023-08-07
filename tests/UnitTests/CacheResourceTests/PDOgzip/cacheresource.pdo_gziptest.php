<?php

use Smarty\Template\Cached;

require_once __DIR__ . '/../../__shared/cacheresources/cacheresource.pdo_gzip.php';

class Smarty_CacheResource_Pdo_Gziptest extends Smarty_CacheResource_Pdo_Gzip
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
