<?php

require_once SMARTY_DIR . '../demo/plugins/cacheresource.pdo.php';

class Smarty_CacheResource_Pdotest extends Smarty_CacheResource_Pdo
{
    public $lockTime = 0;


    public function hasLock(Smarty $smarty, Smarty_Template_Cached $cached)
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
