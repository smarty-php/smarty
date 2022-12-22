<?php


class Smarty_CacheResource_Filetest extends \Smarty\Cacheresource\File
{
    public $lockTime = 0;

    public function hasLock(\Smarty\Smarty $smarty, \Smarty\Template\Cached $cached)
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
