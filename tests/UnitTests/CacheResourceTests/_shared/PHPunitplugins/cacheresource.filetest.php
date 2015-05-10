<?php


class Smarty_CacheResource_Filetest extends Smarty_Internal_CacheResource_File
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
