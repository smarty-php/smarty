<?php

use Smarty\Exception;
use Smarty\Template\Cached;

require_once __DIR__ . '/../../../__shared/cacheresources/cacheresource.mysql.php';

class Smarty_CacheResource_Mysqltest extends Smarty_CacheResource_Mysql
{
    public $lockTime = 0;

	protected function db(): PDO {
		return PHPUnit_Smarty::$pdo;
	}

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
