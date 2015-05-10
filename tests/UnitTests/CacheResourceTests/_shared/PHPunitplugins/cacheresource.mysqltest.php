<?php

require_once SMARTY_DIR . '../demo/plugins/cacheresource.mysql.php';

class Smarty_CacheResource_Mysqltest extends Smarty_CacheResource_Mysql
{
    public $lockTime = 0;

    public function __construct() {
        try {
            $this->db = PHPUnit_Smarty::$pdo;
        } catch (PDOException $e) {
            throw new SmartyException('Mysql Resource failed: ' . $e->getMessage());
        }
        $this->fetch = $this->db->prepare('SELECT modified, content FROM output_cache WHERE id = :id');
        $this->fetchTimestamp = $this->db->prepare('SELECT modified FROM output_cache WHERE id = :id');
        $this->save = $this->db->prepare('REPLACE INTO output_cache (id, name, cache_id, compile_id, content)
            VALUES  (:id, :name, :cache_id, :compile_id, :content)');
    }

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
