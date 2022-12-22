<?php

use Smarty\Exception;

require_once SMARTY_DIR . '../demo/plugins/resource.mysqls.php';

class Smarty_Resource_Mysqlstest extends _MysqlsPlugin
{
    public function __construct()
    {
        try {
            $this->db = PHPUnit_Smarty::$pdo;
        }
        catch
            (PDOException $e) {
                throw new Exception('Mysql Resource failed: ' . $e->getMessage());
            }
        $this->fetch = $this->db->prepare('SELECT modified, source FROM templates WHERE name = :name');
    }
}

