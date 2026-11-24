<?php
/**
 * Smarty PHPunit tests for KeyValueStore cache invalidation
 */

/**
 * class for KeyValueStore invalidation tests
 *
 * @preserveGlobalState    disabled
 */
class KeyValueStoreInvalidationTest extends PHPUnit_Smarty
{
    /**
     * @dataProvider dataCacheIds
     */
    public function testInvalidationWithCompileIdIsRecognized($cacheId)
    {
        $store = new KeyValueStoreInvalidationTestStore();

        $this->assertTrue($store->isInvalidated('uid', 'normal.tpl', $cacheId, 'compile'));
    }

    public function dataCacheIds()
    {
        return array(
            'empty cache_id' => array(''),
            'cache_id' => array('one'),
            'cache group' => array('one|two'),
        );
    }
}

class KeyValueStoreInvalidationTestStore extends \Smarty\Cacheresource\KeyValueStore
{
    private $entries = array();

    protected function read(array $keys)
    {
        return array_intersect_key($this->entries, array_flip($keys));
    }

    protected function write(array $keys, $expire = null)
    {
        $this->entries = array_replace($this->entries, $keys);
        return true;
    }

    protected function delete(array $keys)
    {
        foreach ($keys as $key) {
            unset($this->entries[$key]);
        }
        return true;
    }

    public function isInvalidated($uid, $name, $cacheId, $compileId)
    {
        $cid = $uid . '#' . $this->sanitize($name) . '#' . $this->sanitize($cacheId) . '#' . $this->sanitize($compileId);
        $this->invalidate($cid, $name, $cacheId, $compileId, $uid);
        return $this->getLatestInvalidationTimestamp($cid, $name, $cacheId, $compileId, $uid) > 0;
    }
}
