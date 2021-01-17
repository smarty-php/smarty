<?php
/**
 * Smarty PHPunit tests append method
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for append tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class AppendTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test append
     */
    public function testAppend()
    {
        $this->smarty->assign('foo', 'bar');
        $this->smarty->append('foo', 'bar2');
        $this->assertEquals('bar bar2', $this->smarty->fetch('eval:{$foo[0]} {$foo[1]}'));
    }

    /**
     * test append to unassigned variable
     */
    public function testAppendUnassigned()
    {
        $this->smarty->append('foo', 'bar');
        $this->assertEquals('bar', $this->smarty->fetch('eval:{$foo[0]}'));
    }

    /**
     * test append merge
     */
    public function testAppendMerge()
    {
        $this->smarty->assign('foo', array('a' => 'a', 'b' => 'b', 'c' => 'c'));
        $this->smarty->append('foo', array('b' => 'd'), true);
        $this->assertEquals('a d c', $this->smarty->fetch('eval:{$foo["a"]} {$foo["b"]} {$foo["c"]}'));
    }

    /**
     * test append array merge
     */
    public function testAppendArrayMerge()
    {
        $this->smarty->assign('foo', array('b' => 'd'));
        $this->smarty->append('foo', array('a' => 'a', 'b' => 'b', 'c' => 'c'), true);
        $this->assertEquals('a b c', $this->smarty->fetch('eval:{$foo["a"]} {$foo["b"]} {$foo["c"]}'));
    }

    /**
     * test array append
     */
    public function testArrayAppend()
    {
        $this->smarty->assign('foo', 'foo');
        $this->smarty->append(array('bar' => 'bar2', 'foo' => 'foo2'));
        $this->assertEquals('foo foo2 bar2', $this->smarty->fetch('eval:{$foo[0]} {$foo[1]} {$bar[0]}'));
    }

    /**
     * test array append array merge
     */
    public function testArrayAppendArrayMerge()
    {
        $this->smarty->assign('foo', array('b' => 'd'));
        $this->smarty->append(array('bar' => 'bar', 'foo' => array('a' => 'a', 'b' => 'b', 'c' => 'c')), null, true);
        $this->assertEquals('a b c bar', $this->smarty->fetch('eval:{$foo["a"]} {$foo["b"]} {$foo["c"]} {$bar[0]}'));
    }
}
