<?php
/**
 * Smarty PHPunit tests appendByRef method
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for appendByRef tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class AppendByRefTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    /**
     * test appendByRef
     */
    public function testAppendByRef()
    {
        $bar = 'bar';
        $bar2 = 'bar2';
        $this->smarty->appendByRef('foo', $bar);
        $this->smarty->appendByRef('foo', $bar2);
        $bar = 'newbar';
        $bar2 = 'newbar2';
        $this->assertEquals('newbar newbar2', $this->smarty->fetch('eval:{$foo[0]} {$foo[1]}'));
    }

    /**
     * test appendByRef to unassigned variable
     */
    public function testAppendByRefUnassigned()
    {
        $bar2 = 'bar2';
        $this->smarty->appendByRef('foo', $bar2);
        $bar2 = 'newbar2';
        $this->assertEquals('newbar2', $this->smarty->fetch('eval:{$foo[0]}'));
    }

    /**
     * test appendByRef merge
     *
     * @todo fix testAppendByRefMerge
     */
    public function testAppendByRefMerge()
    {
        $foo = array('a' => 'a', 'b' => 'b', 'c' => 'c');
        $bar = array('b' => 'd');
        $this->smarty->assignByRef('foo', $foo);
        $this->smarty->appendByRef('foo', $bar, true);
        $this->assertEquals('a d c', $this->smarty->fetch('eval:{$foo["a"]} {$foo["b"]} {$foo["c"]}'));
        $bar = array('b' => 'newd');
        $this->smarty->appendByRef('foo', $bar, true);
        $this->assertEquals('a newd c', $this->smarty->fetch('eval:{$foo["a"]} {$foo["b"]} {$foo["c"]}'));
    }
}
