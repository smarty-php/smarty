<?php
/**
 * Smarty PHPunit tests assignGlobal method  and {assignGlobal} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for assignGlobal method  and {assignGlobal} tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class AssignGlobalTest extends PHPUnit_Smarty
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
     * test  assignGlobal and getGlobal
     */
    public function testAssignGlobalGetGlobal()
    {
        $this->smarty->assignGlobal('foo', 'bar');
        $this->assertEquals('bar', $this->smarty->getGlobal('foo'));
    }

    /**
     * test  assignGlobal and getGlobal on arrays
     */
    public function testAssignGlobalGetGlobalArray()
    {
        $this->smarty->assignGlobal('foo', array('foo' => 'bar', 'foo2' => 'bar2'));
        $a1 = array('foo' => array('foo' => 'bar', 'foo2' => 'bar2'));
        $a2 = $this->smarty->getGlobal();
        unset($a2['SCRIPT_NAME']);
        $this->assertTrue($a1 === $a2);
    }

    /**
     * test assignGlobal tag
     */
    public function testAssignGlobalTag()
    {
        $this->smarty->assignGlobal('foo', 'bar');
        $this->assertEquals('bar', $this->smarty->fetch('eval:{$foo}'));
        $this->assertEquals('buh', $this->smarty->fetch('eval:{assign var=foo value=buh scope=global}{$foo}'));
        $this->assertEquals('buh', $this->smarty->fetch('eval:{$foo}'));
        $this->assertEquals('buh', $this->smarty->getGlobal('foo'));
    }

    /**
     * test global var array element tag
     */
    public function testGlobalVarArrayTag()
    {
        $this->smarty->assignGlobal('foo', array('foo' => 'bar', 'foo2' => 'bar2'));
        $this->assertEquals('bar2', $this->smarty->fetch('eval:{$foo.foo2}'));
        $this->assertEquals('bar', $this->smarty->fetch('eval:{$foo.foo}'));
    }
}
