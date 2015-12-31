<?php
/**
 * Smarty PHPunit tests assignByRef method
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for assignByRef tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class AssignByRefBCTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public $loadSmarty = false;
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test Smarty2 assign_By_Ref
     */
    public function testSmarty2AssignByRef()
    {
        $bar = 'bar';
        $this->smartyBC->assign_by_ref('foo', $bar);
        $bar = 'newbar';
        $this->assertEquals('newbar', $this->smartyBC->fetch('eval:{$foo}'));
    }

    /**
     * test Smarty2's behaviour of assign_By_Ref (Issue 88)
     */
    public function testSmarty2AssignByRef2()
    {
        $bar = 'bar';
        $this->smartyBC->assign_by_ref('foo', $bar);
        $this->smartyBC->fetch('eval:{$foo = "newbar"}');
        $this->assertEquals('newbar', $bar);
    }
}
