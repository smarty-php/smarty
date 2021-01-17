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
class AssignByRefTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    /**
     * test simple assignByRef
     */
    public function testSimpleAssignByRef()
    {
        $bar = 'bar';
        $this->smarty->assignByRef('foo', $bar);
        $bar = 'newbar';
        $this->assertEquals('newbar', $this->smarty->fetch('eval:{$foo}'));
    }
}
