<?php
/**
 * Smarty PHPunit tests assign method
 *

 * @author  Uwe Tews
 */

/**
 * class for assign tests
 *
 *
 *
 *
 */
class AssignTest extends PHPUnit_Smarty
{
     public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test simple assign
     */
    public function testSimpleAssign()
    {
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals('bar', $this->smarty->fetch('eval:{$foo}'));
    }

    /**
     * test assign array of variables
     */
    public function testArrayAssign()
    {
        $this->smarty->assign(array('foo' => 'bar', 'foo2' => 'bar2'));
        $this->assertEquals('bar bar2', $this->smarty->fetch('eval:{$foo} {$foo2}'));
    }

	/**
	 * Test that assign returns this.
	 */
	public function testAssignReturnsThis()
	{
		$this->assertEquals(
			'data',
			$this->smarty->assign(['dummy' => 'data'])->fetch('eval:{$dummy}')
		);
	}
}
