<?php
/**
 * Smarty PHPunit tests compilation of delimiter tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for delimiter tags tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileDelimiterTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test delimiter tag test
     */
    public function testLeftDelimiter()
    {
        $tpl = $this->smarty->createTemplate('eval:x{ldelim}x');
        $this->assertEquals('x{x', $this->smarty->fetch($tpl));
    }

    public function testRightDelimiter()
    {
        $tpl = $this->smarty->createTemplate('eval:x{rdelim}x');
        $this->assertEquals('x}x', $this->smarty->fetch($tpl));
    }
}
