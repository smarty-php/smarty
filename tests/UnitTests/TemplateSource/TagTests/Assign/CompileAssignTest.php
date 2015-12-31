<?php
/**
 * Smarty PHPunit tests compilation of assign tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for assign tags tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CompileAssignTest extends PHPUnit_Smarty
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
     * test old style of assign tag
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignOld_001()
    {
        $this->assertEquals("1", $this->smarty->fetch('001_oldFormat_1.tpl'));
        $this->assertEquals("1", $this->smarty->fetch('001_oldFormat_2.tpl'));
        $this->assertEquals("1", $this->smarty->fetch('002_oldFormat_1.tpl'));
        $this->assertEquals("1", $this->smarty->fetch('003_oldFormat_1.tpl'));
        $this->assertEquals("bar", $this->smarty->fetch('004_oldFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('005_oldFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('006_oldFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('007_oldFormat_1.tpl'));
        $this->assertEquals("9876", $this->smarty->fetch('008_oldFormat_1.tpl'));
        $this->assertEquals("a9b8c7d6", $this->smarty->fetch('009_oldFormat_1.tpl'));
        $this->assertEquals("1", $this->smarty->fetch('010_oldFormat_1.tpl'));
        $this->assertEquals("1", $this->smarty->fetch('011_oldFormat_1.tpl'));
    }

    /**
     * test new style of assign tag
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignNew_001()
    {
        $this->assertEquals("1", $this->smarty->fetch('001_newFormat_1.tpl'));
        $this->assertEquals("2", $this->smarty->fetch('001_newFormat_2.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('001_newFormat_3.tpl'));
        $this->assertEquals("bar", $this->smarty->fetch('002_newFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('003_newFormat_1.tpl'));
        $this->assertEquals("4", $this->smarty->fetch('003_newFormat_2.tpl'));
        $this->assertEquals("5", $this->smarty->fetch('003_newFormat_3.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('004_newFormat_1.tpl'));
        $this->assertEquals("3", $this->smarty->fetch('005_newFormat_1.tpl'));
        $this->assertEquals("9876", $this->smarty->fetch('006_newFormat_1.tpl'));
        $this->assertEquals("a9b8c7d6", $this->smarty->fetch('007_newFormat_1.tpl'));
    }

    /**
     * test array append
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignArrayAppend_001()
    {
        $this->assertEquals("0112", $this->smarty->fetch('001_newAppend_1.tpl'));
    }

    /**
     * test array append 2
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignArrayAppend_002()
    {
        $this->smarty->assign('foo', 1);
        $tpl = $this->smarty->createTemplate('002_newAppend_1.tpl', null, null, $this->smarty);
        $this->assertEquals("0112", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate('002_newAppend_2.tpl', null, null, $this->smarty);
        $this->assertEquals("1", $this->smarty->fetch($tpl2));
    }

    /**
     * test array append 3
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignArrayAppend_003()
    {
        $this->smarty->assign('foo', 1);
        $tpl = $this->smarty->createTemplate('003_newAppend_1.tpl', null, null, $this->smarty);
        $this->assertEquals("0112", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate('003_newAppend_2.tpl', null, null, $this->smarty);
        $this->assertEquals("0112", $this->smarty->fetch($tpl2));
    }

    /**
     * test nested array
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testAssignNestedArray_004()
    {
        $this->assertEquals("1", $this->smarty->fetch('004_newNested_1.tpl'));
    }
}
