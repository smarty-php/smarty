<?php
/**
 * Smarty PHPunit tests compilation of {setfilter} tag
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {setfilter} tag tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileSetfilterTest extends PHPUnit_Smarty
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
     * test nested setfilter
     */
    public function testNestedSetfilter()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo}{setfilter htmlspecialchars} {$foo}{setfilter escape:"mail"} {$foo}{/setfilter} {$foo}{/setfilter} {$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("<a@b.c> &lt;a@b.c&gt; <a [AT] b [DOT] c> &lt;a@b.c&gt; <a@b.c>", $this->smarty->fetch($tpl));
    }
}
