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
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * @run
     * InSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testNestedSetfilter()
    {
        $this->smarty->setCaching(1);
        $tpl = $this->smarty->createTemplate('string:{$foo}{setfilter htmlspecialchars} {$foo}{$foo nocache}{setfilter escape:"mail"} {$foo}{$foo nocache}{/setfilter} {$foo}{/setfilter} {$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("<a@b.c> &lt;a@b.c&gt;&lt;a@b.c&gt; <a [AT] b [DOT] c><a [AT] b [DOT] c> &lt;a@b.c&gt; <a@b.c>", $this->smarty->fetch($tpl));
    }
    public function testNestedSetfilter1()
    {
        $this->smarty->setCaching(1);
        $tpl = $this->smarty->createTemplate('string:{$foo}{setfilter htmlspecialchars} {$foo}{$foo nocache}{setfilter escape:"mail"} {$foo}{$foo nocache}{/setfilter} {$foo}{/setfilter} {$foo}');
        $tpl->assign('foo', '<e@f.d>');
        $this->assertEquals("<a@b.c> &lt;a@b.c&gt;&lt;e@f.d&gt; <a [AT] b [DOT] c><e [AT] f [DOT] d> &lt;a@b.c&gt; <a@b.c>", $this->smarty->fetch($tpl));
    }
    /**
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testNestedSetfilter2()
    {
        $tpl = $this->smarty->createTemplate('string:{$foo}{setfilter htmlspecialchars} {$foo}{setfilter escape:"mail"} {$foo}{/setfilter} {$foo}{/setfilter} {$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("<a@b.c> &lt;a@b.c&gt; <a [AT] b [DOT] c> &lt;a@b.c&gt; <a@b.c>", $this->smarty->fetch($tpl));
    }
}
