<?php
/**
 * Smarty PHPunit tests for Extendsresource
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for extends resource tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class ExtendsResourceTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }

    public function compiledPrefilter($text, Smarty_Internal_Template $tpl)
    {
        return str_replace('#', $tpl->getTemplateVars('test'), $text);
    }

    /**
     * test  child/parent template chain with prepend
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildPrepend_003($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->inheritance_merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('extends:003_parent.tpl|003_child_prepend.tpl');
        $this->assertContains("prepend - Default Title", $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }
    /**
     * test  child/parent template chain with apppend
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildAppend_004($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->inheritance_merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('extends:004_parent.tpl|004_child_append.tpl');
        $this->assertContains("Default Title - append", $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with apppend
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockAssignInChild_040($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->inheritance_merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('extends:040_parent.tpl|040_child.tpl');
        $this->assertContains("var-bar-var", $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent dependency test1
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testCompileBlockGrandChildMustCompile_021_1()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test1
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testCompileBlockGrandChildMustCompile_021_12()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
         $tpl2 = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test2
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_2()
    {
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_grandchild.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
    }
    /**
     * test  grandchild/child/parent dependency test2
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_22()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl2 = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test3
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_3()
    {
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_child.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
    }
    /**
     * test  grandchild/child/parent dependency test3
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_32()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl2 = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test4
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_4()
    {
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_parent.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
    }
    /**
     * test  grandchild/child/parent dependency test4
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_42()
    {
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('extends:021_parent.tpl|021_child.tpl|021_grandchild.tpl');
        $this->assertTrue($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('Grandchild Page Title', $result);
    }

    public function data(){
        return array(
            /*
             * caching
             * merging
             * test nr
             * result compile nr
             * result render nr
             * text
             */
            array(false, false, 1, 1, 1, 'no caching, no merge - new'),
            array(false, false, 2, 1, 2, 'no caching, no merge - exits'),
            array(true, false, 3, 3, 3, 'caching, no merge - new'),
            array(true, false, 4, 3, 3, 'caching, no merge - exits'),
            array(false, true, 5, 5, 5, 'no caching, merge - new'),
            array(false, true, 6, 5, 6, 'no caching, merge - exits'),
            array(true, true, 7, 7, 7, 'caching, merge - new'),
            array(true, true, 8, 7, 7, 'caching, merge - exits'),
        );
    }

}

