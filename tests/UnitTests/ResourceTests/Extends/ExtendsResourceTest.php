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
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->enableSecurity();
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
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('extends:003_parent.tpl|003_child_prepend.tpl');
        $this->assertStringContainsString("prepend - Default Title", $result, $testName . ' - content');
        $this->assertStringContainsString("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }
    /**
     * test  child/parent template chain with apppend
     * @run InSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider data
     */
    public function testCompileBlockChildAppend_004($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('extends:004_parent.tpl|004_child_append.tpl');
        $this->assertStringContainsString("Default Title - append", $result, $testName . ' - content');
        $this->assertStringContainsString("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
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
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->compile_id = 1;
        }
        $result = $this->smarty->fetch('extends:040_parent.tpl|040_child.tpl');
        $this->assertStringContainsString("var-bar-var", $result, $testName . ' - content');
        $this->assertStringContainsString("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
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
        $this->assertStringContainsString('Grandchild Page Title', $result);
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
        $this->assertStringContainsString('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test2
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @group slow
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
        $this->assertStringContainsString('Grandchild Page Title', $result);
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
        $this->assertStringContainsString('Grandchild Page Title', $result);
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
        $this->assertStringContainsString('Grandchild Page Title', $result);
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
        $this->assertStringContainsString('Grandchild Page Title', $result);
    }

    /**
     * test  grandchild/child/parent dependency test4
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @group slow
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
        $this->assertStringContainsString('Grandchild Page Title', $result);
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
        $this->assertStringContainsString('Grandchild Page Title', $result);
    }

    /**
     * test  relative includes in {block}
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockRelativeIncludes_033($caching, $merge, $testNumber, $compileTestNumber,
                                                         $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('extends:./child/parent/033_parent.tpl|./child/033_child.tpl|033_grandchild.tpl');
        $this->assertStringContainsString('include grand:content include grand', $result, $testName . ' - grand');
        $this->assertStringContainsString('include child:content include child', $result, $testName . ' - grand');
        $this->assertStringContainsString('include parent:content include parent', $result, $testName . ' - grand');
        $this->assertStringContainsString("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
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
    /**
     * test  relative includes in {block}
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data2
     */
    public function testCompileBlockExtendsRecursion_034($extends_recursion, $merge, $testNumber, $compileTestNumber,
                                                         $renderTestNumber, $testName)
    {
        if (!property_exists($this->smarty, 'extends_recursion')) {
            $this->markTestSkipped('no extends_recursion');
        } else {
            $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
            $this->smarty->assign('test', $testNumber);
            $this->smarty->setExtendsRecursion($extends_recursion);
            $this->smarty->setMergeCompiledIncludes($merge);
            $cid = 0;
            if ($merge) {
                $cid = 1;
            }
            if ($extends_recursion) {
                $cid += 2;
            }
            $this->smarty->setCompileId($cid);
            $result = $this->smarty->fetch('extends:034_parent.tpl|034_grandchild.tpl');
            $this->assertStringContainsString('grandchild - grandchild', $result, $testName . ' - grand');
            $this->assertStringContainsString('parent - parent', $result, $testName . ' - grand');
            $this->assertStringContainsString($extends_recursion ? 'child - child' : 'child - parent', $result,
                                  $testName . ' - grand');
            $this->assertStringContainsString("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}",
                                  $result, $testName . ' - fetch() failure');
        }
    }
    public function data2(){
        return array(
            /*
             * extends_recursion
             * merging
             * test nr
             * result compile nr
             * result render nr
             * text
             */
            array(false, false, 1, 1, 1, 'no EXTENDS; no merge - new'),
            array(false, false, 2, 1, 2, 'no EXTENDS; no merge - exits'),
            array(true, false, 3, 3, 3, 'EXTENDS; no merge - new'),
            array(true, false, 4, 3, 4, 'EXTENDS; no merge - exits'),
            array(false, true, 5, 5, 5, 'no EXTENDS; merge - new'),
            array(false, true, 6, 5, 6, 'no EXTENDS; merge - exits'),
            array(true, true, 7, 7, 7, 'EXTENDS; merge - new'),
            array(true, true, 8, 7, 8, 'EXTENDS; merge - exits'),
        );
    }

}

