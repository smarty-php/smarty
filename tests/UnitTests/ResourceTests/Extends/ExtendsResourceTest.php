<?php
/**
 * Smarty PHPunit tests for Extendsresource
 *

 * @author  Uwe Tews
 */

use Smarty\Template;

/**
 * class for extends resource tests
 */
class ExtendsResourceTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->enableSecurity();
    }


    public function testInit()
    {
        $this->cleanDirs();
    }

    public function compiledPrefilter($text, Template $tpl)
    {
        return str_replace('#', $tpl->getTemplateVars('test'), $text);
    }

    /**
     * test  child/parent template chain with prepend
     * @dataProvider data
     */
    public function testCompileBlockChildPrepend_003($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('extends:003_parent.tpl|003_child_prepend.tpl');
        $this->assertStringContainsString(
			"prepend - Default Title", $result, $testName . ' - content');
        $this->assertStringContainsString(
			"test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}",
			$result,
			$testName . ' - fetch() failure'
        );
    }

    /**
     * test  child/parent template chain with apppend
     * @dataProvider data
     */
    public function testCompileBlockChildAppend_004($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('extends:004_parent.tpl|004_child_append.tpl');
        $this->assertStringContainsString("Default Title - append", $result, $testName . ' - content');
        $this->assertStringContainsString("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with apppend
     * @dataProvider data
     */
    public function testCompileBlockAssignInChild_040($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->caching = $caching;
        $this->smarty->merge_compiled_includes = $merge;
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('extends:040_parent.tpl|040_child.tpl');
        $this->assertStringContainsString("var-bar-var", $result, $testName . ' - content');
        $this->assertStringContainsString("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
    }

	/**
	 * @dataProvider data
	 */
	public function testCompileBlockIncreaseInChild_050($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber, $testName)
	{
		$this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
		$this->smarty->assign('test', $testNumber);
		$this->smarty->caching = $caching;
		$this->smarty->merge_compiled_includes = $merge;
		if ($merge) {
			$this->smarty->setCompileId(1);
		}
		$result = $this->smarty->fetch('extends:050_parent.tpl|050_child.tpl|050_grandchild.tpl');
		$this->assertStringContainsString("var-bar-var", $result, $testName . ' - content');
		$this->assertStringContainsString("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result, $testName . ' - fetch() failure');
	}

    /**
     * test  grandchild/child/parent dependency test1
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
     * @group slow
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

}

