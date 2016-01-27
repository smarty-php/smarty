<?php
/**
 * Smarty PHPunit tests for Block Extends
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for block extends compiler tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CompileBlockExtendsTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        //$this->smarty->setMergeCompiledIncludes(true);
    }

    public function compiledPrefilter($text, Smarty_Internal_Template $tpl)
    {
        return str_replace('#', $tpl->getTemplateVars('test'), $text);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test block default outout
     */
    public function testBlockDefault_000_1()
    {
        $result = $this->smarty->fetch('string:{block name=test}-- block default --{/block}');
        $this->assertEquals('-- block default --', $result);
    }

    public function testBlockDefault_000_2()
    {
        $this->smarty->assign('foo', 'another');
        $result = $this->smarty->fetch('string:{block name=test}-- {$foo} block default --{/block}');
        $this->assertEquals('-- another block default --', $result);
    }

    /**
     * test just call of  parent template, no blocks predefined
     */
    public function testCompileBlockParent_001()
    {
        $this->smarty->assign('parent', 'parent');
        $result = $this->smarty->fetch('001_parent.tpl');
        $this->assertContains('(parent|b)content parent b(parent|/b)', $result);
    }

    /**
     * test  child/parent template chain
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChild_002($caching, $merge, $testNumber, $compileTestNumber, $renderTestNumber,
                                              $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('002_child.tpl');
        $this->assertContains('(child|b)content child b(child|/b)', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with prepend
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildPrepend_003($caching, $merge, $testNumber, $compileTestNumber,
                                                     $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('003_child.tpl');
        $this->assertContains('(child|b)content child b(child|/b)(parent|b)content parent b(parent|/b)', $result,
                              $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with prepend
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockGrandPrepend_003($caching, $merge, $testNumber, $compileTestNumber,
                                                     $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('003_grand.tpl');
        $this->assertContains('(grand|b)content grand b(grand|/b)(child|b)content child b(child|/b)(parent|b)content parent b(parent|/b)',
                              $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with apppend
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildAppend_004($caching, $merge, $testNumber, $compileTestNumber,
                                                    $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('004_child.tpl');
        $this->assertContains('(parent|b)content parent b(parent|/b)(child|b)content child b(child|/b)', $result,
                              $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/parent template chain with apppend
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockGrandAppend_004($caching, $merge, $testNumber, $compileTestNumber,
                                                    $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('004_grand.tpl');
        $this->assertContains('(parent|b)content parent b(parent|/b)(child|b)content child b(child|/b)(grand|b)content grand b(grand|/b)',
                              $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/parent template chain with apppend
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockGrandAppendPrepend_004($caching, $merge, $testNumber, $compileTestNumber,
                                                           $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('004_grand2.tpl');
        $this->assertContains('(parent|b)content parent b(parent|/b)(grand|b)content grand b(grand|/b)(child|b)content child b(child|/b)(grand|b)content grand b(grand|/b)',
                              $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with apppend and shorttags
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildAppendShortag_005($caching, $merge, $testNumber, $compileTestNumber,
                                                           $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('005_child.tpl');
        $this->assertContains('(parent|b)content parent b(parent|/b)(child|b)content child b(child|/b)', $result,
                              $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with {$this->smarty.block.child)
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildSmartyChild_006($caching, $merge, $testNumber, $compileTestNumber,
                                                         $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('006_child.tpl');
        $this->assertContains('(parent|b)content (child|b)content child b(child|/b) b(parent|/b)', $result,
                              $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with {$this->smarty.block.parent)
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildSmartyParent_007($caching, $merge, $testNumber, $compileTestNumber,
                                                          $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('007_child.tpl');
        $this->assertContains('(child|b)content (parent|b)content parent b(parent|/b) b(child|/b)', $result,
                              $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent template chain with {$this->smarty.block.parent)
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildSmartyParent_007_2($caching, $merge, $testNumber, $compileTestNumber,
                                                            $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('007_grand.tpl');
        $this->assertContains('(grand|b)content (child|b)content (parent|b)content parent b(parent|/b) b(child|/b) b(grand|/b)',
                              $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain loading plugin
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildPlugin_008($caching, $merge, $testNumber, $compileTestNumber,
                                                    $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('008_child.tpl');
        $this->assertContains('(child|b)content escaped &lt;text&gt; child 1 b(child|/b', $result,
                              $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test parent template with nested blocks
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockParentNested_009($caching, $merge, $testNumber, $compileTestNumber,
                                                     $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('009_parent.tpl');
        $this->assertContains('(parent|b)content (parent|c)content parent c(parent|/c) b(parent|/b)', $result,
                              $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with nested block
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildNested_010($caching, $merge, $testNumber, $compileTestNumber,
                                                    $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('010_child.tpl');
        $this->assertContains('(parent|b)content (parent|c)content child c(parent|/c) b(parent|/b)', $result,
                              $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain with nested block and include
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildNestedInclude_011($caching, $merge, $testNumber, $compileTestNumber,
                                                           $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('011_grand.tpl');
        $this->assertContains('(child|b)content(child|c)content(child|n)content(include)content child i(/include)n(child|/n)c(child|/c)(grand|c)content grand c(grand|\c)b(child|/b)',
                              $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  template chain with nested block level test
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildNestedInclude_012($caching, $merge, $testNumber, $compileTestNumber,
                                                           $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('012_grandgrand.tpl');
        $this->assertContains('(grand|b)content (grandgrand|c)content c(grandgrand|\c)(grand|c)content c(grand|\c) b(grand|/b)',
                              $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  nested child block with hide
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildNestedHide_018($caching, $merge, $testNumber, $compileTestNumber,
                                                        $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('018_child.tpl');
        $this->assertContains('(child|b)content(child|c)content child c(child|/c)b(child|/b)', $result,
                              $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  nested grand/child block with hide
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildNestedHide_018_2($caching, $merge, $testNumber, $compileTestNumber,
                                                          $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('018_grand.tpl');
        $this->assertContains('(child|b)content(child|c)content child c(child|/c)(child|d)content (grand|d)content grand d(grand|/d) d(child|/d)b(child|/b)',
                              $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  nested grandgrand/grand/child block with hide
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockChildNestedHide_018_3($caching, $merge, $testNumber, $compileTestNumber,
                                                          $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('018_grandgrand.tpl');
        $this->assertContains('(child|b)content(grand|c)content (grandgrand|c)content grandgrand c(grandgrand|/c) c(grand|/c)(child|d)content (grandgrand|d)content grandgrand d(grandgrand|/d) d(child|/d)b(child|/b)',
                              $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain starting in subtempates
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockStartSubTemplates_020($caching, $merge, $testNumber, $compileTestNumber,
                                                          $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('020_include_root.tpl');
        $this->assertContains('(include1)(include1|p)content 1 p(include1|\p)(include1|b)content 1 b(include1|\b)(\include1)',
                              $result, $testName . ' - include 1');
        $this->assertContains('(include2)(include2|p)content 2 p(include2|\p)(include2|b)content 2 b(include2|\b)(\include2)',
                              $result, $testName . ' - include 1');
        $this->assertContains('(include3)(include3|p)content 3 p(include3|\p)(include3|b)content 3 b(include3|\b)(\include3)',
                              $result, $testName . ' - include 1');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent dependency test1
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testCompileBlockGrandChildMustCompile_021_1()
    {
        $this->smarty->assign('parent', 'parent');
        $this->smarty->assign('child', 'child', true);
        $this->smarty->assign('grand', 'grand', true);
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grand.tpl', $this->smarty);
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('(grand|b)content grand b(grand|/b)(child|b)content child b(child|/b)(parent|b)content parent b(parent|/b)',
                              $result);
        $this->smarty->_clearTemplateCache();
        $this->smarty->assign('parent', 'parent2');
        $this->smarty->assign('child', 'child2', true);
        $this->smarty->assign('grand', 'grand2', true);
        $tpl2 = $this->smarty->createTemplate('021_grand.tpl', $this->smarty);
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('(grand|b)content grand2 b(grand|/b)(child|b)content child2 b(child|/b)(parent|b)content parent b(parent|/b)',
                              $result);
    }

    /**
     * test  grandchild/child/parent dependency test1
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testCompileBlockGrandChildMustCompile_021_12()
    {
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $this->smarty->assign('parent', 'parent3');
        $this->smarty->assign('child', 'child3', true);
        $this->smarty->assign('grand', 'grand3', true);
        $tpl2 = $this->smarty->createTemplate('021_grand.tpl', $this->smarty);
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('(grand|b)content grand3 b(grand|/b)(child|b)content child3 b(child|/b)(parent|b)content parent b(parent|/b)',
                              $result);
    }

    /**
     * test  grandchild/child/parent dependency test2
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_2()
    {
        $this->smarty->assign('parent', 'parent4');
        $this->smarty->assign('child', 'child4', true);
        $this->smarty->assign('grand', 'grand4', true);
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_grand.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grand.tpl', $this->smarty);
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('(grand|b)content grand4 b(grand|/b)(child|b)content child4 b(child|/b)(parent|b)content parent4 b(parent|/b)',
                              $result);
        $this->smarty->_clearTemplateCache();
        $this->smarty->assign('parent', 'parent5');
        $this->smarty->assign('child', 'child5', true);
        $this->smarty->assign('grand', 'grand5', true);
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        $tpl2 = $this->smarty->createTemplate('021_grand.tpl', $this->smarty);
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('(grand|b)content grand5 b(grand|/b)(child|b)content child5 b(child|/b)(parent|b)content parent4 b(parent|/b)',
                              $result);
    }

    /**
     * test  grandchild/child/parent dependency test3
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_3()
    {
        $this->smarty->assign('parent', 'parent6');
        $this->smarty->assign('child', 'child6', true);
        $this->smarty->assign('grand', 'grand6', true);
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_child.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grand.tpl', $this->smarty);
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('(grand|b)content grand6 b(grand|/b)(child|b)content child6 b(child|/b)(parent|b)content parent6 b(parent|/b)',
                              $result);
    }

    /**
     * test  grandchild/child/parent dependency test3
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     *
     */
    public function testCompileBlockGrandChildMustCompile_021_32()
    {
        $this->smarty->assign('parent', 'parent7');
        $this->smarty->assign('child', 'child7', true);
        $this->smarty->assign('grand', 'grand7', true);
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl2 = $this->smarty->createTemplate('021_grand.tpl', $this->smarty);
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('(grand|b)content grand7 b(grand|/b)(child|b)content child7 b(child|/b)(parent|b)content parent6 b(parent|/b)',
                              $result);
    }

    /**
     * test  grandchild/child/parent dependency test4
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testCompileBlockGrandChildMustCompile_021_4()
    {
        $this->smarty->assign('parent', 'parent8');
        $this->smarty->assign('child', 'child8', true);
        $this->smarty->assign('grand', 'grand8', true);
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        sleep(2);
        touch($this->smarty->getTemplateDir(0) . '021_parent.tpl');
        clearstatcache();
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl = $this->smarty->createTemplate('021_grand.tpl', $this->smarty);
        $this->assertFalse($tpl->isCached());
        $result = $this->smarty->fetch($tpl);
        $this->assertContains('(grand|b)content grand8 b(grand|/b)(child|b)content child8 b(child|/b)(parent|b)content parent8 b(parent|/b)',
                              $result);
    }

    /**
     * test  grandchild/child/parent dependency test4
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testCompileBlockGrandChildMustCompile_021_42()
    {
        $this->smarty->assign('parent', 'parent9');
        $this->smarty->assign('child', 'child9', true);
        $this->smarty->assign('grand', 'grand9', true);
        $this->smarty->setCompileDir('./templates_c/mustcompile');
        $this->smarty->caching = true;
        $this->smarty->cache_lifetime = 1000;
        $tpl2 = $this->smarty->createTemplate('021_grand.tpl', $this->smarty);
        $this->assertTrue($tpl2->isCached());
        $result = $this->smarty->fetch($tpl2);
        $this->assertContains('(grand|b)content grand9 b(grand|/b)(child|b)content child9 b(child|/b)(parent|b)content parent8 b(parent|/b)',
                              $result);
    }

    /**
     * test dirt in child templates
     */
    public function testDirt_022()
    {
        $result = $this->smarty->fetch('022_child.tpl');
        $this->assertEquals('Page Title', $result);
    }

    /**
     * test {$this->smarty.block.child} for not existing child {block]
     */
    public function testNotExistingChildBlock_024()
    {
        $result = $this->smarty->fetch('024_parent.tpl');
        $this->assertContains('no >< child', $result);
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage tag {$smarty.block.child} used outside {block} tags
     * test {$this->smarty.block.child} outside {block]
     */
    public function testSmartyBlockChildOutsideBlock_025()
    {
        $this->smarty->fetch('025_parent.tpl');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage tag {$smarty.block.parent} used outside {block} tags
     * test {$this->smarty.block.parent} outside {block]
     */
    public function testSmartyBlockParentOutsideBlock_026()
    {
        $this->smarty->fetch('026_child.tpl');
    }

    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage illegal {$smarty.block.parent}
     * test {$this->smarty.block.parent} in parent template
     */
    public function testSmartyBlockParentInParent_027()
    {
        $result = $this->smarty->fetch('027_parent.tpl');
    }

    /**
     * test  child/parent template chain
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testSmartyBlockVariablePartentInclude_28($caching, $merge, $testNumber, $compileTestNumber,
                                                             $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        $this->smarty->setCompileId(10);
        if ($merge) {
            $this->smarty->setCompileId(11);
        }
        $this->smarty->assign('foo', '028_parent_include1.tpl');
        $result = $this->smarty->fetch('028_child.tpl');
        $this->assertContains('b1-include-1--b1', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  child/parent template chain
     *
     * @dataProvider data
     */
    public function testSmartyBlockVariablePartentInclude_282($caching, $merge, $testNumber, $compileTestNumber,
                                                              $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $this->smarty->assign('foo', '028_parent_include2.tpl');
        $result = $this->smarty->fetch('028_child.tpl');
        $this->assertContains('b1-child-i2-include-2--b1', $result, $testName . ' - content');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    /**
     * test  grandchild/child/parent template chain with nested {$this->smarty.block.child} and {include nocache}
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockGrandChildNested_030($caching, $merge, $testNumber, $compileTestNumber,
                                                         $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('030_grandchild_nested.tpl');
        $this->assertContains('child pre -grandchild content- child post', $result, $testName . ' - content');
        $this->assertContains('include:' . $testNumber, $result, $testName . ' - content 2');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }
    /**
     * test  grandchild/child/parent template chain with nested {$this->smarty.block.child} and {include nocache}
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockGrandChildNestedRelative_030($caching, $merge, $testNumber, $compileTestNumber,
                                                         $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('sub/030_grandchild_nested_rel.tpl');
        $this->assertContains('child pre -grandchild content- child post', $result, $testName . ' - content');
        $this->assertContains('include:' . $testNumber, $result, $testName . ' - content 2');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }

    public function data()
    {
        return array(/*
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
                     array(true, true, 8, 7, 7, 'caching, merge - exits'),);
    }

    /*
     * Test post filter on {block}
     */
    public function testPostFilter_031()
    {
        function smarty_postfilter_test031($compiled, Smarty_Internal_Template $template)
        {
            return str_replace("'foo'", "'bar'", $compiled);
        }

        $this->smarty->loadFilter('post', 'test031');
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertContains('bar', $this->smarty->fetch('031_post_filter.tpl'));
    }
}
