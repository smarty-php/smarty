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
     * @group slow
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
     * @group slow
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
     * @group slow
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
     * @group slow
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
     * @group slow
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
     * @expectedExceptionMessage '{$smarty.block.child}' used outside {block} tags
     * test {$this->smarty.block.child} outside {block]
     */
    public function testSmartyBlockChildOutsideBlock_025()
    {
        $this->smarty->fetch('025_parent.tpl');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage '{$smarty.block.parent}' used outside {block} tags
     * test {$this->smarty.block.parent} outside {block]
     */
    public function testSmartyBlockParentOutsideBlock_026()
    {
        $this->smarty->fetch('026_child.tpl');
    }

    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage illegal '{$smarty.block.parent}'
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
     /**
     * test  grandchild/child/parent template chain with nested {$this->smarty.block.child} and {include nocache}
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        data
     */
    public function testCompileBlockSmartyBlockParent_034_1($caching, $merge, $testNumber, $compileTestNumber,
                                                          $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('034_1child.tpl');
        $this->assertContains('parent b1', $result, $testName . ' - content');
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
    public function testCompileBlockSmartyBlockParent_034_2($caching, $merge, $testNumber, $compileTestNumber,
                                                          $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('034_2child.tpl');
        $this->assertContains('parent b1', $result, $testName . ' - content');
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
    public function testCompileBlockSmartyBlockParent_034_3($caching, $merge, $testNumber, $compileTestNumber,
                                                            $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('034_3child.tpl');
        $this->assertContains('parent b1', $result, $testName . ' - content');
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
    public function testCompileBlockSmartyBlockParent_034_4($caching, $merge, $testNumber, $compileTestNumber,
                                                            $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('034_4child.tpl');
        $this->assertContains('parent b1', $result, $testName . ' - content');
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
    public function testCompileBlockSmartyBlockChild_035_1($caching, $merge, $testNumber, $compileTestNumber,
                                                         $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parenttpl', '035_1parent.tpl');
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('035_child.tpl');
        $this->assertContains('(parent|b)content (child|b)content child b(child|/b) b(parent|/b)', $result,
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
    public function testCompileBlockSmartyBlockChild_035_2($caching, $merge, $testNumber, $compileTestNumber,
                                                           $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parenttpl', '035_2parent.tpl');
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('035_child.tpl');
        $this->assertContains('(parent|b)content (child|b)content child b(child|/b) b(parent|/b)', $result,
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
    public function testCompileBlockSmartyBlockChild_035_3($caching, $merge, $testNumber, $compileTestNumber,
                                                           $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parenttpl', '035_3parent.tpl');
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('035_child.tpl');
        $this->assertContains('(parent|b)content (child|b)content child b(child|/b) b(parent|/b)', $result,
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
    public function testCompileBlockSmartyBlockChild_035_4($caching, $merge, $testNumber, $compileTestNumber,
                                                           $renderTestNumber, $testName)
    {
        $this->smarty->registerFilter('pre', array($this, 'compiledPrefilter'));
        $this->smarty->assign('test', $testNumber);
        $this->smarty->assign('parenttpl', '035_4parent.tpl');
        $this->smarty->assign('parent', 'parent', true);
        $this->smarty->assign('child', 'child', true);
        $this->smarty->setCaching($caching);
        $this->smarty->setMergeCompiledIncludes($merge);
        if ($merge) {
            $this->smarty->setCompileId(1);
        }
        $result = $this->smarty->fetch('035_child.tpl');
        $this->assertContains('(parent|b)content (child|b)content child b(child|/b) b(parent|/b)', $result,
                              $testName . ' - content');
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
    /*
      * Test new inheritance root in outermost include
      */
    public function testInclude_032()
    {
        $result = $this->smarty->fetch('032_child.tpl');
        $this->assertContains('foo in 032_child.tpl', $result);
        $this->assertContains('bar in 032_included_child.tpl', $result);
        $this->assertContains('foo in 032_included_parent.tpl', $result);
        $this->assertNotContains('bar in 032_included_parent.tpl', $result);
        $this->assertNotContains('foo in 032_parent.tpl', $result);
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
        $result = $this->smarty->fetch('033_grandchild.tpl');
        $this->assertContains('include grand:content include grand', $result, $testName . ' - grand');
        $this->assertContains('include child:content include child', $result, $testName . ' - grand');
        $this->assertContains('include parent:content include parent', $result, $testName . ' - grand');
        $this->assertContains("test:{$testNumber} compiled:{$compileTestNumber} rendered:{$renderTestNumber}", $result,
                              $testName . ' - fetch() failure');
    }
    /**
     *
     * test smarty.block.foo
     *
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage $smarty.block is not defined
     */
    public function testSmartyBlockWrongBlockName_036()
    {
        $this->smarty->fetch('036_parent.tpl');
    }
    /**
     *
     * test '{$smarty.block.parent}'
     *
     * @expectedException        SmartyException
     * @expectedExceptionMessage inheritance: illegal '{$smarty.block.parent}' used in child template
     */
    public function testSmartyParentBlockCalledInParent_036_1()
    {
        $this->smarty->fetch('036_1_parent.tpl');
    }
    /**
     *
     * test {block_parent}
     *
     * @expectedException        SmartyException
     * @expectedExceptionMessage inheritance: illegal '{block_parent}' used in child template
     */
    public function testSmartyParentBlockCalledInParent_036_2()
    {
        $this->smarty->fetch('036_2_parent.tpl');
    }
    /**
     *
     * test {block_parent}
     *
     * @expectedException        SmartyException
     * @expectedExceptionMessage inheritance: illegal '{parent}' used in child template
     */
    public function testSmartyParentBlockCalledInParent_036_3()
    {
        $this->smarty->fetch('036_3_parent.tpl');
    }

    /**
     *
     * test smarty.block
     *
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage $smarty.block is not defined
     */
    public function testSmartyBlockMissigBlockName_037()
    {
        $this->smarty->fetch('037_parent.tpl');
    }

    /**
     * Test spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testSpacing - {$file}");
    }
    /**
     * Test Output nocache spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testBlockSpacingNocache($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->smarty->setCompileId('VarNocache');
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar',true);
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testVarNocache - {$file}");
    }
    /**
     * Test Output nocache spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestSpacing
     * @runInSeparateProcess
     */
    public function testBlockSpacingNocache2($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->smarty->setCompileId('VarNocache');
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'foo',true);
        $this->assertEquals(str_replace('bar','foo',$result),
                            $this->smarty->fetch($file),
                            "testVarNocache1 - {$file}");
    }

    /*
      * Data provider für testSpacing
      */
    public function dataTestSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array("A{block name='a'}{\$foo}{/block}C", "AbarC", 'Var0', $i++),
                     array("A{block name='a'}\n{\$foo}{/block}C", "A\nbarC", 'Var1', $i++),
                     array("A{block name='a'}\n{\$foo}\n{/block}C", "A\nbar\nC", 'Var2', $i++),
                     array("A{block name='a'}\n{\$foo}{/block}\nC", "A\nbar\nC", 'Var3', $i++),
                     array("A\n{block name='a'}\n{\$foo}\n{/block}\nC", "A\n\nbar\n\nC", 'Var4', $i++),
        );
    }
    /**
     * Test spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestChildSpacing
     * @runInSeparateProcess
     */
    public function testChildSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_Parent{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $child = "{extends file='$file'}\n";
        $child .= preg_replace(array('/A/','/C/','/[$]foo/','/\s*[{][$]smarty[.]block[.]child[}]\s*/'),array('G','H','$bar','{$bar}'),$code);
        $file = "Spacing_Child{$name}.tpl";
        $this->makeTemplateFile($file, $child);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'foo');
        $this->smarty->assign('bar', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "testChildSpacing - {$file}");
    }

    /*
      * Data provider für testSpacing
      */
    public function dataTestChildSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array("A{block name='a'}{\$foo}{/block}C", "AbarC", 'Var0', $i++),
                     array("A{block name='a'}\n{\$foo}{/block}C", "A\nbarC", 'Var1', $i++),
                     array("A{block name='a'}\n{\$foo}\n{/block}C", "A\nbar\nC", 'Var2', $i++),
                     array("A{block name='a'}\n{\$foo}{/block}\nC", "A\nbar\nC", 'Var3', $i++),
                     array("A\n{block name='a'}\n{\$foo}\n{/block}\nC", "A\n\nbar\n\nC", 'Var4', $i++),
                     array("A{block name='a'}{\$smarty.block.child}{/block}C", "AbarC", 'BlockChild0', $i++),
                     array("A{block name='a'}\n{\$smarty.block.child}{/block}C", "A\nbarC", 'BlockChild1', $i++),
                     array("A{block name='a'}\n{\$smarty.block.child}\n{/block}C", "A\nbar\nC", 'BlockChild2', $i++),
                     array("A{block name='a'}\n{\$smarty.block.child}{/block}\nC", "A\nbar\nC", 'BlockChild3', $i++),
                     array("A\n{block name='a'}\n{\$smarty.block.child}\n{/block}\nC", "A\n\nbar\n\nC", 'BlockChild4', $i++),
        );
    }

    /**
     * Test Block nocache spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestBlockNocache
     * @runInSeparateProcess
     */
    public function testBlockNocacheSpacing($code, $result, $name, $testNumber)
    {
        $file = "blockNocache_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setCompileId('BlockNocache');
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            "blockNocache - {$file}");
    }
    /**
     * Test Block nocache spacings
     *
     * @preserveGlobalState disabled
     * @dataProvider        dataTestBlockNocache
     * @runInSeparateProcess
     */
    public function testBlockNocacheSpacing2($code, $result, $name, $testNumber)
    {
        $file = "blockNocache_{$name}.tpl";
        $this->smarty->setCompileId('BlockNocache');
        $this->smarty->setCaching(1);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('foo', 'foo');
        $this->assertEquals(str_replace('bar','foo',$result),
                            $this->smarty->fetch($file),
                            "blockNocache - {$file}");
    }
    /*
      * Data provider für TestBlockNocache
      */
    public function dataTestBlockNocache()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(array("A{nocache}{block name='a'}{\$foo}{/block}{/nocache}C", "AbarC", 'Var0', $i++),
                     array("A{nocache}{block name='a'}\n{\$foo}{/block}{/nocache}C", "A\nbarC", 'Var1', $i++),
                     array("A{nocache}{block name='a'}\n{\$foo}\n{/block}{/nocache}C", "A\nbar\nC", 'Var2', $i++),
                     array("A{nocache}{block name='a'}\n{\$foo}{/block}\n{/nocache}C", "A\nbar\nC", 'Var3', $i++),
                     array("A{nocache}\n{block name='a'}\n{\$foo}\n{/block}\n{/nocache}C", "A\n\nbar\n\nC", 'Var4', $i++),
        );
    }

}
